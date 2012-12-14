%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB ETS backend
%%%
%%% Backend implementation in ETS
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
% TODO: Add auto expire for keys with expiry set
-module(war_db_ets_backend).
-behavior(war_db_backend).

-include("war_db.hrl").
-include_lib("war_util/include/war_common.hrl").

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start/0, start/1, ping/1, reset/1, backup/2, restore/2,
         x/2]).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec start() -> {ok, #client{}}.
start() ->
    Name = war_util_conf:get(name, ?MODULE),
    Options = war_util_conf:get(options, ?MODULE),
    {ok, #client{inst=ets:new(Name, Options)}}.

-spec start(list()) -> {ok, #client{}}.
start([Name | Options]) ->
    {ok, #client{inst=ets:new(Name, Options)}}.

-spec reset(Client::#client{}) -> {ok, #client{}}.
reset(#client{inst=Table}) ->
    ets:delete(Table),
    ?MODULE:start().

-spec backup(File::string(), Client::#client{}) -> ok | {error, _}.
backup(File, #client{inst=Table}) ->
    ets:tab2file(Table, File).

-spec restore(File::string(), Client::#client{}) -> {ok, success}.
restore(File, #client{inst=Table}) ->
    ets:delete(Table),
    ets:file2tab(File).

-spec ping(Client::#client{}) -> ping | pang.
ping(#client{inst=Table}) ->
    case ets:info(Table) of
        % Table not available
        undefined ->
            pang;
        _ ->
            pong
    end.

-spec x(Cmd::term(), Client::#client{}) -> term().
% Get object with given key
x([get, Key], #client{inst=Table}=Client) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, Value, ExpireTime}] ->
            case now_to_seconds(os:timestamp()) =< ExpireTime of
                true ->
                    {ok, Value};
                false ->
                    x([del, Key], Client),
                    {ok, not_found}
            end;
        [{Key, Value}] ->
            {ok, Value};
        [_H | _T] ->
            {error, multiple_values}
    end;
% Store object
x([set, Key, Value], #client{inst=Table}) ->
    true = ets:insert(Table, {Key, Value}),
    {ok, success};
% Set object if not already set
x([setnx, Key, Value], Client) ->
    case x([get, Key], Client) of
        {ok, not_found} ->
            x([set, Key, Value], Client);
        {ok, _Value} ->
            {ok, not_set}
    end;
% Store object with expiry.Time in milli seconds
x([setex, Time, Key, Value], #client{inst=Table}) ->
    ExpireTime = expire_sec_to_timestamp(Time),
    true = ets:insert(Table, {Key, Value, ExpireTime}),
    {ok, success};
% Store object if not set. Time in milli seconds
% Extend expire if already set,  "Value" should be equal to the one in the db
x([setenx, Time, Key, Value], Client) ->
    case x([get, Key], Client) of
        {ok, not_found} ->
            x([setex, Time, Key, Value], Client);
        {ok, Value} ->
            x([setex, Time, Key, Value], Client);
        {ok, _Val} ->
            {ok, not_set}
    end;
% Delete object with given Key
x([del, Key], #client{inst=Table}) ->
    true = ets:delete(Table, Key),
    {ok, success};
% Set expire if not set, extend expire if already set
x([expire, Time, Key], Client) ->
    case x([get, Key], Client) of
        {ok, not_found} ->
            {ok, not_found};
        {ok, Value} ->
            x([setex, Time, Key, Value], Client)
    end;
x([ttl, Key], #client{inst=Table}=Client) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, _Value, ExpireTime}] ->
            NowSec = now_to_seconds(os:timestamp()),
            case NowSec =< ExpireTime of
                true ->
                    {ok, ExpireTime - NowSec};
                false ->
                    x([del, Key], Client),
                    {ok, not_found}
            end;
        [{Key, _Value}] ->
            {ok, -1}
    end;
x([<<"get">>, Key], #client{inst=Table}=Client) ->
    case ets:lookup(Table, Key) of
        [] ->
            not_found;
        [{Key, Value, ExpireTime}] ->
            case now_to_seconds(os:timestamp()) =< ExpireTime of
                true ->
                    Value;
                false ->
                    x([del, Key], Client),
                    not_found
            end;
        [{Key, Value}] ->
            Value;
        [_H | _T] ->
            multiple_values
    end;
x([<<"set">>, Key, Value], #client{inst=Table}) ->
    true = ets:insert(Table, {Key, Value}),
    ok;
x(_, _) ->
    {error, unknown_command}.


expire_sec_to_timestamp(Time) ->
    now_to_seconds(now_add(os:timestamp(), Time * 1000000)).

now_add ({ Mega, Sec, Micro }, Add) ->
  proper ({ Mega, Sec, Micro + Add }).

proper (Time = { _, Sec, Micro }) when Sec < 1000000, Micro < 1000000 ->
  Time;
proper ({ Mega, Sec, Micro }) when Sec < 1000000 ->
  proper ({ Mega, Sec + 1, Micro - 1000000 });
proper ({ Mega, Sec, Micro }) ->
  proper ({ Mega + 1, Sec - 1000000, Micro }).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.
