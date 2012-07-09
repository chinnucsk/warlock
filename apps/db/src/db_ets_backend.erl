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
-module(db_ets_backend).
-behavior(db_backend).

-include("db.hrl").

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
    Name = conf_helper:get(name, ?MODULE),
    Options = conf_helper:get(options, ?MODULE),
    db_ets_timer:start_link(),
    {ok, #client{inst=ets:new(Name, Options)}}.

-spec start(list()) -> {ok, #client{}}.
start([Name | Options]) ->
    db_ets_timer:start_link(),
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
x([get, Key], #client{inst=Table}) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
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
x([setex, Time, Key, Value], Client) ->
    Result = x([set, Key, Value], Client),
    db_ets_timer:expire_after(Time, Key),
    Result;
% Set expiry for object with given key
x([expire, Time, Key], _Client) ->
    db_ets_timer:expire_after(Time, Key);
% Store object if not set. Time in milli seconds
% Extend expire if already set,  "Value" should be equal to the one in the db
x([setenx, Time, Key, Value], Client) ->
    case x([get, Key], Client) of
        {ok, not_found} ->
            Result = x([set, Key, Value], Client),
            db_ets_timer:expire_after(Time, Key),
            Result;
        {ok, Value} ->
            db_ets_timer:expire_after(Time, Key),
            {ok, success};
        {ok, _Val} ->
            {ok, not_set}
    end;
% Delete object with given Key
x([del, Key], #client{inst=Table}) ->
    true = ets:delete(Table, Key),
    {ok, success};
x(_, _) ->
    {error, unknown_command}.
