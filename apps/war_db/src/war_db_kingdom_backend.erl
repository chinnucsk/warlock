%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB ETS backend
%%%
%%% Backend implementation in ETS tailored for magicland
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
% TODO: Add auto expire for keys with expiry set
-module(war_db_kingdom_backend).
-behavior(war_db_backend).

-include("war_db.hrl").

%% ------------------------------------------------------------------
%% Function Exports and macros
%% ------------------------------------------------------------------
-export([start/0, start/1, ping/1, reset/1, backup/2, restore/2,
         x/2]).
-include_lib("war_util/include/war_common.hrl").

-define(PREFIX, "kingdom").

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

% FIXME: Include node servers as well
% Another possible option is to rebuild node tables from data available
-spec backup(File::string(), Client::#client{}) -> ok | {error, _}.
backup(File, #client{inst=Table}) ->
    ets:tab2file(Table, File).

% FIXME: Include node servers as well
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
    get_value(Table, Key);
x([set, Time, Node, Key, Value], #client{inst=Table}) ->
    % Store key value
    case get(Table, Key) of
        % New entry
        {ok, not_found} ->
            set_new(Table, Node, Time, Key, Value),
            {ok, success};
        % Already set
        {Key, _Value, _Node, _ExpireTime} ->
            {ok, not_set}
    end;
% Delete object with given Key
x([del, Key], #client{inst=Table}) ->
    case get(Table, Key) of
        {ok, not_found} ->
            {ok, success};
        {Key, _Value, Node, _ExpireTime} ->
            delete(Table, Node, Key),
            {ok, success}
    end;
% Set expire if not set, extend expire if already set
x([expire, Time, Key], #client{inst=Table}) ->
    case get(Table, Key) of
        {ok, not_found} ->
            {ok, not_found};
        {Key, Value, Node, _ExpireTime} ->
            set(Table, Node, Time, Key, Value),
            {ok, success}
    end;
x([ttl, Key], #client{inst=Table}) ->
    case get(Table, Key) of
        {ok, not_found} ->
            {ok, not_found};
        {Key, _Value, _Node, ExpireTime} ->
            NowSec = now_to_seconds(erlang:now()),
            {ok, ExpireTime - NowSec}
    end;
x([del_node, Node], Client) ->
    case catch ets:tab2list(Node) of
        [] ->
            {ok, success};
        {'EXIT', {badarg, _}} ->
            {ok, success};
        Data ->
            delete_node(Node, Data, Client),
            {ok, success}
    end;
x(_, _) ->
    {error, unknown_command}.


set(Table, Node, Time, Key, Value) ->
    ExpireTime = expire_sec_to_timestamp(Time),
    true = ets:insert(Table, {Key, Value, Node, ExpireTime}).

set_new(Table, Node, Time, Key, Value) ->
    set(Table, Node, Time, Key, Value),

    % Create reverse lookup
    case catch ets:insert(Node, {Key}) of
        true ->
            ok;
        % New node
        {'EXIT', _} ->
            ets:new(Node, [set, named_table]),
            ets:insert(Node, {Key})
    end.

get_value(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, Value, Node, ExpireTime}] ->
            case now_to_seconds(erlang:now()) =< ExpireTime of
                true ->
                    {ok, Value};
                false ->
                    delete(Table, Node, Key),
                    {ok, not_found}
            end
    end.

get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, Value, Node, ExpireTime}] ->
            case now_to_seconds(erlang:now()) =< ExpireTime of
                true ->
                    {Key, Value, Node, ExpireTime};
                false ->
                    delete(Table, Node, Key),
                    {ok, not_found}
            end
    end.

delete(Table, Node, Key) ->
    true = ets:delete(Table, Key),
    true = ets:delete(Node, Key).

delete_node(Node, Data, Client) ->
    delete_mult(Data, Client),
    ets:delete(Node).

delete_mult([], _Client) ->
    ok;
delete_mult([{Key} | T], Client) ->
    x([del, Key], Client),
    delete_mult(T, Client).

expire_sec_to_timestamp(Time) ->
    now_to_seconds(now_add(erlang:now(), Time * 1000000)).

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
