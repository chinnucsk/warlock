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
x([get, Key], #client{inst=Table}) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, Value}] ->
            {ok, Value};
        [_H | _T] ->
            {error, multiple_values}
    end;
x([set, Key, Value], #client{inst=Table}) ->
    true = ets:insert(Table, {Key, Value}),
    {ok, success};
x([setnx, Key, Value], Client) ->
    case x([get, Key], Client) of
        {ok, not_found} ->
            x([set, Key, Value], Client);
        {ok, _Value} ->
            {ok, not_set}
    end;
x([del, Key], #client{inst=Table}) ->
    true = ets:delete(Table, Key),
    {ok, success};
x(_, _) ->
    {error, unknown_command}.
