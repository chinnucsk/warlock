%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB Redis backend
%%%
%%% Backend implementation for Redis
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_db_judy_backend).
-behavior(war_db_backend).

-include("war_db.hrl").

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
    C =  judy:new(),
    {ok, #client{inst=C}}.

-spec start(term()) -> {ok, #client{}}.
start(_Options) ->
    C =  judy:new(),
    {ok, #client{inst=C}}.

-spec reset(Client::#client{}) -> {ok, #client{}}.
reset(_Client) ->
    ok.

%% FIXME: To be implemented
-spec backup(File::string(), Client::#client{}) -> ok | {error, _}.
backup(_File, #client{inst=_C}) ->
    ok.

%% FIXME: To be implemented
-spec restore(File::string(), Client::#client{}) -> {ok, success}.
restore(_File, #client{inst=_C}) ->
    ok.

-spec ping(Client::#client{}) -> ping | pang.
ping(_Client) ->
    pong.

-spec x(Cmd::term(), Client::#client{}) -> term().
x([get, Key], #client{inst=C}) ->
    case judy:get(Key, C) of
        {error, Key} ->
            {ok, not_found};
        Value ->
            {ok, Value}
    end;
x([set, Key, Value], #client{inst=C}) ->
    judy:insert(Key, Value, C),
    {ok, success};
x([del, Key], #client{inst=C}) ->
    judy:remove(Key, C),
    {ok, success}.
