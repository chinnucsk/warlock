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
%%% @since : 30 May 2012 by Wooga GmbH
%%% @end
%%%-------------------------------------------------------------------
-module(db_ets_backend).
-behavior(db_backend).

-include("db.hrl").

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start/0, start/1, ping/1, get/2, put/3, delete/2]).

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

-spec ping(Client::#client{}) -> ping | pang.
ping(#client{inst=Table}) ->
    case ets:info(Table) of
        % Table not available
        undefined ->
            pang;
        _ ->
            pong
    end.

-spec get(Key::key(), Client::#client{}) ->
          {ok, not_found} |
          {ok, Value::term()} |
          {error, multiple_values} |
          {error, unknown}.
get(Key, #client{inst=Table}) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, Value}] ->
            {ok, Value};
        [_H | _T] ->
            {error, multiple_values};
        _ ->
            {error, unknown}
    end.

-spec put(Key::key(), Value::value(), Client::#client{}) -> {ok, success}.
put(Key, Value, #client{inst=Table}) ->
    true = ets:insert(Table, {Key, Value}),
    {ok, success}.

-spec delete(Key::key(), Client::#client{}) -> {ok, success}.
delete(Key, #client{inst=Table}) ->
    true = ets:delete(Table, Key),
    {ok, success}.
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    