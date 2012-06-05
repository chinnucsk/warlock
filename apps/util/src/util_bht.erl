%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc A bi-diretional hash table using ETS
%%%
%%% A simple implementation of a bi-directional hash table using one ETS table.
%%% We create two entries for one key, value pair.
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Add specs
-module(util_bht).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([new/0, set/3, keyget/2, valget/2, del/3, to_list/1]).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
new() ->
    ets:new(bht, []).

set(K, V, Table) ->
    Key = {k, K},
    Val = {v, V},
    ets:insert(Table, {Key, V}),
    ets:insert(Table, {Val, K}),
    ok.

keyget(K, Table) ->
    Key = {key, K},
    get(Key, Table).

valget(V, Table) ->
    Val = {v, V},
    get(Val, Table).

del(K, V, Table) ->
    Key = {k, K},
    Val = {v, V},
    ets:delete(Table, Key),
    ets:delete(Table, Val),
    ok.

to_list(Table) ->
    ets:tab2list(Table).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get(Key, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            {ok, not_found};
        [{Key, Value}] ->
            {ok, Value};
        _ ->
            {error, unknown_object}
    end.
