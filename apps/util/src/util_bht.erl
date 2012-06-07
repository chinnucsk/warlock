%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc A bi-diretional hash table using ETS
%%%
%%% A simple implementation of a bi-directional hash table using one ETS table.
%%% We create two entries for one key-value pair.
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
-export([new/0, del/1, set/3, keyget/2, valget/2, del/3, to_list/1]).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
new() ->
    ets:new(bht, []).

del(Table) ->
    ets:delete(Table).

set(Key, Val, Table) ->
    ets:insert(Table, [{getkey_key(Key), Val},
                       {getkey_val(Val), Key}]),
    ok.

keyget(Key, Table) ->
    get(getkey_key(Key), Table).

valget(Val, Table) ->
    get(getkey_val(Val), Table).

del(Key, Val, Table) ->
    % ets does not have atomic delete object list
    ets:delete(Table, getkey_key(Key)),
    ets:delete(Table, getkey_val(Val)),
    ok.

to_list(Table) ->
    ets:tab2list(Table).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
getkey_key(K) ->
    {k, K}.

getkey_val(V) ->
    {v, V}.

get(Key, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            not_found;
        [{Key, Value}] ->
            Value;
        _ ->
            error
    end.
