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
-module(dict_bht).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([new/0, new/1, del/1, to_list/1, reset/1,
         set/3, keyget/2, valget/2, del/3]).

-type tid() :: integer().
-type table() :: atom() | tid().

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec new() -> Table :: table().
new() ->
    dict:new().

-spec new(list()) -> Table :: table().
new(_Options) ->
    dict:new().

-spec del(Table::table()) -> true.
del(_Table) ->
    ok.

-spec set(Key::term(), Val::term(), Table::table()) -> ok.
set(Key, Val, Table) ->
    Table1 = dict:store(getkey_key(Key), Val, Table),
    dict:store(getkey_val(Val), Key, Table1).

-spec keyget(Key::term(), Table::table()) -> Val::term().
keyget(Key, Table) ->
    get(getkey_key(Key), Table).

-spec valget(Val::term(), Table::table()) -> Key::term().
valget(Val, Table) ->
    get(getkey_val(Val), Table).

-spec del(Key::term(), Val::term(), Table::table()) -> ok.
del(Key, Val, Table) ->
    Table1 = dict:erase(getkey_key(Key), Table),
    dict:erase(getkey_val(Val), Table1).

-spec to_list(Table::table()) -> [{term(), term()}].
to_list(Table) ->
    get_keyval(dict:to_list(Table)).

-spec reset(Table::table()) -> true.
reset(_Table) ->
    dict:new().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
getkey_key(K) ->
    {k, K}.

getkey_val(V) ->
    {v, V}.

get(Key, Table) ->
    case catch dict:fetch(Key, Table) of
        {'EXIT', _} ->
            not_found;
        Value ->
            Value
    end.

get_keyval(List) ->
    get_keyval(List, []).

get_keyval([], Acc) ->
    Acc;
get_keyval([{{k, K}, V}|T], Acc) ->
    get_keyval(T, [{K, V} | Acc]);
get_keyval([{{v, _V}, _K}|T], Acc) ->
    get_keyval(T, Acc).

