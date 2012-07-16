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
%%%
%%% Note: The key and value has to be unique in key space and value space resp.
%%%
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(ets_bht).

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
    ets:new(bht, []).

-spec new(list()) -> Table :: table().
new(Options) ->
    ets:new(hd(Options), tl(Options)).

-spec del(Table::table()) -> true.
del(Table) ->
    ets:delete(Table).

-spec set(Key::term(), Val::term(), Table::table()) -> ok.
set(Key, Val, Table) ->
    ets:insert(Table, [{getkey_key(Key), Val},
                       {getkey_val(Val), Key}]),
    Table.

-spec keyget(Key::term(), Table::table()) -> Val::term().
keyget(Key, Table) ->
    get(getkey_key(Key), Table).

-spec valget(Val::term(), Table::table()) -> Key::term().
valget(Val, Table) ->
    get(getkey_val(Val), Table).

-spec del(Key::term(), Val::term(), Table::table()) -> ok.
del(Key, Val, Table) ->
    % ets does not have atomic delete object list
    ets:delete(Table, getkey_key(Key)),
    ets:delete(Table, getkey_val(Val)),
    Table.

-spec to_list(Table::table()) -> [{term(), term()}].
to_list(Table) ->
    get_keyval(ets:tab2list(Table)).

% TODO: Check if dropping the table and creating a new table it is faster
-spec reset(Table::table()) -> true.
reset(Table) ->
    ets:delete_all_objects(Table),
    Table.

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
