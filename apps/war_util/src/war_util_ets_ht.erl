%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc A simple hash table using ETS
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_util_ets_ht).
-behaviour(war_util_ht).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([new/0, new/1, del/1, reset/1, to_list/1,
         get/2, set/3, del/2]).

-type table() :: term().

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec new() -> Table :: table().
new() ->
    ets:new(ht, []).

-spec new(list()) -> Table :: table().
new(Options) ->
    ets:new(hd(Options), tl(Options)).

-spec del(Table::table()) -> true.
del(Table) ->
    ets:delete(Table).

-spec set(Key::term(), Val::term(), Table::table()) -> ok.
set(Key, Value, Table) ->
    ets:insert(Table, {Key, Value}),
    Table.

-spec get(Key::term(), Table::table()) -> Val::term() | error.
get(Key, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            not_found;
        [{Key, Value}] ->
            Value
    end.

-spec del(Key::term(), Table::table()) -> ok.
del(Key, Table) ->
    ets:delete(Table, Key),
    Table.

-spec to_list(Table::table()) -> [{term(), term()}].
to_list(Table) ->
    ets:tab2list(Table).

% TODO: Check if dropping the table and creating a new table it is faster
-spec reset(Table::table()) -> true.
reset(Table) ->
    ets:delete_all_objects(Table),
    Table.