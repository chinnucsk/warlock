%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc A simple hash table using Dict
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_util_dict_ht).
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
    dict:new().

-spec new(list()) -> Table :: table().
new(_) ->
    dict:new().

-spec del(Table::table()) -> true.
del(_Table) ->
    ok.

-spec set(Key::term(), Val::term(), Table::table()) -> ok.
set(Key, Value, Table) ->
    dict:store(Key, Value, Table).

-spec get(Key::term(), Table::table()) -> Val::term() | error.
get(Key, Table) ->
    case catch dict:fetch(Key, Table) of
        {'EXIT', _} ->
            not_found;
        Value ->
            Value
    end.

-spec del(Key::term(), Table::table()) -> ok.
del(Key, Table) ->
    dict:erase(Key, Table).

-spec to_list(Table::table()) -> [{term(), term()}].
to_list(Table) ->
    dict:to_list(Table).

-spec reset(Table::table()) -> true.
reset(_Table) ->
    new().