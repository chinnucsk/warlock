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
%% TODO: Add specs
-module(util_ht).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([new/0, new/1, del/1, set/3, get/2, del/2, to_list/1]).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
new() ->
    ets:new(ht, []).

new(Options) ->
    ets:new(ht, [Options]).

del(Table) ->
    ets:delete(Table).

set(Key, Value, Table) ->
    ets:insert(Table, {Key, Value}),
    ok.

get(Key, Table) ->
    case ets:lookup(Table, Key) of
        [] ->
            not_found;
        [{Key, Value}] ->
            Value;
        _ ->
            error
    end.

del(Key, Table) ->
    ets:delete(Table, Key),
    ok.

to_list(Table) ->
    ets:tab2list(Table).