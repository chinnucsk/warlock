%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc A bi-diretional hash table behaviour
%%% @end
%%%-------------------------------------------------------------------
-module(util_bht).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{new, 0},
     {new, 1},
     {del, 1},
     {reset, 1},
     {to_list, 1},
     {keyget, 2},
     {valget, 2},
     {set, 3},
     {del, 3}];
behaviour_info(_Other) ->
    undefined.
