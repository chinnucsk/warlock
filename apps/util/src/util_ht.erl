%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc A simple hash table behaviour
%%% @end
%%%-------------------------------------------------------------------
-module(util_ht).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{new, 0},              % Create new HT
     {new, 1},              % Create new HT with options
     {del, 1},              % Delete entire HT
     {reset, 1},            % Delete all the content of HT
     {to_list, 1},          % Return all data of HT as list
     {get, 2},              % Get value of specific key
     {set, 3},              % Set {Key, Val}=Obj in HT
     {del, 2}];             % Del Obj with Key
behaviour_info(_Other) ->
    undefined.
