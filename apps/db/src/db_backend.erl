%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB Backend
%%%
%%% Behaviour for defining different types of backend
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(db_backend).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start, 0},                % Start with default options
     {start, 1},                % Options
     {ping, 1},                 % Client
     {reset, 1},                % Client
     {backup, 2},               % Client
     {get, 2},                  % {Key, Client}
     {set, 3},                  % {Key, Value, Client}
     {del, 2}];                 % {Key, Client}
behaviour_info(_Other) ->
    undefined.