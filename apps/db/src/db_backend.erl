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
%%% @since : 30 May 2012 by Wooga GmbH
%%% @end
%%%-------------------------------------------------------------------
-module(db_backend).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start, 0},                % Start with default options
     {start, 1},                % Options
     {ping, 1},                 % Client
     {get, 2},                  % {Key, Client}
     {put, 3},                  % {Key, Value, Client}
     {delete, 2}];              % {Key, Client}       
behaviour_info(_Other) ->
    undefined.