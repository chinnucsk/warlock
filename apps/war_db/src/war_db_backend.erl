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
-module(war_db_backend).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{start, 0},                % Start with default options
     {start, 1},                % Options
     {ping, 1},                 % Client
     {reset, 1},                % Client
     {backup, 2},               % File, Client
     {restore, 2},              % File, Client
     {x, 2}];                   % {Cmd, Client}
behaviour_info(_Other) ->
    undefined.