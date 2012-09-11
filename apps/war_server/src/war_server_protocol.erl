%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Ranch protocol handler
%%%
%%% Ranch calls start_link/4
%%% ListenerPid, Socket, Transport and Opts. ListenerPid
%%%
%%% We start a new server_worker dedicated to that client
%%%
%%% @end
%%%
%%% @since : 06 August 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_server_protocol).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/4]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(pid(), pid(), atom(), list()) -> {error, _} | {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    supervisor:start_child(war_server_pool_sup,
                           [ListenerPid, Socket, Transport, Opts]).
