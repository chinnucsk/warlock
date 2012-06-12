%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB app supervisor
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(db_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% Supervisor callbacks
%% ------------------------------------------------------------------
-export([init/1]).

-include_lib("util/include/common.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ------------------------------------------------------------------
%% API Functions
%% ------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% Supervisor callbacks
%% ------------------------------------------------------------------
init([]) ->
    % Start the backend, to maintain ownership
    % TODO: Replace with table manager
    Backend = conf_helper:get(backend, ?APP),
    {ok, _Client} = Backend:start(),

    Children = [?CHILD(db_worker, worker)],
    {ok, { {one_for_one, 5, 10}, Children}}.
