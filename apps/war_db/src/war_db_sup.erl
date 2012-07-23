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
-module(war_db_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% Supervisor callbacks
%% ------------------------------------------------------------------
-export([init/1]).

-include_lib("war_util/include/war_common.hrl").

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
    Backend = war_util_conf:get(backend, ?APP),
    {ok, Client} = Backend:start(),

    Children = [{war_db_worker,
                 {war_db_worker, start_link, [Client]},
                 permanent,
                 5000,
                 worker,
                 [war_db_worker]}
               ],
    {ok, { {one_for_one, 5, 10}, Children}}.
