%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Sup
%%%
%%% Main supervisor for consensus app
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Add specs
-module(war_consensus_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% Supervisor callbacks
%% ------------------------------------------------------------------
-export([init/1]).

%% ------------------------------------------------------------------
%% Includes and macros
%% ------------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Restart, Type),
{   I,                      %% Id
    {I, start_link, []},    %% Start function
    Restart,                %% Restart strategy
    5000,                   %% Shutdown strategy/time
    Type,                   %% Type of process
    [I]                     %% Modules
}
).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% supervisor callbacks
%% ------------------------------------------------------------------
init([]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),
    % Init consensus state
    war_consensus_state:new(),

    RestartStrategy = one_for_all,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = lists:flatten(
        [?CHILD(war_consensus_acceptor, transient, worker),
         ?CHILD(war_consensus_replica, transient, worker),
         ?CHILD(war_consensus_leader, transient, worker),
         ?CHILD(war_consensus_client, permanent, worker)
         ]),
    {ok, {SupFlags, Children}}.
