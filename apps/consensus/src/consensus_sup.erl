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
-module(consensus_sup).
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
-include_lib("util/include/common.hrl").

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
    consensus_state:new(),

    RestartStrategy = one_for_all,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = lists:flatten(
        [?CHILD(consensus_acceptor, transient, worker),
         ?CHILD(consensus_replica, transient, worker),
         ?CHILD(consensus_leader, transient, worker),
         ?CHILD(consensus_client, permanent, worker)
         ]),
    {ok, {SupFlags, Children}}.
