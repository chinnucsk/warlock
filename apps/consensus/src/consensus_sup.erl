
-module(consensus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


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

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),
    % Init consensus state
    consensus_state:new(),

    RestartStrategy = one_for_all,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    %%TODO: Change supervision tree such that commander and scouts die when
    %% corresponsing leader dies
    Children = lists:flatten(
        [?CHILD(consensus_acceptor, transient, worker),
         ?CHILD(consensus_replica, transient, worker),
         ?CHILD(consensus_scout_sup, permanent, supervisor),
         ?CHILD(consensus_commander_sup, permanent, supervisor),
         ?CHILD(consensus_leader_sup, permanent, supervisor)]),
    {ok, {SupFlags, Children}}.
