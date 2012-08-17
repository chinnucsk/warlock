%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server app supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(war_server_pool_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
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
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize supervisor with child specs
%% ------------------------------------------------------------------
init([]) ->
    %% Start Ranch listener

    Port = war_util_conf:get(port, ?APP),
    PoolSize = war_util_conf:get(pool_size, ?APP),

    % Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    ranch:start_listener(war_server_listener, PoolSize,
                         ranch_tcp, [{port, Port}],
                         war_server_pool_worker, []),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
                     ?CHILD(war_server_pool_worker, temporary, worker)
                     ]}}.
