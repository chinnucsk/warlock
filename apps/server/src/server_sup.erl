%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server app supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(server_sup).
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
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
                     ?CHILD(server_callback, permanent, worker),
                     ?CHILD(server_worker, permanent, worker)
                     ]}}.
