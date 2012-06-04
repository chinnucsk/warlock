%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server Command Supervisor
%%%
%%% Spawns children for every command to be run on the server
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_command_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, create_worker/1]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-define(WORKER, server_command_worker).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% create a new worker
%% ------------------------------------------------------------------
create_worker(WorkerArgs) ->
    supervisor:start_child(?MODULE, WorkerArgs).

%% ------------------------------------------------------------------
%% supervisor callbacks
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize supervisor with child specs
%% Type = type of command - read or write
%% TODO: TYPE of command - use something else?
%% ------------------------------------------------------------------
init(WorkerType) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {WorkerType,
              {?WORKER, start_link, []},
              Restart,
              Shutdown,
              Type,
              [?WORKER]},

    {ok, {SupFlags, [AChild]}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

