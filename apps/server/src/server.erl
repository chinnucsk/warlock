%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server wrapper, interface for handling server requests
%%%
%%% The Server module consists of a supervised worker that runs
%%% commands on the local DB as per consensus
%%%
%%% How a request is handled
%%% 1. A command is constructed based on the request (command_worker)
%%% 2. Command is passed on to the consensus module
%%% 3. Consensus module gets agreement based on the client
%%% 4. The callback function (part of command) is called by consensus module
%%% 5. If reply required, the master replies
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Write specs
-module(server).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([ping/0, ping_service/0, ping_backend/0,
         x/1,
         receive_complete/1, ready_repl/1,
         repl/1]).

%% -----------------------------------------------------------------
%% Private macros and include files
%% -----------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("server.hrl").

-define(SELF_NODE, node()).

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Check if this server is up.
%%-------------------------------------------------------------------
-spec ping() -> pong | pang.
ping() ->
    pong.

%%-------------------------------------------------------------------
%% @doc
%% Check if the service(cluster) is up.
%%-------------------------------------------------------------------
-spec ping_service() -> pong | pang.
ping_service() ->
    consensus:ping().

%%-------------------------------------------------------------------
%% @doc
%% Check if the backend for this server is up.
%%-------------------------------------------------------------------
-spec ping_backend() -> pong | pang.
ping_backend() ->
    db:ping().

%%-------------------------------------------------------------------
%% @doc
%% eXecute a command on the database
%%-------------------------------------------------------------------
-spec x(term()) -> term().
x(Cmd) ->
    spawncall_worker(Cmd).

%%-------------------------------------------------------------------
%% @doc
%% Join this node to the cluster
%%-------------------------------------------------------------------
repl(SeedNode) ->
    ?LDEBUG("Replicating from seed node: ~p", [SeedNode]),
    % Get a member node from the master from which data is in sync
    SourceNode = rpc:call(SeedNode, consensus, get_sync_member, []),
    % Send a message to SourceNode to get ready
    {ok, _Pid} = rpc:call(SourceNode, server, ready_repl, [?SELF_NODE]),
    % Start receiver
    server_receiver:start_link(SourceNode),
    ok.

%% ------------------------------------------------------------------
%% Exported Function Definitions to be only used within the app
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Fun called by server_receiver once process is complete
%%-------------------------------------------------------------------
receive_complete(SourceNode) ->
    % DB backup is now synced. Add self to cluster
    Callback = {server_callback, trig_active, []},
    consensus:add_repl_member(SourceNode, Callback).

%%-------------------------------------------------------------------
%% @doc
%% Some node requests to replicate from this node
%%-------------------------------------------------------------------
ready_repl(FromNode) ->
    % Check if replication is already in progress
    case server_callback:is_active() of
        true ->
            % Set server as inactive
            server_callback:set_inactive(),
            % Add the receiving node as subscriber for queued decisions
            server_callback:add_subscriber(FromNode),
            % Start sender
            {ok, _Pid} = server_sender:start_link();
        false ->
            {error, repl_in_progress}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% TODO: Check if there is a usecase where we need cast worker
spawncall_worker(Cmd) ->
    {ok, Worker} = server_command_worker:start_link(),
    try gen_server:call(Worker, {request, Cmd})
    catch
        exit:{timeout, _} -> {error, timeout}
    end.
