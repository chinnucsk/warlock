%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server wrapper, interface for handling server requests
%%%
%%% The Server module consists of a worker that runs
%%% commands on the local DB as per consensus
%%%
%%% How a request is handled
%%% 1. A command is constructed based on the request
%%% 2. Command is passed on to the consensus module via war_server_worker
%%% 3. Consensus module gets agreement based on the client
%%% 4. The callback function (part of command) is called by consensus module
%%% 5. If reply required, the master replies for "CLUSTER" requests and local
%%%    replica replies for "LOCAL" requests
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_server).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([ping/0, ping_service/0, ping_backend/0,
         x/2,
         repl/1,
         receive_complete/1, ready_repl/1
         ]).

%% -----------------------------------------------------------------
%% Private macros and include files
%% -----------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").
-include("war_server.hrl").

-define(SELF_NODE, node()).

%% Send request to server worker (single gen_server)
-define(WORKER(Type, Cmd), war_server_worker:request(Type, Cmd)).

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
    war_consensus:ping().

%%-------------------------------------------------------------------
%% @doc
%% Check if the backend for this server is up.
%%-------------------------------------------------------------------
-spec ping_backend() -> pong | pang.
ping_backend() ->
    war_db:ping().

%%-------------------------------------------------------------------
%% @doc
%% eXecute a command on the database
%% loc = execute the command on the local replica. Usually for reads
%% cls = execute the command on the cluster. Must for writes
%%-------------------------------------------------------------------
-spec x(loc|cls, term()) -> term().
x(?LOCAL, Cmd) ->
    ?WORKER(?LOCAL, Cmd);
x(?CLUSTER, Cmd) ->
    ?WORKER(?CLUSTER, Cmd);
x(_, _Cmd) ->
    erlang:error(unknown_type).

%%-------------------------------------------------------------------
%% @doc
%% Join this node to the cluster
%%-------------------------------------------------------------------
-spec repl(node()) -> ok.
repl(SeedNode) ->
    ?LDEBUG("Replicating from seed node: ~p", [SeedNode]),
    % Get a member node from the master from which data is in sync
    SourceNode = rpc:call(SeedNode, war_consensus, get_sync_member, []),
    % Send a message to SourceNode to get ready
    {ok, _Pid} = rpc:call(SourceNode, war_server, ready_repl, [?SELF_NODE]),
    % Start receiver
    war_server_receiver:start_link(SourceNode),
    ok.

%% ------------------------------------------------------------------
%% Exported Function Definitions to be only used within the app
%% ------------------------------------------------------------------
%%-------------------------------------------------------------------
%% @doc
%% Fun called by war_server_receiver once process is complete
%%-------------------------------------------------------------------
-spec receive_complete(node()) -> ok.
receive_complete(SourceNode) ->
    % DB backup is now synced. Add self to cluster
    Callback = {war_server_callback, trig_active, []},
    war_consensus:add_repl_member(SourceNode, Callback).

%%-------------------------------------------------------------------
%% @doc
%% Some node requests to replicate from this node
%%-------------------------------------------------------------------
-spec ready_repl(node()) -> {ok, pid()} | {error, repl_in_progress}.
ready_repl(FromNode) ->
    % Check if replication is already in progress
    case war_server_callback:is_active() of
        true ->
            % Set server as inactive
            war_server_callback:set_inactive(),
            % Add the receiving node as subscriber for queued decisions
            war_server_callback:add_subscriber(FromNode),
            % Start sender
            war_server_sender:start_link();
        false ->
            {error, repl_in_progress}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
