%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus State
%%%
%%% Wraps the consensus state of the node. Uses ETS internally.
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Write specs
-module(consensus_state).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([new/0, del/0,
         set_node_status/2, get_node_status/1,
         get_nodes/0, get_nodes/1, get_members/0,
         get_master/0, get_valid_master/0, is_master/0,
         set_master/1, set_master/2,
         get_lease/0, get_lease_validity/0,
         get_cluster_size/0, set_cluster_size/1
        ]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include("consensus.hrl").

%% Initial status of the node
-define(INITIAL_STATUS, valid).

%% Initial lease {erlang_time, ms}
-define(INITIAL_LEASE, {now(), 0}).

%% Self node
-define(SELF_NODE, node()).

%% Node's initial state
-define(INITIAL_SYSTEM_STATE, [
            %% Name of the current node
            {node, ?SELF_NODE},

            %% Status of the current node. Start as down
            {status, ?INITIAL_STATUS},

            %% Status of all the connected nodes, including self
            {c_status, []},

            %% Master lease time
            {lease, ?INITIAL_LEASE},

            %% Number of members needed to form the cluster
            {cluster_size, 1},

            %% Valid cluster members
            %% valid, join, down are disjoint

            %% Master node, its also present in valid
            {master, []},

            %% Valid cluster members
            {valid, []},

            %% Joining cluster members
            {join, []},

            %% Down cluster members
            {down, []}
]).

%% Name of the ets table
-define(TABLE, cons_state).

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
new() ->
    ets:new(?TABLE, [set, named_table, public]),
    % Initialize the node state and add itself to valid member list
    ets:insert(?TABLE, ?INITIAL_SYSTEM_STATE),
    set_node_status(?SELF_NODE, valid).

del() ->
    ets:delete(?TABLE).

%% Get status of a specific node in the cluster
get_node_status(Node) ->
    case lists:keyfind(Node, 1, get_state(c_status)) of
        {Node, Status} ->
            Status;
        _ ->
            undefined
    end.

%% Add a new node or update a node's cluster status
%% Makes sure master, valid, down, join are disjoint
set_node_status(Node, Status) when
  Status == master;
  Status == valid;
  Status == join;
  Status == down ->
    case get_node_status(Node) of
        undefined ->
            add_node(Node, Status);
        OldStatus ->
            update_node(Node, OldStatus, Status)
    end.

%% Get all nodes
get_nodes() ->
    [Node || {Node, _Status} <- get_state(c_status)].

%% Get a list of nodes specified
get_nodes(Type) when
  Type == valid;
  Type == master;
  Type == join;
  Type == down ->
    get_state(Type).

%% Members of the clusters who can vote
get_members() ->
    get_nodes(valid).

%% Get the master node
get_master() ->
    get_state(master).

%% Get lease time
get_lease() ->
    get_state(lease).

%% Get the amount of time the lease is valid for, in milli seconds
get_lease_validity() ->
    {LeaseStart, LeaseTime} = get_lease(),
    %% LeaseTime is in ms. Covert it to micro
    Lease = now_add(LeaseStart, LeaseTime * 1000),
    erlang:trunc(timer:now_diff(Lease, erlang:now()) / 1000).

%% Get master while making sure it still has the lease
get_valid_master() ->
    case get_lease_validity() > ?MIN_LEASE of
        true ->
            get_master();
        false ->
            undefined
    end.

%% Used as callback in master election
set_master([Node, Lease]) ->
    set_master(Node, Lease).

%% Set a new master node
set_master(Node, Lease) ->
    set_state(master, [Node]),
    set_state(lease, Lease).

%% Check if the current node is the master
is_master() ->
    get_master() =:= [get_state(node)].

%% Get required member count of the cluster
get_cluster_size() ->
    get_state(cluster_size).

%% Update the size of the cluster
set_cluster_size(Size) ->
    set_state(cluster_size, Size).

%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------
get_state(Key) ->
    [{Key, Val}] = ets:lookup(?TABLE, Key),
    Val.

set_state(Key, Value) ->
    ets:insert(?TABLE, {Key, Value}).

add_node(Node, Status) ->
    % Update main nodes list
    set_state(c_status, [{Node, Status} | get_state(c_status)]),
    % Update list based in Status
    set_state(Status, [Node | get_state(Status)]).

update_node(Node, OldStatus, NewStatus) ->
    % Update main nodes list
    set_state(c_status, lists:keyreplace(Node, 1, get_state(c_status),
                                         {Node, NewStatus})),
    % Update list based in OldStatus
    set_state(OldStatus, get_state(OldStatus) -- [Node]),
    % Update list based in NewStatus
    set_state(NewStatus, get_state(NewStatus) ++ [Node]).

now_add ({ Mega, Sec, Micro }, Add) ->
  proper ({ Mega, Sec, Micro + Add }).

proper (Time = { _, Sec, Micro }) when Sec < 1000000, Micro < 1000000 ->
  Time;
proper ({ Mega, Sec, Micro }) when Sec < 1000000 ->
  proper ({ Mega, Sec + 1, Micro - 1000000 });
proper ({ Mega, Sec, Micro }) ->
  proper ({ Mega + 1, Sec - 1000000, Micro }).
