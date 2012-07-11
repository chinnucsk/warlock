%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus callback
%%%
%%% The consensus module calls this module for reconfiguration purposes
%%% @end
%%%
%%% @since : 20 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_rcfg).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([% Functions to be used by external apps
         join/1, leave/0, add_repl_member/2,
         remove_member/1, replace_member/2,
         % Functions to be used within the consensus app
         set_master/1, node_down/1,
         get_slot/1, is_slot/1,
         callback/1,
         cluster_add_node/2,
         get_election_node/1
        ]).

%% -----------------------------------------------------------------
%% Include files and private macros
%% -----------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------
%% Join an existing cluster
%% To be called only after this node's dataset and consensus app state
%% is in sync with the cluster
%% This increases the cluster size
-spec join(node()) -> ok.
join(SeedNode) ->
    add_member(empty, SeedNode, 1, undefined).

%% Add a new member via replication
-spec add_repl_member(node(), term()) -> ok.
add_repl_member(SourceNode, Callback) ->
    add_member(repl, SourceNode, 1, Callback).

%FIXME
%% A member of the cluster tries to leave
%% Size of the cluster decreases
leave() ->
    ok.

%FIXME
%% Forcefully remove a member
%% TODO: Do we need to export this option?
%% Size of the cluster remains same
remove_member(Node) ->
    Node.

%FIXME
%% Replace an existing member in the cluster
%% Size of the cluster remains same
replace_member(SeedNode, TargetNode) ->
    {SeedNode, TargetNode}.

%% -----------------------------------------------------------------
%% Public functions, internal to the application
%% -----------------------------------------------------------------
%% Start a rcfg round to set the current node as master
-spec set_master(ballot()) -> ok.
set_master(Ballot) ->
    consensus_client:propose_rcfg(get_election_op(Ballot)).

%% Move a node from status "valid" to "down"
-spec node_down(node()) -> ok.
node_down(Node) ->
    consensus_client:propose_rcfg(get_node_down_op(Node)).

%% Callback functions to change system config
-spec callback(#rop{}) -> ok.
callback(#rop{type=election,
              data={Node, Lease, Ballot}}) ->
    OldMaster = case consensus_state:get_master() of
        [] ->
            undefined;
        [OldNode] ->
            OldNode
    end,

    consensus_state:set_master(Node, Lease),

    % All proposals made to the old master are no longer valid since they
    % will not get quorum. Reset the min_slot_num to slot_num to repropose
    % for the unused slots. Requests sent to old master will timeout
    ?ASYNC_MSG(?REPLICA, new_master),

    % If master, send a message to create timer for renewing the lease
    case Node =:= ?SELF_NODE of
        true ->
            ?ASYNC_MSG(?LEADER, {master_adopted, Ballot, OldMaster}),
            % Send a message to "down" nodes to shutdown, in case they are back
            ?ASYNC_MSG(down_leaders, stop_out_of_sync);
        false ->
            ok
    end;
callback(#rop{type=join,
              data={Node, ClusterDelta}}) ->
    % Add the node as a valid member, update cluster size
    cluster_add_node(Node, ClusterDelta),
    % If master, activate the new member
    case consensus_state:is_master() of
        true ->
            rpc:call(Node,
                     consensus_rcfg, cluster_add_node, [Node, ClusterDelta]),
            ?LEADER:monitor(Node);
        false ->
            ok
    end;
%% This is the stoppable state machine part
%% Consensus app state is stopped and view incremented allowing the entire
%% state to start fresh. All decisions made in the previous view and not
%% committed are dropped. Those requests will timeout and client has to resend
callback(#rop{type=repl_join,
              data={SourceNode, Node, ClusterDelta, {M, F, A}}}) ->
    IsMaster = consensus_state:is_master(),

    % Add the node as a valid member, update cluster size
    cluster_add_node(Node, ClusterDelta),

    % Reset all the consensus actors' state and increment "view"
    case IsMaster of
        true -> ?LEADER:incr_view();
        false -> ?LEADER:reset()
    end,
    ?REPLICA:reset(),
    ?ACCEPTOR:reset(),

    % If source node, inform execute callback
    case SourceNode =:= ?SELF_NODE of
        true ->
            erlang:apply(M, F, A);
        false ->
            ok
    end,

    % If master, activate the new member
    case IsMaster of
        true ->
            rpc:call(Node,
                     consensus_rcfg, cluster_add_node, [Node, ClusterDelta]),
            ?LEADER:monitor(Node);
        false ->
            ok
    end;
callback(#rop{type=node_down, data=Node}) ->
    consensus_state:set_node_status(Node, down).


%% Addition of new node to the cluster - update local state
-spec cluster_add_node(node(), integer()) -> integer().
cluster_add_node(Node, ClusterDelta) ->
    consensus_state:set_node_status(Node, valid),
    consensus_state:set_cluster_delta(ClusterDelta).

%% Slots used for reconfiguration commands
%% We use atoms here since integers are used for the regular state machine
-spec get_slot(atom()) -> atom().
get_slot(election) ->
    a;
get_slot(join) ->
    b;
get_slot(repl_join) ->
    c;
get_slot(node_down) ->
    d.

%% Check if given slot is a rcfg slot
-spec is_slot(slot()) -> boolean().
is_slot(Slot) when is_atom(Slot) ->
    true;
is_slot(Slot) when is_integer(Slot) ->
    false.

%% Return node of if given proposal is for election
-spec get_election_node(#rop{}) -> node() | false.
get_election_node(#rop{type=election, data={Node, _, _}}) ->
    Node;
get_election_node(_) ->
    false.

%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------
%% Generate reconfig operations
get_election_op(Ballot) ->
    #rop{type=election,
         data={?SELF_NODE, consensus_util:get_lease(), Ballot}
        }.

get_join_op(ClusterDelta) ->
    #rop{type=join,
         data={?SELF_NODE, ClusterDelta}
        }.

get_repl_op(SourceNode, ClusterDelta, Callback) ->
    #rop{type=repl_join,
         data={SourceNode, ?SELF_NODE, ClusterDelta, Callback}}.

get_node_down_op(Node) ->
    #rop{type=node_down,
         data=Node}.

% Pull cluster state and sync self
sync_with_cluster(SeedNode) ->
    [Master] = rpc:call(SeedNode, consensus_state, get_master, []),
    SysState = rpc:call(Master, consensus_state, get_state, []),
    NewSysState = clean_state(SysState, [node, status]),
    consensus_state:set_state(NewSysState).

% Update local node status to make sure it does run master election
disable_local_leader() ->
    consensus_state:set_node_status(join),
    % Deactivate local leader
    ?ASYNC_MSG(?LEADER, cluster_join).

clean_state(PropList, []) ->
    PropList;
clean_state(PropList, [H|L]) ->
    clean_state(proplists:delete(H, PropList), L).

% Add self to the cluster
add_member(Type, Node, ClusterDelta, Callback) ->
    sync_with_cluster(Node),
    disable_local_leader(),
    Operation = case Type of
        empty ->
            get_join_op(ClusterDelta);
        repl ->
            get_repl_op(Node, ClusterDelta, Callback)
    end,
    % Ask cluster to start consensus to add self
    % This increases the cluster size as per ClusterDelta
    % This works since we have the set of replicas in our local state
    consensus_client:propose_rcfg(Operation).
