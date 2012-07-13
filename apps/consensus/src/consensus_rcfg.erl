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
         join/1, add_repl_member/2,
         leave/0, remove/1,
         % Functions to be used within the consensus app
         set_master/1, node_down/1,
         get_slot/1, is_slot/1,
         callback/1,
         cluster_add_node/2,
         get_election_node/1,
         transfer_master/0, transfer_master/1,
         get_sync_member/0
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
%% This increases the cluster size
-spec add_repl_member(node(), term()) -> ok.
add_repl_member(SourceNode, Callback) ->
    add_member(repl, SourceNode, 1, Callback).

%% A member of the cluster tries to leave
%% Size of the cluster decreases
-spec leave() -> ok.
leave() ->
    remove(?SELF_NODE).

%% Forcefully remove a member
%% Size of the cluster decreases
-spec remove(node()) -> ok.
remove(Node) ->
    %TODO: For removing master node, transfer it first and then remove
    % for now, let user handle it manually
    case consensus_state:get_master() =:= [Node] of
        true ->
            {error, master_removal};
        false ->
            consensus_client:sync_propose_rcfg(remove_op(Node))
    end.

-spec transfer_master() -> ok.
transfer_master() ->
    transfer_master(get_sync_member()).

-spec transfer_master(node()) -> ok.
transfer_master(Node) ->
    case consensus_state:get_master() of
        [Node] ->
            {error, cannot_transfer_to_master};
        _ ->
            consensus_client:sync_propose_rcfg(transfer_master_op(Node))
    end.


%% -----------------------------------------------------------------
%% Public functions, internal to the application
%% -----------------------------------------------------------------
%% Start a rcfg round to set the current node as master
-spec set_master(ballot()) -> ok.
set_master(Ballot) ->
    consensus_client:propose_rcfg(election_op(Ballot)).

%% Move a node from status "valid" to "down"
-spec node_down(node()) -> ok.
node_down(Node) ->
    consensus_client:propose_rcfg(node_down_op(Node)).

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

    %TODO: Master can demonitor "Node" here for a cleaner implementation

    % If master, activate the new member
    case consensus_state:is_master() of
        true ->
            rpc:call(Node,
                     consensus_rcfg, cluster_add_node, [Node, ClusterDelta]),
            ?ASYNC_MSG(?LEADER, {monitor, Node}),
            ?ASYNC_MSG({?LEADER, Node}, delay_election);
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
    reset_cons_state(IsMaster),

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
            ?ASYNC_MSG({?LEADER, Node}, delay_election),
            ?ASYNC_MSG(?LEADER, {monitor, Node});
        false ->
            ok
    end;
callback(#rop{type=node_down, data=Node}) ->
    consensus_state:set_node_status(Node, down);
callback(#rop{type=remove, data={Node, ClusterDelta}}) ->
    IsMaster = consensus_state:is_master(),

    % Remove the given node from state
    consensus_state:remove_node(Node),
    consensus_state:set_cluster_delta(ClusterDelta),

    reset_cons_state(IsMaster),
    case IsMaster of
        true ->
            ?ASYNC_MSG(?LEADER, {demonitor, Node});
        false ->
            ok
    end,

    case Node =:= ?SELF_NODE of
        true ->
            ?ASYNC_MSG(?LEADER, stop_out_of_sync);
        false ->
            ok
    end;
callback(#rop{type=transfer_master, data=Node}) ->
    IsMaster = consensus_state:is_master(),
    IsNewMaster = Node =:= ?SELF_NODE,

    % If master, make it a normal member
    case {IsMaster, IsNewMaster} of
        {true, false} ->
            ?ASYNC_MSG(?LEADER, disable_master);
        {false, true} ->
            ?ASYNC_MSG(?LEADER, start_election);
        {true, true} ->
            ?LINFO("Possible bug: Cannot transfer, already master");
        {false, false} ->
            ?ASYNC_MSG(?LEADER, delay_election)
    end.


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
    d;
get_slot(remove) ->
    e;
get_slot(transfer_master) ->
    f.

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
election_op(Ballot) ->
    #rop{type=election,
         data={?SELF_NODE, consensus_util:get_lease(), Ballot}
        }.

join_op(ClusterDelta) ->
    #rop{type=join,
         data={?SELF_NODE, ClusterDelta}
        }.

repl_op(SourceNode, ClusterDelta, Callback) ->
    #rop{type=repl_join,
         data={SourceNode, ?SELF_NODE, ClusterDelta, Callback}}.

node_down_op(Node) ->
    #rop{type=node_down,
         data=Node}.

remove_op(Node) ->
    #rop{type=remove,
         data={Node, -1}}.

transfer_master_op(Node) ->
    #rop{type=transfer_master,
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
            join_op(ClusterDelta);
        repl ->
            repl_op(Node, ClusterDelta, Callback)
    end,
    % Ask cluster to start consensus to add self
    % This increases the cluster size as per ClusterDelta
    % This works since we have the set of replicas in our local state
    consensus_client:propose_rcfg(Operation).

% Increment the view in the master leader ballot and reset state
% This is used whenever there is a change to the cluster configuration
% This helps ignore messages, decisions from previous cluster config
reset_cons_state(IsMaster) ->
    case IsMaster of
        true -> ?ASYNC_MSG(?LEADER, incr_view);
        false -> ?ASYNC_MSG(?LEADER, reset)
    end,
    ?ASYNC_MSG(?REPLICA, reset),
    ?ASYNC_MSG(?ACCEPTOR, reset).

%% Get a member that is in sync with the cluster
%% Try to make sure that the return member is not the master
% TODO: Implement priority list to keep track of "good" members?
-spec get_sync_member() -> node().
get_sync_member() ->
    AllMembers = consensus_state:get_members(),
    Master = consensus_state:get_master(),
    case AllMembers of
        Master ->
            hd(Master);
        _ ->
            hd(AllMembers -- Master)
    end.