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
         join/1, leave/0,
         remove_member/1, replace_member/2,
         % Functions to be used within the consensus app
         set_master/1,
         get_slot/1, is_slot/1,
         callback/1,
         cluster_add_node/2
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
%% A clean node is trying to join an exiting cluster
%% This increases the cluster size
join(SeedNode) ->
    add_member(SeedNode, 1).

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
    remove_member(TargetNode),
    add_member(SeedNode, 0),
    TargetNode.

%% -----------------------------------------------------------------
%% Public functions, internal to the application
%% -----------------------------------------------------------------
%% Start a rcfg round to set the current node as master
set_master(Ballot) ->
    consensus_client:propose_rcfg(get_election_op(Ballot)).

%% Callback functions to change system config
callback(#rop{type=election, data={Node, Lease, Ballot}}) ->
    consensus_state:set_master(Node, Lease),
    case Node =:= ?SELF_NODE of
        true ->
            ?ASYNC_MSG(?LEADER, {master_adopted, Ballot});
        false ->
            ok
    end;
callback(#rop{type=join, data={Node, ClusterDelta}}) ->
    % Add the node as a valid member, update cluster size
    cluster_add_node(Node, ClusterDelta),
    % If master, activate the new member
    case consensus_state:is_master() of
        true ->
            rpc:call(Node,
                     consensus_rcfg, cluster_add_node, [Node, ClusterDelta]);
        false ->
            ok
    end.
    % TODO: Increment view(?), reset internal data (?)

%% Called by master leader when node is successfully added to the cluster
cluster_add_node(Node, ClusterDelta) ->
    consensus_state:set_node_status(Node, valid),
    consensus_state:set_cluster_delta(ClusterDelta).

%% Slots used for reconfiguration commands
%% We use atoms here since integers are used for the regular state machine
get_slot(election) ->
    a;
get_slot(join) ->
    b.

%% Check if given slot is a rcfg slot
is_slot(Slot) when is_atom(Slot) ->
    true;
is_slot(Slot) when is_integer(Slot) ->
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

clean_state(PropList, []) ->
    PropList;
clean_state(PropList, [H|L]) ->
    clean_state(proplists:delete(H, PropList), L).

add_member(SeedNode, ClusterDelta) ->
    % Sync self state with cluster
    [Master] = rpc:call(SeedNode, consensus_state, get_master, []),
    SysState = rpc:call(Master, consensus_state, get_state, []),
    NewSysState = clean_state(SysState, [node, status]),
    consensus_state:set_state(NewSysState),
    % Update local node status to make sure it does run master election
    consensus_state:set_node_status(join),
    % Deactivate local leader
    ?ASYNC_MSG(?LEADER, cluster_join),
    % Ask cluster to start consensus to add self
    % This increases the cluster size as per ClusterDelta
    % This works since we have the set of replicas in our local state
    consensus_client:propose_rcfg(get_join_op(ClusterDelta)).


