%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus State
%%%
%%% Records that defines the state of this node in the consensus cluster
%%% Exists as separate module since it is to be exchanged between nodes
%%%
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_state).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([]).

%% -----------------------------------------------------------------
%% Private macros
%% -----------------------------------------------------------------

%% State of the node and cluster
-record(ncstate, {
              %% Current node
              nodename :: {node(), member_status()},

              %% Members of the cluster
              members :: [{node(), member_status()}],

              %% Leader node
              leader :: {node(), lease()},

              %% Leader election FSM state
              fsm_state :: fsm_state()
}).

%% Type of members
%% valid - Node is part of the cluster
%% invalid - Node is removed the the cluster and is to be ignored
%% joining - Node is trying to join the cluster
-type member_status() :: valid | invalid | joining.

%% Lease is a time in the future, until which the master has lease
-type lease() :: {Megaseconds::Integer,
                  Seconds::Integer,
                  Microseconds::Integer}.

%% The possible states of the leader election FSM
%% A node can either be a leader, a member or in election
-type fsm_state() :: election | leader | member.

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

