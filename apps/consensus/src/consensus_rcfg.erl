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
-export([set_master/1,
         get_slot/1, is_slot/1,
         callback/1
        ]).

%% -----------------------------------------------------------------
%% Include files and private macros
%% -----------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------
%% Start a rcfg round to set the current node as master
set_master(Ballot) ->
    consensus_client:propose(get_election_op(Ballot)).


%% Callback functions to change system config
callback(#rop{type=election, data={Node, Lease, Ballot}}) ->
    consensus_state:set_master(Node, Lease),
    case Node =:= ?SELF_NODE of
        true ->
            ?ASYNC_MSG(?LEADER, {master_adopted, Ballot});
        false ->
            ok
    end.

%% Slots used for reconfiguration commands
%% We use atoms here since integers are used for the regular state machine
get_slot(election) ->
    a.

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
