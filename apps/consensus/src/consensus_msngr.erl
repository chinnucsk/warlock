%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Messenger
%%%
%%% Helper module used to send different types of messages
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Get the registered names of services from a central config file
-module(consensus_msngr).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([sync/2, async/2]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include("consensus.hrl").

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
-spec sync(atom(), term()) -> term().
sync(Target, Msg) ->
    TargetAdd = get_address(Target),
    gen_server:call(TargetAdd, Msg).

-spec async(atom(), term()) -> ok.
async(Target, Msg) when
  is_pid(Target);
  Target == ?LEADER;
  Target == ?ACCEPTOR;
  Target == ?REPLICA ->
    gen_server:cast(Target, Msg);
async(Target, Msg) ->
    {Name, Nodes} = get_address(Target),
    gen_server:abcast(Nodes, Name, Msg).

%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------
%% Get address based on given target
% Leader process on the master node
get_address(leaders) ->
    {?LEADER, consensus_state:get_members()};
% Acceptors on valid set of nodes
get_address(acceptors) ->
    {?ACCEPTOR, consensus_state:get_members()};
% Replicas on valid set of nodes
get_address(replicas) ->
    {?REPLICA, consensus_state:get_members()};
% Replica process on the master node
get_address(master_replica) ->
    {?REPLICA, consensus_state:get_master()};
% Leader process on the master node
get_address(master_leader) ->
    {?LEADER, consensus_state:get_master()};
% Leaders on set of down nodes
get_address(down_leaders) ->
    {?LEADER, consensus_state:get_nodes(down)};
% General case where registered name/pid and node is specified
get_address({Name, Node}) ->
    {Name, [Node]}.