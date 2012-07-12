%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus
%%%
%%% Main interface to the consensus app
%%% @end
%%%
%%% @since : 25 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([ping/0,
         propose/1,
         rcfg_join/1, add_repl_member/2,
         rcfg_leave/0, rcfg_remove/1,
         get_sync_member/0]).

-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
-spec ping() -> pong | pang.
ping() ->
    case consensus_state:get_node_status(?SELF_NODE) of
        valid ->
            pong;
        _ ->
            pang
    end.

-spec propose(#dop{}) -> ok.
propose(Operation) ->
    consensus_client:propose(Operation).

%% To be used only for initial setup and when nodes are empty
-spec rcfg_join(node()) -> ok.
rcfg_join(Node) ->
    consensus_rcfg:join(Node).

%% To be used when adding a member via replication
-spec add_repl_member(node(), term()) -> ok.
add_repl_member(SourceNode, Callback) ->
    consensus_rcfg:add_repl_member(SourceNode, Callback).

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

%% Try and leave the cluster
rcfg_leave() ->
    consensus_rcfg:leave().

%% Forcefully remove a node from the cluster
rcfg_remove(Node) ->
    consensus_rcfg:remove(Node).