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
-export([propose/1,
         rcfg_join/1, add_repl_member/2,
         get_sync_member/0]).

-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
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