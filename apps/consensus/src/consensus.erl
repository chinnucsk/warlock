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

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------

propose(Operation) ->
    consensus_client:propose(Operation).

%% To be used only for initial setup and when nodes are empty
rcfg_join(Node) ->
    consensus_rcfg:join(Node).

%% To be used when adding a member via replication
add_repl_member(SourceNode, Callback) ->
    consensus_rcfg:add_repl_member(SourceNode, Callback).

%% Get a member that is in sync with the cluster
%% Try to make sure that the return member is not the master
% TODO: Implement priority list to keep track of "good" members?
get_sync_member() ->
    AllMembers = consensus_state:get_members(),
    Master = consensus_state:get_master(),
    case AllMembers of
        Master ->
            hd(Master);
        _ ->
            hd(AllMembers -- Master)
    end.