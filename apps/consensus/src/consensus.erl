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
    try case consensus_state:get_node_status(?SELF_NODE) of
        down ->
            pang;
        _ ->
            pong
    end
    catch
        _Error:_Reason ->
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
%% It tries to make sure that the return member is not the master
-spec get_sync_member() -> node().
get_sync_member() ->
    consensus_rcfg:get_sync_member().

%% Try and leave the cluster
-spec rcfg_leave() -> ok.
rcfg_leave() ->
    consensus_rcfg:leave().

%% Forcefully remove a node from the cluster
-spec rcfg_remove(node()) -> ok.
rcfg_remove(Node) ->
    consensus_rcfg:remove(Node).
