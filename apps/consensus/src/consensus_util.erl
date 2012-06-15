%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Util
%%%
%%% Utility functions
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Add specs
-module(consensus_util).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([ballot_greateq/2, ballot_greater/2,
         ballot_lesser/2,
         incr_ballot/2,
         ballot_equal/2,
         is_majority/1]).

-include_lib("util/include/common.hrl").

% TODO: Create types for frequently used structures

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
% Special case during init
ballot_greater({_IntA, _LeaderA}, {0, 0}) ->
    true;
% General case
ballot_greater({IntA, _LeaderA}, {IntB, _LeaderB}) ->
    IntA > IntB.

ballot_greateq({IntA, _LeaderA}, {IntB, _LeaderB}) ->
    IntA >= IntB.

ballot_lesser({IntA, _LeaderA} = BallotA, {IntB, _LeaderB} = BallotB) ->
    IntA > IntB orelse ballot_equal(BallotA, BallotB).

ballot_equal(BallotA, BallotB) ->
    BallotA =:= BallotB.

%% Assumes IntB > IntA
incr_ballot({_IntA, LeaderA}, {IntB, _LeaderB}) ->
    {IntB + 1, LeaderA}.

%% Check if the number of votes make it the majority
%% VoteCount > Number of acceptors
is_majority(VoteCount) ->
    Size = consensus_state:get_cluster_size(),
    ?LDEBUG("MAJORITY: {VoteCount, ClusterSize, Result}:: {~p, ~p, ~p}",
            [VoteCount, Size, VoteCount >= (erlang:trunc(Size/2) + 1)]),
    VoteCount >= (erlang:trunc(Size/2) + 1).
