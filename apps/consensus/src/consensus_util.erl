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
         ballot_equal/2, ballot_same/2,
         is_majority/1,
         get_lease/0]).

-include_lib("util/include/common.hrl").
-include("consensus.hrl").

% TODO: Create types for frequently used structures

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
% General case
ballot_greater(BallotA, BallotB) ->
    case max(BallotA, BallotB) of
        BallotA ->
            true;
        BallotB ->
            false
    end.

ballot_lesser({IntA, _LeaderA}, {IntB, _LeaderB}) ->
    IntA < IntB.

ballot_equal({IntA, _LeaderA}, {IntB, _LeaderB}) ->
    IntA =:= IntB.

ballot_same(BallotA, BallotB) ->
    BallotA =:= BallotB.

ballot_greateq(LeaderA, LeaderB) ->
    ballot_greater(LeaderA, LeaderB) orelse ballot_equal(LeaderA, LeaderB).

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

%% Get default lease for master
get_lease() ->
    {now(), ?LEASE_TIME}.
