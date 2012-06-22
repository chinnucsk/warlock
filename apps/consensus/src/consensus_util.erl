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
         incr_ballot/2, incr_view/1,
         ballot_equal/2, ballot_same/2,
         is_majority/1,
         get_lease/0]).

-include_lib("util/include/common.hrl").
-include("consensus.hrl").

% TODO: Create types for frequently used structures

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
ballot_greater({ViewA, _IntA, _LeaderA}, {ViewB, _IntB, _LeaderB})
  when ViewA < ViewB ->
    false;
ballot_greater({ViewA, IntA, _LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewA =:= ViewB ->
    IntA > IntB;
ballot_greater(_BallotA, _BallotB) ->
    true.

ballot_lesser({ViewA, _IntA, _LeaderA}, {ViewB, _IntB, _LeaderB})
  when ViewA > ViewB ->
    false;
ballot_lesser({ViewA, IntA, _LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewA =:= ViewB ->
    IntA < IntB;
ballot_lesser(_BallotA, _BallotB) ->
    true.

ballot_equal({View, Int, _LeaderA}, {View, Int, _LeaderB}) ->
    true;
ballot_equal(_BallotA, _BallotB) ->
    false.

ballot_same(BallotA, BallotB) ->
    BallotA =:= BallotB.

ballot_greateq(BallotA, BallotB) ->
    ballot_greater(BallotA, BallotB) orelse ballot_equal(BallotA, BallotB).

%% Increment only allowed when second ballot is greater
incr_ballot({ViewA, IntA, LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewB >= ViewA andalso IntB >= IntA ->
    {ViewB, IntB + 1, LeaderA}.

%% View change reset ballot's incrementing id
incr_view({View, _Int, Leader}) ->
    {View + 1, 0, Leader}.

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
