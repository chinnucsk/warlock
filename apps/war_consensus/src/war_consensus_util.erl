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
-module(war_consensus_util).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([ballot_greateq/2, ballot_greater/2,
         ballot_lesser/2,
         incr_ballot/2, incr_view/1,
         ballot_equal/2, ballot_same/2,
         is_view_change/2,
         is_majority/1,
         get_lease/0,
         stop_app/0, stop_app/1]).

-include_lib("war_util/include/war_common.hrl").
-include("war_consensus.hrl").

% Time to wait to stop consensus application
-define(STOP_TIME, 2000).

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
-spec ballot_greater(ballot(), ballot()) -> boolean().
ballot_greater({ViewA, _IntA, _LeaderA}, {ViewB, _IntB, _LeaderB})
  when ViewA < ViewB ->
    false;
ballot_greater({ViewA, IntA, _LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewA =:= ViewB ->
    IntA > IntB;
ballot_greater(_BallotA, _BallotB) ->
    true.

-spec ballot_lesser(ballot(), ballot()) -> boolean().
ballot_lesser({ViewA, _IntA, _LeaderA}, {ViewB, _IntB, _LeaderB})
  when ViewA > ViewB ->
    false;
ballot_lesser({ViewA, IntA, _LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewA =:= ViewB ->
    IntA < IntB;
ballot_lesser(_BallotA, _BallotB) ->
    true.

-spec ballot_equal(ballot(), ballot()) -> boolean().
ballot_equal({View, Int, _LeaderA}, {View, Int, _LeaderB}) ->
    true;
ballot_equal(_BallotA, _BallotB) ->
    false.

-spec ballot_same(ballot(), ballot()) -> boolean().
ballot_same(BallotA, BallotB) ->
    BallotA =:= BallotB.

-spec ballot_greateq(ballot(), ballot()) -> boolean().
ballot_greateq(BallotA, BallotB) ->
    ballot_greater(BallotA, BallotB) orelse ballot_equal(BallotA, BallotB).

-spec is_view_change(ballot(), ballot()) -> boolean().
is_view_change({ViewA, _IntA, _LeaderA}, {ViewB, _IntB, _LeaderB})
    when ViewA > ViewB ->
      true;
is_view_change(_, _) ->
    false.

%% Increment only allowed when second ballot is greater
-spec incr_ballot(ballot(), ballot()) -> ballot().
incr_ballot({ViewA, IntA, LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewB =:= ViewA andalso IntB >= IntA ->
    {ViewB, IntB + 1, LeaderA};
incr_ballot({ViewA, _IntA, LeaderA}, {ViewB, IntB, _LeaderB})
  when ViewB > ViewA ->
    {ViewB, IntB + 1, LeaderA}.

%% View change reset ballot's incrementing id
-spec incr_view(ballot()) -> ballot().
incr_view({View, _Int, Leader}) ->
    {View + 1, 0, Leader}.

%% Check if the number of votes make it the majority
%% VoteCount > Number of acceptors
-spec is_majority(integer()) -> boolean().
is_majority(VoteCount) ->
    Size = war_consensus_state:get_cluster_size(),
    ?LDEBUG("MAJORITY: {VoteCount, ClusterSize, Result}:: {~p, ~p, ~p}",
            [VoteCount, Size, VoteCount >= (erlang:trunc(Size/2) + 1)]),
    VoteCount >= (erlang:trunc(Size/2) + 1).

%% Get default lease for master
-spec get_lease() -> tuple().
get_lease() ->
    {now(), ?LEASE_TIME}.

% Stop the consensus application after given time
-spec stop_app() -> ok.
stop_app() ->
    stop_app(?STOP_TIME).

-spec stop_app(integer()) -> ok.
stop_app(Time) ->
    spawn(fun()-> stop(Time) end).

stop(Time) ->
    receive
    after
        Time ->
            application:stop(?APPLICATION)
    end.
