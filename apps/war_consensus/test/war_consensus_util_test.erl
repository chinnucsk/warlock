-module(war_consensus_util_test).

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------
%% test code
%%-------------------------------------------------------------------

ballot_greater_test() ->
    ?assertEqual(false, war_consensus_util:ballot_greater({1, 12, self()},
                                                      {1, 12, self()})),

    ?assertEqual(false, war_consensus_util:ballot_greater({1, 11, self()},
                                                      {1, 12, self()})),

    ?assertEqual(false, war_consensus_util:ballot_greater({1, 12, self()},
                                                      {2, 12, self()})),

    ?assertEqual(true, war_consensus_util:ballot_greater({2, 12, self()},
                                                     {1, 12, self()})),

    ?assertEqual(true, war_consensus_util:ballot_greater({1, 13, self()},
                                                     {1, 12, self()})).

ballot_lesser_test() ->
    ?assertEqual(false, war_consensus_util:ballot_lesser({1, 12, self()},
                                                    {1, 12, self()})),

    ?assertEqual(true, war_consensus_util:ballot_lesser({1, 11, self()},
                                                    {1, 12, self()})),

    ?assertEqual(true, war_consensus_util:ballot_lesser({1, 12, self()},
                                                    {2, 12, self()})),

    ?assertEqual(false, war_consensus_util:ballot_lesser({2, 12, self()},
                                                     {1, 12, self()})),

    ?assertEqual(false, war_consensus_util:ballot_lesser({1, 13, self()},
                                                     {1, 12, self()})).

ballot_greateq_test() ->
    ?assertEqual(true, war_consensus_util:ballot_greateq({1, 12, self()},
                                                      {1, 12, self()})),

    ?assertEqual(false, war_consensus_util:ballot_greateq({1, 11, self()},
                                                      {1, 12, self()})),

    ?assertEqual(false, war_consensus_util:ballot_greateq({1, 12, self()},
                                                      {2, 12, self()})),

    ?assertEqual(true, war_consensus_util:ballot_greateq({2, 12, self()},
                                                     {1, 12, self()})),

    ?assertEqual(true, war_consensus_util:ballot_greateq({1, 13, self()},
                                                     {1, 12, self()})).

ballot_equal_test() ->
    ?assertEqual(true, war_consensus_util:ballot_equal({1, 12, self()},
                                                   {1, 12, self()})),

    Pid = spawn(fun() -> ok end),
    ?assertEqual(true, war_consensus_util:ballot_equal({1, 12, self()},
                                                   {1, 12, Pid})).

ballot_same_test() ->
    ?assertEqual(true, war_consensus_util:ballot_same({1, 12, self()},
                                                  {1, 12, self()})),

    Pid = spawn(fun() -> ok end),
    ?assertEqual(false, war_consensus_util:ballot_same({1, 12, self()},
                                                   {1, 12, Pid})).

