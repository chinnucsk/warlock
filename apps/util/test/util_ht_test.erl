%%%-------------------------------------------------------------------
%%% EUnit test for util_ht
%%%-------------------------------------------------------------------
-module(util_ht_test).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
    [?_test(simple_tst_())].

simple_tst_() ->
    HT = util_ht:new(),
    Keys = keys(),
    Vals = vals(),

    insert_mult(HT, Keys, Vals),
    HTVals = get_mult(HT, Keys),
    ?assertEqual(Vals, HTVals),

    delete_mult(HT, Keys),
    DHTVals = get_mult(HT, Keys),
    ?assertNotEqual(Vals, DHTVals),

    insert_mult(HT, Keys, Vals),
    util_ht:reset(HT),
    RHTVals = get_mult(HT, Keys),
    ?assertNotEqual(Vals, RHTVals),

    util_ht:del(HT).

%%-------------------------------------------------------------------
%% helper functions
%%-------------------------------------------------------------------

keys() ->
    [key, {bigger_term, {some_values, [1, 2, 3, {a, b}]}}, 1].

vals() ->
    [value, {big_term_val}, 2].

insert_mult(_HT, [], []) ->
    ok;
insert_mult(HT, [Key | KTail], [Val | VTail]) ->
    util_ht:set(Key, Val, HT),
    insert_mult(HT, KTail, VTail).


get_mult(HT, Keys) ->
    lists:reverse(get_mult(HT, Keys, [])).

get_mult(_HT, [], Acc) ->
    Acc;
get_mult(HT, [Key | Tail], Acc) ->
    get_mult(HT, Tail, [util_ht:get(Key, HT) | Acc]).

delete_mult(_HT, []) ->
    ok;
delete_mult(HT, [Key | Tail]) ->
    util_ht:del(Key, HT),
    delete_mult(HT, Tail).
