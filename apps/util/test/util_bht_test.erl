%%%-------------------------------------------------------------------
%%% EUnit test for util_bht
%%%-------------------------------------------------------------------
-module(util_bht_test).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
    [?_test(simple_tst_())].

simple_tst_() ->
    HT = util_bht:new(),
    Keys = keys(),
    Vals = vals(),
    insert_mult(HT, Keys, Vals),

    % Check by getting vals through keys
    HTVals = getk_mult(HT, Keys),
    ?assertEqual(Vals, HTVals),

    % Check by getting keys through vals
    HTKeys = getv_mult(HT, Vals),
    ?assertEqual(Keys, HTKeys),

    % Delete all the objects
    delete_mult(HT, Keys, Vals),

    % Check if deleted via keys
    DHTVals = getk_mult(HT, Keys),
    ?assertNotEqual(Vals, DHTVals),

    % Check if deleted via vals
    DHTKeys = getv_mult(HT, Vals),
    ?assertNotEqual(Keys, DHTKeys),

    util_bht:del(HT).

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
    util_bht:set(Key, Val, HT),
    insert_mult(HT, KTail, VTail).


getk_mult(HT, Keys) ->
    lists:reverse(getk_mult(HT, Keys, [])).

getk_mult(_HT, [], Acc) ->
    Acc;
getk_mult(HT, [Key | Tail], Acc) ->
    getk_mult(HT, Tail, [util_bht:keyget(Key, HT) | Acc]).

getv_mult(HT, Keys) ->
    lists:reverse(getv_mult(HT, Keys, [])).

getv_mult(_HT, [], Acc) ->
    Acc;
getv_mult(HT, [Key | Tail], Acc) ->
    getv_mult(HT, Tail, [util_bht:valget(Key, HT) | Acc]).

delete_mult(_HT, [], []) ->
    ok;
delete_mult(HT, [Key | KTail], [Val | VTail]) ->
    util_bht:del(Key, Val, HT),
    delete_mult(HT, KTail, VTail).
