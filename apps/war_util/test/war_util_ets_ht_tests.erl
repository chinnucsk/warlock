%%%-------------------------------------------------------------------
%%% EUnit test for war_util_ht
%%%-------------------------------------------------------------------
-module(war_util_ets_ht_tests).
-include_lib("eunit/include/eunit.hrl").

%% Set, Get
set_test() ->
    HT = war_util_ets_ht:new(),
    HT1 = insert(HT),
    [?assertEqual(Value, war_util_ets_ht:get(Key, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% Delete
del_test() ->
    HT = war_util_ets_ht:new(),
    HT1 = delete(HT),
    [?assertNotEqual(Value, war_util_ets_ht:get(Key, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% Reset
res_test() ->
    HT = war_util_ets_ht:new(),
    HT1 = insert(HT),
    HT2 = war_util_ets_ht:reset(HT1),
    [?assertNotEqual(Value, war_util_ets_ht:get(Key, HT2)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% To list
list_test() ->
    HT = war_util_ets_ht:new(),
    HT1 = insert(HT),
    ListData = war_util_ets_ht:to_list(HT1),
    ?assertEqual(lists:keysort(1, ListData),
                 lists:keysort(1, lists:zip(keys(), vals()))),

    war_util_ets_ht:del(HT).

keys() ->
    [key,
     {bigger_term, {some_values, [1, 2, 3, {a, b}]}},
     1].

vals() ->
    [value,
     {big_term_val},
     2].

insert(HT) ->
    insert(HT, keys(), vals()).

insert(HT, [], []) ->
    HT;
insert(HT, [Key|Keys], [Val|Vals]) ->
    insert(war_util_ets_ht:set(Key, Val, HT), Keys, Vals).

delete(HT) ->
    delete(HT, keys()).

delete(HT, []) ->
    HT;
delete(HT, [Key|Keys]) ->
    delete(war_util_ets_ht:del(Key, HT), Keys).

