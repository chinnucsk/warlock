%%%-------------------------------------------------------------------
%%% EUnit test for util_ht
%%%-------------------------------------------------------------------
-module(ets_ht_tests).
-include_lib("eunit/include/eunit.hrl").

%% Set, Get
set_test() ->
    HT = ets_ht:new(),
    HT1 = insert(HT),
    [?assertEqual(Value, ets_ht:get(Key, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% Delete
del_test() ->
    HT = ets_ht:new(),
    HT1 = delete(HT),
    [?assertNotEqual(Value, ets_ht:get(Key, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% Reset
res_test() ->
    HT = ets_ht:new(),
    HT1 = insert(HT),
    HT2 = ets_ht:reset(HT1),
    [?assertNotEqual(Value, ets_ht:get(Key, HT2)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% To list
list_test() ->
    HT = ets_ht:new(),
    HT1 = insert(HT),
    ListData = ets_ht:to_list(HT1),
    ?assertEqual(lists:keysort(1, ListData),
                 lists:keysort(1, lists:zip(keys(), vals()))),

    ets_ht:del(HT).

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
    insert(ets_ht:set(Key, Val, HT), Keys, Vals).

delete(HT) ->
    delete(HT, keys()).

delete(HT, []) ->
    HT;
delete(HT, [Key|Keys]) ->
    delete(ets_ht:del(Key, HT), Keys).

