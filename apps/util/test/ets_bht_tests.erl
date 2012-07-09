%%%-------------------------------------------------------------------
%%% EUnit test for util_ht
%%%-------------------------------------------------------------------
-module(ets_bht_tests).
-include_lib("eunit/include/eunit.hrl").

%% Set, Get
set_test() ->
    HT = ets_bht:new(),
    HT1 = insert(HT),
    [?assertEqual(Value, ets_bht:keyget(Key, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())],
    [?assertEqual(Key, ets_bht:valget(Value, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% Delete
del_test() ->
    HT = ets_bht:new(),
    HT1 = delete(HT),
    [?assertNotEqual(Value, ets_bht:keyget(Key, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())],
    [?assertNotEqual(Key, ets_bht:valget(Value, HT1)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% Reset
res_test() ->
    HT = ets_bht:new(),
    HT1 = insert(HT),
    HT2 = ets_bht:reset(HT1),
    [?assertNotEqual(Value, ets_bht:keyget(Key, HT2)) ||
       {Key, Value} <- lists:zip(keys(), vals())],
    [?assertNotEqual(Key, ets_bht:valget(Value, HT2)) ||
       {Key, Value} <- lists:zip(keys(), vals())].

%% To list
list_test() ->
    HT = ets_bht:new(),
    HT1 = insert(HT),
    ListData = ets_bht:to_list(HT1),
    ?assertEqual(lists:keysort(1, ListData),
                 lists:keysort(1, lists:zip(keys(), vals()))),

    ets_bht:del(HT).

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
    insert(ets_bht:set(Key, Val, HT), Keys, Vals).

delete(HT) ->
    delete(HT, keys(), vals()).

delete(HT, [], []) ->
    HT;
delete(HT, [Key|Keys], [Val|Vals]) ->
    delete(ets_bht:del(Key, Val, HT), Keys, Vals).

