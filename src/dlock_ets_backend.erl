%%% Backend for dlock in ets

-module(dlock_ets_backend).

-behavior(dlock_backend).

-export([start/1, get/2, put/3]).

start(InitArgs) ->
    Name = dlock_config:get(name, InitArgs),
    Options = dlock_config:get(options, InitArgs),
    ets:new(Name, Options).

put(Key, Value, Table) ->
    true = ets:insert(Table, {Key, Value}),
    ok.

get(Key, Table) ->
    ets:lookup_element(Table, Key, 2).
