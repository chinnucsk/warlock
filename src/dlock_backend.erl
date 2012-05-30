%%% dlock backend behaviour

-module(dlock_backend).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start, 1},
     {get, 2},                  % {Key, Db}
     {put, 3}];                 % {Key, Value, Db}
behaviour_info(_Other) ->
    undefined.