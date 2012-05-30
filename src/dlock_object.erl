%%% CURRENTLY NOT USED
%%% Container for dlock data

%% TODO: Check max key, value size?

-module(dlock_object).

-export_type([dlock_object/0, key/0, value/0]).

-type key() :: binary().
-type value() :: term().


%% Container for dlock objects
-record(d_obj, {
                key :: key(),
                value :: value()
               }).

-opaque dlock_object() :: #d_obj{}.

-export([new/2, get_key/1, get_value/1]).

new(Key, Value) when is_binary(Key) ->
    #d_obj{key = Key, value = Value}.

get_key(#d_obj{key=Key}) ->
    Key.

get_value(#d_obj{value=Value}) ->
    Value.
