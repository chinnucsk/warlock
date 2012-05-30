%%% Helper functions to get config params

-module(dlock_config).

-export([get/1, get/2, get/3, get_prop/3, get_prop/4]).

get(App) ->
    application:get_all_env(App).

get(App, Key) ->
    get(App, Key, undefined).

get(App, Key, Default) ->
    application:get_env(App, Key, Default).

get_prop(Key, Proerties, App) ->
   get_prop(Key, Proerties, App, undefined).

get_prop(Key, Properties, App, Default) ->
    case proplists:get_value(Key, Properties, Default) of
        undefined ->
            get(App, Key, Default);
        Value ->
            Value
    end.