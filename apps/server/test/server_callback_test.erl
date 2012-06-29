-module(server_callback_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("util/include/common.hrl").

-define(LOGLEVEL, info).

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [compiler, syntax_tools, lager, db, consensus, server].

app_start() ->
    lists:foreach (fun (App) ->
                           case application:start (App) of
                               {error, {already_started, App}} -> ok;
                               ok -> ok;
                               Other ->
                                   erlang:error ({error,
                                                  {?MODULE, ?LINE,
                                                   'could not start',
                                                   App,
                                                   'reason was', Other}})
                           end
                   end,
                   apps ()),
    error_logger:tty(false).

app_stop(_) ->
    [ ?assertEqual(ok, application:stop(App)) || App <- lists:reverse(apps())],
    error_logger:tty(true).

%%-------------------------------------------------------------------
%% test code
%%-------------------------------------------------------------------

db_test_() ->
    {timeout, 60,
     {setup,
      fun app_start/0,
      fun app_stop/1,
      [
       ?_test(simple_run())
      ]
     }}.

simple_run() ->
    lager:set_loglevel(lager_console_backend, ?LOGLEVEL),

    timer:sleep(100),

    {Key, Val} = {kkey, vval},
    {Key1, Val1} = {kkey1, vval1},

    % Set server_callback as inactive, queue requests

    server_callback:set_inactive(),

    ?assertEqual({ok, queued}, server:set(Key, Val)),
    ?assertEqual({ok, queued}, server:set(Key1, Val1)),

    ?assertEqual({ok, inactive}, server:get(Key)),
    ?assertEqual({ok, inactive}, server:get(Key1)),

    ?assertEqual({ok, queued}, server:del(Key)),

    ?assertEqual({ok, inactive}, server:get(Key)),

    % Set server_callback back to active and see if decisions are processed

    server_callback:trig_active(),
    timer:sleep(100),

    case server_callback:is_active() of
        true ->
            ?assertEqual({ok, not_found}, server:get(Key)),
            ?assertEqual({ok, Val1}, server:get(Key1)),

            ?assertEqual({ok, success}, server:del(Key1)),

            ?assertEqual({ok, not_found}, server:get(Key1));
        false ->
            ok
    end.

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
