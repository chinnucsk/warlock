-module(war_server_callback_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("war_util/include/war_common.hrl").

-define(LOGLEVEL, info).

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [compiler, syntax_tools, lager, war_db, war_consensus, war_server].

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

war_server_callback_test_() ->
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

    % Set war_server_callback as inactive, queue requests

    war_server_callback:set_inactive(),

    ?assertEqual({ok, queued}, war_server:x(?CLUSTER, [set, Key, Val])),
    ?assertEqual({ok, queued}, war_server:x(?CLUSTER, [set, Key1, Val1])),

    ?assertEqual({ok, not_found}, war_server:x(?LOCAL, [get, Key])),
    ?assertEqual({ok, not_found}, war_server:x(?LOCAL, [get, Key1])),

    ?assertEqual({ok, queued}, war_server:x(?CLUSTER, [del, Key])),

    % Set war_server_callback back to active and see if decisions are processed

    war_server_callback:trig_active(),
    timer:sleep(100),

    case war_server_callback:is_active() of
        true ->
            ?assertEqual({ok, not_found}, war_server:x(?LOCAL, [get, Key])),
            ?assertEqual({ok, Val1}, war_server:x(?LOCAL, [get, Key1])),

            ?assertEqual({ok, success}, war_server:x(?CLUSTER, [del, Key1])),

            ?assertEqual({ok, not_found}, war_server:x(?LOCAL, [get, Key1]));
        false ->
            ok
    end.

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
