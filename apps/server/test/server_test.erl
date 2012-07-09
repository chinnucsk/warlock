-module(server_test).

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

server_test_() ->
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
    ?assertEqual({ok, success}, server:x(?CLUSTER, [set, Key, Val])),

    ?assertEqual({ok, Val}, server:x(?LOCAL, [get, Key])),

    ?assertEqual({ok, success}, server:x(?CLUSTER, [del, Key])),

    ?assertEqual({ok, not_found}, server:x(?LOCAL, [get, Key])).


%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
