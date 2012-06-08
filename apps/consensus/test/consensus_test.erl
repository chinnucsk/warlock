-module(consensus_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("util/include/config.hrl").

% TODO: Have operations independant from db

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [compiler, syntax_tools, lager, consensus].

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
    lager:set_loglevel(lager_console_backend, info),


    consensus_state:set_master({node(), {123, 123, 123}}),

    Operation1 = #dop{type=write,
                     module=lists,
                     function=min,
                     args=[1, 2, 3, 4],
                     client=self()
                     },
    consensus_client:propose(Operation1),

    Operation2 = #dop{type=write,
                     module=lists,
                     function=max,
                     args=[1, 2, 3, 4],
                     client=self()
                     },
    consensus_client:propose(Operation2),

    timer:sleep(1000),
    ?assertNotEqual(1, 2).

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
