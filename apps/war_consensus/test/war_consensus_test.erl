%%% Consensus app test
%%%
%%% The tests are targeted for war_db_ets_backend
%%%-------------------------------------------------------------------



-module(war_consensus_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("war_util/include/war_common.hrl").

-define(LOG_LEVEL, info).

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
%% Note: We use the server app here for testing callbacks
apps() ->
    [compiler, syntax_tools, lager, war_db, war_consensus, war_server].

app_start() ->
    lists:foreach (fun (App) ->
                           case application:start (App) of
                               {error, {already_started, App}} ->
                                   ok;
                               ok ->
                                   ok;
                               Other ->
                                   erlang:error ({error,
                                                  {?MODULE, ?LINE,
                                                   'could not start',
                                                   App,
                                                   'reason was', Other}})
                           end,
                           case App of
                               lager ->
                                   lager:set_loglevel(lager_console_backend,
                                                      ?LOG_LEVEL);
                               _ ->
                                   ok
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

receive_cast(Ref) ->
    receive
        {'$gen_cast',{response, Ref, Val}} ->
            Val
    end.

consensus_test_() ->
    {timeout, 60,
     {setup,
      fun app_start/0,
      fun app_stop/1,
      [
       ?_test(simple_run())
      ]
     }}.

simple_run() ->
    % Give the system some time to start
    timer:sleep(1000),

    % Set the current node as master for the test
    Ref1 = erlang:make_ref(),
    Operation1 = #dop{type=?CLUSTER,
                     module=war_server_callback,
                     function=handle,
                     args=[set, a, b],
                     client={self(), Ref1}
                     },
    war_consensus:propose(Operation1),

    Result1 = receive_cast(Ref1),
    ?assertEqual(Result1, {ok, success}),

    Operation2 = #dop{type=?LOCAL,
                     module=war_server_callback,
                     function=handle,
                     args=[get, a],
                     client=null
                     },
    Result2 = war_consensus:propose(Operation2),

    ?assertEqual(Result2, {ok, b}).
