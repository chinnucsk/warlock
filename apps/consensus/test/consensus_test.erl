-module(consensus_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("server/include/server.hrl").

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [compiler, syntax_tools, lager, db, consensus].

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
    consensus_state:set_master({node(), {123, 123, 123}}),
%%     consensus_msngr:cast(leader, {propose, {1, 2}}),

    Operation1 = #dop{type=write,
                     module=db,
                     function=set,
                     args=[kkey, vval],
                     client=self()
                     },
    consensus_client:propose(Operation1),
    Operation2 = #dop{type=write,
                     module=db,
                     function=get,
                     args=kkey,
                     client=self()
                     },
    consensus_client:propose(Operation2),


    timer:sleep(2000),



    ?assertNotEqual(1, 2).

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
