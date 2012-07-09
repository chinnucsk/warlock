-module(db_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("util/include/common.hrl").

%%-------------------------------------------------------------------
%% setup code
%%-------------------------------------------------------------------
apps() ->
    [compiler, syntax_tools, lager, db].

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
    Backend = conf_helper:get(backend, ?APP),

    case Backend of
        db_ets_backend ->
            ?assertEqual(pong, db:ping()),

            Keys = keys(),
            Vals = vals(),

            % General test
            insert_mult(Keys, Vals),

            DBVals = get_mult(Keys),
            ?assertEqual(Vals, DBVals),

            delete_mult(Keys),
            DDBVals = get_mult(Keys),
            ?assertNotEqual(Vals, DDBVals),

            % SetNX test
            ResultSetnx1 = db:x([setnx, a, b]),
            ?assertEqual({ok, success}, ResultSetnx1),
            ResultSetnx2 = db:x([setnx, a, b]),
            ?assertEqual({ok, not_set}, ResultSetnx2),

            % Reset test
            insert_mult(Keys, Vals),
            db:reset(),
            ResetDBVals = get_mult(Keys),
            ?assertNotEqual(Vals, ResetDBVals),

            % Timer test
            ResultTimer1 = db:x([setenx, 1000, a, b]),
            ?assertEqual(ResultTimer1, {ok, success}),
            timer:sleep(500),
            ResultTimer2 = db:x([setenx, 1000, a, b]),
            ?assertEqual(ResultTimer2, {ok, success}),
            timer:sleep(500),
            ResultTimer3 = db:x([get, a]),
            ?assertEqual(ResultTimer3, {ok, b}),
            timer:sleep(500),
            ResultTimer4 = db:x([get, a]),
            ?assertEqual(ResultTimer4, {ok, not_found}),

            % Backup test
            insert_mult(Keys, Vals),
            File = "./tmp-db-test",
            db:backup(File),
            {Result, _ResultData} = file:read_file_info(File),
            ?assertEqual(ok, Result),

            % Clean table and restore from file
            db:reset(),
            db:restore(File),
            RestoreDBVals = get_mult(Keys),
            ?assertEqual(Vals, RestoreDBVals),

            file:delete(File);
        _ ->
            %% TODO: Write tests for redis backend
            ok
    end.

%%-------------------------------------------------------------------
%% internal functions
%%-------------------------------------------------------------------
keys() ->
    [key, {bigger_term, {some_values, [1, 2, 3, {a, b}]}}, 1].

vals() ->
    [value, {big_term_val}, 2].

insert_mult([], []) ->
    ok;
insert_mult([Key | KTail], [Val | VTail]) ->
    db:x([set, Key, Val]),
    insert_mult(KTail, VTail).


get_mult(Keys) ->
    lists:reverse(get_mult(Keys, [])).

get_mult([], Acc) ->
    Acc;
get_mult([Key | Tail], Acc) ->
    {ok, Val} = db:x([get, Key]),
    get_mult(Tail, [Val | Acc]).

delete_mult([]) ->
    ok;
delete_mult([Key | Tail]) ->
    db:x([del, Key]),
    delete_mult(Tail).
