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

            % SETEX test
            ResultSetex1 = db:x([setenx, 100, x, y]),
            ?assertEqual({ok, success}, ResultSetex1),
            timer:sleep(100),
            ResultSetex2 = db:x([get, x]),
            ?assertEqual({ok, not_found}, ResultSetex2),

            % EXPIRE test
            ResultExp1 = db:x([set, m, n]),
            ?assertEqual({ok, success}, ResultExp1),
            {ok, _} = db:x([expire, 100, m]),
            timer:sleep(101),
            ResultExp2 = db:x([get, m]),
            ?assertEqual({ok, not_found}, ResultExp2),

            % SETENX test
            ResultSetenx1 = db:x([setenx, 100, a, b]),
            ?assertEqual({ok, success}, ResultSetenx1),
            timer:sleep(50),
            ResultSetenx2 = db:x([setenx, 100, a, b]),
            ?assertEqual({ok, success}, ResultSetenx2),
            timer:sleep(50),
            ResultSetenx3 = db:x([get, a]),
            ?assertEqual({ok, b}, ResultSetenx3),
            timer:sleep(50),
            ResultSetenx4 = db:x([get, a]),
            ?assertEqual({ok, not_found}, ResultSetenx4),

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
