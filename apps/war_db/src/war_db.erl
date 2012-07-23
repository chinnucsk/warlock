%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB wrapper, interface for DB requests
%%%
%%% The DB module consists of a supervised worker that manages and runs
%%% commands on the specified backend
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_db).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([ping/0, reset/0, backup/1, restore/1,
         x/1]).

%% -----------------------------------------------------------------
%% Private macros
%% -----------------------------------------------------------------
-define(TIMEOUT, 1000).
-define(WORKER, war_db_worker).
-define(CALL_WORKER(Cmd), try gen_server:call(?WORKER, Cmd, ?TIMEOUT)
                          catch
                              exit:{timeout, _} -> {error, timeout}
                          end).

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Check if DB is up.
%%-------------------------------------------------------------------
-spec ping() -> pong | pang.
ping() ->
    ?CALL_WORKER(ping).

%%-------------------------------------------------------------------
%% @doc
%% Cleans db by deleting all objects
%%-------------------------------------------------------------------
-spec reset() -> ok.
reset() ->
    ?CALL_WORKER(reset).

%%-------------------------------------------------------------------
%% @doc
%% Backup db to given file
%%-------------------------------------------------------------------
-spec backup(string()) -> ok.
backup(File) ->
    ?CALL_WORKER({backup, File}).

%%-------------------------------------------------------------------
%% @doc
%% Restores db from given file
%%-------------------------------------------------------------------
-spec restore(string()) -> ok.
restore(File) ->
    ?CALL_WORKER({restore, File}).

%%-------------------------------------------------------------------
%% @doc
%% eXecute command on database
%%-------------------------------------------------------------------
-spec x(term()) -> term().
x(Cmd) ->
    ?CALL_WORKER({x, Cmd}).
