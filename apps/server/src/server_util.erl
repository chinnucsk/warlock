%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Utility functions
%%%
%%% The Server module consists of a supervised worker that runs
%%% commands on the local DB as per consensus
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_util).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export().

%%TODO

%% -----------------------------------------------------------------
%% Private macros
%% -----------------------------------------------------------------
-define(WORKER, server_worker).
-define(CALL_WORKER(Cmd), try gen_server:call(?WORKER, Cmd)
                          catch
                              exit:{timeout, _} -> {error, timeout}
                          end).
-define(CAST_WORKER(Cmd), gen_server:cast(?WORKER, Cmd)).

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Check if the server is up.
%%-------------------------------------------------------------------
-spec ping() -> pong | pang.
ping() ->
    ?CALL_WORKER(ping).

%%-------------------------------------------------------------------
%% @doc
%% Get a value from the DB.
%%-------------------------------------------------------------------
get(Key) ->
    ?CALL_WORKER({get, Key}).
    
%%-------------------------------------------------------------------
%% @doc
%% Store an object in the database.
%%-------------------------------------------------------------------
put(Key, Value) ->
    ?CALL_WORKER({put, {Key, Value}}).

%%-------------------------------------------------------------------
%% @doc
%% Deletes object with given key from the database.
%%-------------------------------------------------------------------
delete(Key) ->
    ?CALL_WORKER({delete, Key}).
