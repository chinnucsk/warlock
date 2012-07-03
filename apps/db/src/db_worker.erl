%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB worker, handles all DB requests
%%%
%%% DB worker initiates a backend module, as per setting, and handles commands
%%% targeted at that backend.
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(db_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("db.hrl").

%% gen_server State
-record(state, {
            %% The backend module used
            module,

            %% Client to the backend module
            client
}).

-define(DEFAULT_TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(Client::term()) -> {error, _} | {ok, pid()}.
start_link(Client) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Client, []).

-spec ping() -> pong | pang.
ping() ->
    gen_server:call(?MODULE, ping).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Client) ->
    Backend = conf_helper:get(backend, ?APP),
    {ok, #state{module=Backend, client=Client}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
%% eXecute command on db
handle_call({x, Cmd}, _From,
            #state{module=Backend, client=Client} = State) ->
    Reply = Backend:x(Cmd, Client),
    {reply, Reply, State};
handle_call(ping, _From,
            #state{module=Backend, client=Client} = State) ->
    Reply = Backend:ping(Client),
    {reply, Reply, State};
%% RESET db
handle_call(reset, _From,
            #state{module=Backend, client=Client} = State) ->
    {ok, _NewTable} = Backend:reset(Client),
    {reply, ok, State};
%% BACKUP db
handle_call({backup, File}, _From,
            #state{module=Backend, client=Client} = State) ->
    ok = Backend:backup(File, Client),
    {reply, ok, State};
%% RESTORE db
handle_call({restore, File}, _From,
            #state{module=Backend, client=Client} = State) ->
    {ok, _} = Backend:restore(File, Client),
    {reply, ok, State};
%% Unknown command
handle_call(_Request, _From, State) ->
    Reply = {error, unknown_command},
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:terminate/2
%% ------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% ------------------------------------------------------------------
%% gen_server:code_change/3
%% ------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
