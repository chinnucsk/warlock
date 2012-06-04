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
-export([start_link/0, ping/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/config.hrl").

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_arg, []).

ping() ->
    gen_server:call(?MODULE, ping).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(no_arg) ->
    Backend = conf_helper:get(backend, ?APP),
    {ok, Client} = Backend:start(),
    {ok, #state{module=Backend, client=Client}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
%% Check if the client is up
%%TODO: is the name "Client" appropriate here, or server
handle_call(ping, _From, 
            #state{module=Backend, client=Client} = State) ->
    Reply = Backend:ping(Client),
    {reply, Reply, State};
%% GET object
handle_call({get, Key}, _From, 
            #state{module=Backend, client=Client} = State) ->
    Reply = Backend:get(Key, Client),
    {reply, Reply, State};
%% PUT value for given key
handle_call({put, {Key, Value}}, _From, 
            #state{module=Backend, client=Client} = State) ->
    Reply = Backend:put(Key, Value, Client),
    {reply, Reply, State};
%% DELETE object
handle_call({delete, Key}, _From, 
            #state{module=Backend, client=Client} = State) ->
    Reply = Backend:delete(Key, Client),
    {reply, Reply, State};
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
