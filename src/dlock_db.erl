%%% dlock db module

-module(dlock_db).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-define(DEFAULT_TIMEOUT, 5000).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% -include("dlock_config.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

-record(state, {
            
            % The backend module used
            module,
            
            % Client to the backend module
            client
}).

-define(SELF, self()).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run(Command) ->
    run(Command, ?DEFAULT_TIMEOUT).

run(Command, Timeout) ->
    try gen_server:call(?MODULE, Command, Timeout) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {error, Error} ->
            throw(Error)
    catch
        _:{timeout, _} ->
            throw(timeout)
    end.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    Backend = dlock_config:get(dlock, backend),
    InitArgs = dlock_config:get(Backend),
    BackendClient = Backend:start(InitArgs),
    {ok, #state{module=Backend, client=BackendClient}}.


% TODO: Write actual pattern matching commands
handle_call({Command, Args}, #state{module=Mod, client=Client} = State) ->
    Reply = Mod:Command(Args, Client),
    {reply, Reply, State};
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
