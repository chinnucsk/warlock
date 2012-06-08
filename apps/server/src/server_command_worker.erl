%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server Command worker
%%%
%%% With respect to "Paxos made moderately complex", we do not have separate
%%% client id and operation id since the pid of this process defines both
%%% uniquely
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Add retry logic? (in case of timeout)
-module(server_command_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/config.hrl").

-record(state, {
            % The operation that is to be performed on the server
            % Currently not being used
            % TODO: Remove if not needed
            operation,

            % PID of the process calling the client
            % We reply to this once we get a response for the server
            caller
}).

%% How long the process waits for the reply from consensus module
-define(TIMEOUT, 2000).

%% Client id: To be sent as part of the request
-define(CID, self()).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    {ok, #state{}, ?TIMEOUT}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
%% Request is sent by server
%% TODO: Responsible for executing one command. Make sure it does not
%% accept other commands
handle_call({request, {Cmd, Data}}, From, _State) ->
    Operation = get_operation({Cmd, Data, ?CID}),
    consensus_client:propose(Operation),
    {noreply, #state{operation=Operation, caller=From}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% Response is sent by consensus client
handle_cast({response, Result}, #state{caller=Caller} = State) ->
    % Reply to the original caller
    gen_server:reply(Caller, Result),
    % Shutdown normally
    {stop, normal, State};
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

get_operation({Cmd, Data, ClientId}) ->
    #dop{
         type = server_util:get_type(Cmd),
         module = server_callback,
         function = handle,
         args = [Cmd, Data],
         client = ClientId
         }.
