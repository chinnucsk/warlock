%%% CURRENTLY NOT USED

%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server worker
%%%
%%% Manages requests and replies with the caller
%%% Uses server_command_worker for each command and waits for it to get back
%%%
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_worker).
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
-include_lib("util/include/common.hrl").

%% gen_server State
-record(state, {
}).

-define(SPAWN_CHILD(Cmd, CmdData), server_command_sup:create_worker({get_type(Cmd), CmdData})).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_arg, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(no_arg) ->
    {ok, #state{}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
%% Ping for local gen_server
handle_call(ping, _From, State) ->
    {reply, pong, State};
%% Ping check if consensus cluster is up
handle_call(ping_service, _From, State) ->
    Reply = consensus:ping(),
    {reply, Reply, State};
%% Pick check if local backend is up
handle_call(ping_backend, _From, State) ->
    Reply = db:ping(),
    {reply, Reply, State};
%% Handle command
handle_call([Command | _Data], _From, State)
  when Command == get;
       Command == set;
       Command == del ->


% TODO
%{Type, {Operation, From}=WorkerArgs}
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
