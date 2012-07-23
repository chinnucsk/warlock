%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server worker
%%%
%%% Server worker creates a timer and a unique reference for every command.
%%% Timeouts need to be greater than that of the consensus module (commander).
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/0, start_link/0, request/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").

-record(state, {
                %% Keeps track of all the requests
                requests
}).

%% How long the process waits for the reply from consensus module
-define(TIMEOUT, 5000).
-define(CALL_TIMEOUT, 6000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start() -> {error, _} | {ok, pid()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    {ok, #state{requests=ets_ht:new()}}.

request(?LOCAL, Cmd) ->
    consensus:propose(get_operation(?LOCAL, Cmd, null));
request(?CLUSTER, Cmd) ->
    gen_server:call(?MODULE, {request, {?CLUSTER, Cmd}}, ?CALL_TIMEOUT).

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
%% Request is sent by server
handle_call({request, {Type, Cmd}}, From, #state{requests=Requests}=State) ->
    % TODO: Better timer implementation for more performance
    TRef = erlang:start_timer(?TIMEOUT, self(), ?MODULE),
    ets_ht:set(TRef, From, Requests),
    Operation = get_operation(Type, Cmd, {self(), TRef}),
    consensus:propose(Operation),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% Response is sent by consensus client
handle_cast({response, TRef, Result}, #state{requests=Requests} = State) ->
    % Reply to the original caller
    case ets_ht:get(TRef, Requests) of
        not_found ->
            ?LINFO("SERW::Error, req not found");
        Caller ->
            gen_server:reply(Caller, Result),
            erlang:cancel_timer(TRef),
            ets_ht:del(TRef, Requests)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info({timeout, TRef, ?MODULE},#state{requests=Requests}=State) ->
    ?LINFO("SERW::timeout"),
    case ets_ht:get(TRef, Requests) of
        not_found ->
            ?LINFO("Error, timeout req not found");
        Caller ->
            gen_server:reply(Caller, {error, timeout}),
            ets_ht:del(TRef, Requests)
    end,
    {noreply, State};
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
get_operation(Type, Cmd, ClientId) ->
    #dop{type = Type,
         module = server_callback,
         function = handle,
         args = Cmd,
         client = ClientId}.
