%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus client
%%%
%%% As a Paxos client, it is responsible for sending the first request
%%% to the replicas.
%%% In this modified version, we only send a message to the local replica.
%%% Another modification is that the response is sent directly to the caller
%%% by the local client on the master node.
%%%
%%% Uses gen_server+ets for synchronous rcfg requests
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_client).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/0, start_link/0,
         propose/1, propose_rcfg/1, sync_propose_rcfg/1,
         exec/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

-record(state, {
                %% Keeps track of all the requests
                requests
}).

% Wait for 10 minutes before timing out a sync rcfg request
-define(TIMEOUT, 10 * 60 * 1000).

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
%% Send read request to the master replica
%% Note: Here we are assuming Operation is uniquely identified
-spec propose(#dop{}) -> ok.
propose(#dop{type=?LOCAL}=Operation) ->
    %% Check if lease is valid, if yes, execute on local replica
    LeaseTime = consensus_state:get_lease_validity(),
    case (LeaseTime > ?MIN_LEASE) of
        true ->
            ?MODULE:exec(Operation);
        false ->
            {error, out_of_sync}
    end;
%% Send other requests to the local replica
propose(Operation) ->
    Msg = {request, Operation},
    ?ASYNC_MSG(?REPLICA, Msg).

%% Send the request to all replicas
%% Only the ones with active leaders will succeed, rest are ignored
%% Paxos will handle multiple active leaders using ballots to make sure
%% only one master exists
-spec propose_rcfg(#rop{}) -> ok.
propose_rcfg(Operation) ->
    Msg = {request, Operation},
    ?ASYNC_MSG(replicas, Msg).

%% Execute the callback function in the operation
%% Called by the replica
-spec exec(#dop{} | #rop{}) -> ok.
exec(#dop{type = Type,
          module = M,
          function = F,
          args = A,
          client = {Client, Ref}}) ->
    ?LDEBUG("Executing operation ~p:~p(~p)", [M, F, A]),

    Result = M:F(Type, A),
    ?LDEBUG("RESULT ==>> ~p", [Result]),

    case Type of
        ?LOCAL ->
            ?ASYNC_MSG(Client, {response, Ref, Result});
        ?CLUSTER ->
            case consensus_state:is_master() of
                true ->
                    ?ASYNC_MSG(Client, {response, Ref, Result});
                false ->
                    ok
            end
    end,
    Result;
exec(#rop{client=undefined}=ROp) ->
    consensus_rcfg:callback(ROp);
exec(#rop{}=ROp) ->
    {Client, TRef} = ROp#rop.client,

    % Execute rcfg on all nodes
    Result = consensus_rcfg:callback(ROp),

    % If master, clear timer on the caller
    case consensus_state:is_master() of
        true ->
            ?ASYNC_MSG(Client, {response, TRef, Result});
        false ->
            ok
    end.

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
%% Initialize gen_server
%% ------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{requests=ets_ht:new()}}.

%% ------------------------------------------------------------------
%% Sync requests
%% ------------------------------------------------------------------
-spec sync_propose_rcfg(#rop{}) -> ok.
sync_propose_rcfg(Operation) ->
    gen_server:call(?MODULE, {sync_propose_rcfg, Operation}, infinity).

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call({sync_propose_rcfg, Operation}, From, #state{requests=Requests}=State) ->
    TRef = erlang:start_timer(?TIMEOUT, self(), ?MODULE),
    ets_ht:set(TRef, From, Requests),
    NewOp = Operation#rop{client={self(), TRef}},
    propose_rcfg(NewOp),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
handle_cast({response, TRef, Result}, #state{requests=Requests} = State) ->
    % Reply to the original caller
    case ets_ht:get(TRef, Requests) of
        not_found ->
            ?LINFO("CONC::Error, req not found");
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
    ?LINFO("CONC::timeout"),
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
%% Internal functions
%% ------------------------------------------------------------------
