%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Commander
%%%
%%% Paxos commander
%%%
%%% @end
%%%
%%% @since : 06 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_commander).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/1, start_link/1]).

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
            % The leader that spawned this commander
            leader,

            % pvalue = {Ballot number, Slot number, Proposal}
            pvalue,

            % Number of acceptors that has agreed for this ballot
            vote_count = 0
}).

-define(SELF, self()).
%% Time allowed for the scout to survive being idle
-define(TIMEOUT, 1000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link({node(), pvalue()}) -> {error, _} | {ok, pid()}.
start_link({Leader, PValue}) ->
    gen_server:start_link(?MODULE, [{Leader, PValue}], [{timeout, ?TIMEOUT}]).

-spec start({node(), pvalue()}) -> {error, _} | {ok, pid()}.
start({Leader, PValue}) ->
    gen_server:start(?MODULE, [{Leader, PValue}], [{timeout, ?TIMEOUT}]).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([{Leader, PValue}]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),

    % Send a message to  all the acceptors and wait for their response
    Message = {p2a, {?SELF, PValue}},
    ?ASYNC_MSG(acceptors, Message),
    {ok, #state{leader = Leader,
                pvalue = PValue}, ?TIMEOUT}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% phase 2 b message from some acceptor
% TODO: We currently do not check the acceptor identity. Add check to make sure
% votes are only counted for unique acceptors
handle_cast({p2b, {_Acceptor, ABallot}},
            #state{pvalue = {CurrBallot, Slot, Proposal},
                   vote_count = VoteCount,
                   leader = Leader} = State) ->
    ?LDEBUG("COM ~p::Received message ~p", [self(), {p2b, {_Acceptor, ABallot}}]),
    case consensus_util:ballot_same(ABallot, CurrBallot) of
        true ->
            case consensus_util:is_majority(VoteCount + 1) of
                true ->
                    exec_decision(Slot, Proposal),
                    {stop, normal, State};
                false ->
                    NewState = State#state{vote_count = VoteCount + 1},
                    {noreply, NewState, ?TIMEOUT}
            end;
        false ->
            % We have another ballot running. Since acceptor will not send any
            % ballot smaller than ours, inform leader and exit
            ?ASYNC_MSG(Leader, {preempted, ABallot}),
            {stop, normal, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info(timeout,#state{leader = Leader, pvalue =PValue}=State) ->
    ?ASYNC_MSG(Leader, {commander_timeout, PValue}),
    {stop,normal,State};
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
exec_decision(Slot, #dop{}=Proposal) ->
    Msg = {decision, {Slot, Proposal}},
    ?ASYNC_MSG(replicas, Msg);
exec_decision(_Slot, #rop{}=Proposal) ->
    Msg = {decision_rcfg, Proposal},
    ?ASYNC_MSG(replicas, Msg).
