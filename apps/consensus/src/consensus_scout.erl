%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Scout
%%%
%%% Paxos scout
%%%
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_scout).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

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

            % Set of pvalues where
            % pvalue = {Ballot number, Slot number, Proposal}
            pvalues = orddict:new(),

            % Ballot number
            ballot_num,

            % Number of acceptors that has agreed for this ballot
            vote_count = 0
}).

-define(SELF, self()).
%% Time allowed for the scout to survive being idle
-define(TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link({Leader, Ballot}) ->
    gen_server:start_link(?MODULE, [{Leader, Ballot}], [{timeout, ?TIMEOUT}]).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([{Leader, Ballot}]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),

    % Send a message to  all the acceptors and wait for their response
    Message = {p1a, {?SELF, Ballot}},
    ?ASYNC_MSG(acceptors, Message),
    {ok, #state{leader = Leader,
                ballot_num = Ballot}, ?TIMEOUT}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% phase 1 b message from some acceptor
% TODO: We currently do not check the acceptor identity. Add check to make sure
% votes are only counted for unique acceptors
handle_cast({p1b, {_Acceptor, ABallot, APValues}},
            #state{ballot_num = CurrBallot,
                   vote_count = VoteCount,
                   leader = Leader,
                   pvalues = PValues} = State) ->
    ?LDEBUG("Received message ~p", [{p1b, {_Acceptor, ABallot, APValues}}]),
    case consensus_util:ballot_same(ABallot, CurrBallot) of
        true ->
            NewPValues = merge_pvalues(APValues, PValues),
            case consensus_util:is_majority(VoteCount + 1) of
                true ->
                    Message = {adopted, {ABallot, orddict:to_list(NewPValues)}},
                    ?ASYNC_MSG(Leader, Message),
                    {stop, normal, State};
                false ->
                    NewState = State#state{pvalues = NewPValues,
                                           vote_count = VoteCount + 1},
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
handle_info(timeout,#state{leader = Leader}=State) ->
    ?ASYNC_MSG(Leader, scout_timeout),
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
%% Makes sure only the highest ballot for a slot exists
merge_pvalues([], PValues) ->
    PValues;
merge_pvalues([{Key, {Ballot, _Proposal}=Val} | APValues], PValues) ->
    PValue = (catch orddict:fetch(Key, PValues)),
    case PValue of
        % Object not in dict
        {'EXIT', _} ->
            orddict:store(Key, Val, PValues);
        {Key, {CBallot, _CProposal}} ->
            case consensus_util:ballot_greater(Ballot, CBallot) of
                % New ballot is larger, replace current
                true ->
                    orddict:store(Key, Val, PValues);
                % Nothing to change
                false ->
                    PValues
            end
    end,
    merge_pvalues(APValues, PValues).