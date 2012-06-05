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
-record(state, {
            % The leader that spawned this commander
            leader,

            % Set of pvalues where
            % pvalue = {Ballot number, Slot number, Proposal}
            pvalues = sets:new(),

            % Ballot number
            ballot_num,

            % Number of acceptors that has agreed for this ballot
            vote_count
}).

-define(SELF, self()).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link({Leader, Ballot}) ->
    gen_server:start_link(?MODULE, [{Leader, Ballot}], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([{Leader, Ballot}]) ->
    % Send a message to  all the acceptors and wait for their response
    Message = {p1a, {?SELF, Ballot}},
    consensus_msngr:cast(acceptors, Message),
    {ok, #state{leader = Leader,
                ballot_num = Ballot}}.

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
    case consensus_util:ballot_equal(ABallot, CurrBallot) of
        true ->
            NewPValues = sets:union(APValues, PValues),
            case is_majority(VoteCount) of
                true ->
                    Message = {adopted, {ABallot, NewPValues}},
                    consensus_msngr:cast(Leader, Message),
                    {stop, normal, State};
                false ->
                    NewState = State#state{pvalues = NewPValues,
                                           vote_count = VoteCount + 1},
                    {noreply, NewState}
            end;
        false ->
            % We have another ballot running. Since acceptor will not send any
            % ballot smaller than what we have, we need not check explicitly.
            % Added it just to make sure!
            case consensus_util:ballot_lesser(ABallot, CurrBallot) of
                true ->
                    % TODO: Some issue with reason for init. Disable check for now
%%                     ?LERROR("Logic error! Smaller ballot received, ~p ~p", [ABallot, CurrBallot]),
%%                     {stop, logic_error, State};
                    {noreply, State};
                false ->
                    % We have a larger ballot; inform leader and exit
                    consensus_msngr:cast(Leader, {preempted, ABallot}),
                    {stop, normal, State}
            end
    end;
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

% Votes > (N/2 + 1)
is_majority(VoteCount) ->
    Acceptors = consensus_state:get_members(),
    VoteCount > (erlang:trunc(erlang:length(Acceptors)/2) + 1).
