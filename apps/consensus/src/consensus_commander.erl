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
-export([start_link/1]).

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
            % The leader that spawned this commander
            leader,

            % pvalue = {Ballot number, Slot number, Proposal}
            pvalue,

            % Number of acceptors that has agreed for this ballot
            vote_count = 0
}).

-define(SELF, self()).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link({Leader, PValue}) ->
    gen_server:start_link(?MODULE, [{Leader, PValue}], []).


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
    consensus_msngr:cast(acceptors, Message),
    {ok, #state{leader = Leader,
                pvalue = PValue}}.

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
    ?LDEBUG("Received message ~p", [{p2b, {_Acceptor, ABallot}}]),
    case consensus_util:ballot_equal(ABallot, CurrBallot) of
        true ->
            case is_majority(VoteCount + 1) of
                true ->
                    Message = {decision, {Slot, Proposal}},
                    consensus_msngr:cast(replicas, Message),
                    {stop, normal, State};
                false ->
                    NewState = State#state{vote_count = VoteCount + 1},
                    {noreply, NewState}
            end;
        false ->
            % We have another ballot running. Since acceptor will not send any
            % ballot smaller than what we have, we need not check explicitly.
            % Added it just to make sure!
            case consensus_util:ballot_lesser(ABallot, CurrBallot) of
                true ->

%%                     ?LERROR("Logic error! Smaller ballot received"),
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
    VoteCount > (erlang:trunc(erlang:length(Acceptors)/2)).
