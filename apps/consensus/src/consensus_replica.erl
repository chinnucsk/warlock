%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Replica
%%%
%%% Paxos replica
%%%
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_replica).
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
-include("consensus.hrl").

-record(state, {
            % Replicated transaction log
            % The log is appended once consensus is reached
            % Implemented as a simple hash table
            % TODO: Change to ordered set to make iteration easier?
            tlog = util_ht:new(),

            % Slot is the index of next item in tlog (for new decisions)
            slot_num = 1,

            % Minimum available open slot number in the set (for new proposals)
            % (Proposals U Decisions)
            min_slot_num = 1,

            % A set of {slot number, command} pairs for proposals that the
            % replica has made in the past
            % Implemented as a bidirectional hash table (BHT)
            proposals = util_bht:new(),

            % Another set of {slot number, command} pairs for decided slots
            % Implemented as a bidirectional hash table
            % TODO: It is possible to use proposals BHT itself in combination
            % with slot_num for decisions. Optimize it.
            decisions = util_bht:new()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),
    {ok, #state{}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
% READ request sent from client
handle_cast({request, #dop{type=read}=Proposal}, State) ->
    ?LDEBUG("REP ~p::Received read message ~p", [self(), {request, Proposal}]),
    % Execute the decision directly
    ?CLIENT:exec(Proposal),
    {noreply, State};
% WRITE/Other request sent from client
handle_cast({request, #dop{}=Proposal}, State) ->
    ?LDEBUG("REP ~p::Received message ~p", [self(), {request, Proposal}]),
    NewState = propose(Proposal, State),
    {noreply, NewState};
% Reconfiguration requests
handle_cast({request, #rop{type=Type}=Proposal}, State) ->
    ?LDEBUG("REP ~p::Received message ~p", [self(), {request, Proposal}]),
    Slot = consensus_rcfg:get_slot(Type),
    Message = {propose_rcfg, {Slot, Proposal}},
    % TODO: Make this configurable
    ?ASYNC_MSG(master_leader, Message),
    {noreply, State};

% Handle reconfiguration decision
handle_cast({decision_rcfg, Proposal}, State) ->
    ?LDEBUG("REP ~p::Received message ~p", [self(),
                                            {decision_rcfg, Proposal}]),

    %% Decision is for reconfiguration. Bypass rest of the logic and execute
    %% We also don't cleanup this slot since we don't store it anywhere
    ?CLIENT:exec(Proposal),
    {noreply, State};
% Handle leader's decision
handle_cast({decision, {Slot, Proposal}},
            #state{decisions = Decisions} = State) ->
    ?LDEBUG("REP ~p::Received message ~p", [self(),
                                            {decision, {Slot, Proposal}}]),
    % Add the decision to the set of decisions

    % A safety check to see if this is a duplicate decision
    % TODO: If someone is re-proposing a request that is in decision, should we
    % reply to them?
    case util_bht:valget(Proposal, Decisions) of
        not_found ->
            % Save the decision
            util_bht:set(Slot, Proposal, Decisions),
            % Go through decisions and update state if needed
            % TODO: check_decisions could take arbitrarily long time. Spawn
            % helper process? Perhaps blocking is essential for algo. Check
            NewState = check_decisions(State),
            {noreply, NewState};
        _ ->
            {noreply, State}
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
%% Proposes a new operation to the leaders
propose(Proposal, #state{proposals = Proposals,
                         decisions = Decisions,
                         min_slot_num = MinSlot} = State) ->
    % Check if there has been a decision of this proposal
    case util_bht:valget(Proposal, Decisions) of
        % We haven't seen this proposal before
        % Get the next available slot number, add it to proposals and
        % Let master know about it
        not_found ->
            util_bht:set(MinSlot, Proposal, Proposals),
            Message = {propose, {MinSlot, Proposal}},
            % TODO: Make this configurable
            ?ASYNC_MSG(master_leader, Message),
            State#state{min_slot_num = MinSlot + 1};
        % If already decided, ignore it
        _ ->
            State
    end.

%% Executes the decision for the specified slot
perform(Proposal, #state{slot_num = Slot,
                         proposals = Proposals,
                         decisions = Decisions,
                         tlog = TLog} = State) ->
    % Add a new entry in transaction log
    util_ht:set(Slot, Proposal, TLog),
    % Let the consensus client handle the callback execution
    ?CLIENT:exec(Proposal),
    % Perform cleanup functions
    % Once the slot is filled, all actors no longer need to maintain
    % data for that slot
    util_bht:del(Slot, Proposal, Proposals),
    % We use Decisions map for managing concurrent proposals and to
    % avoid duplicates (multiple leaders)
    % Since the current design allows for atmost 1 leader, we can cleanup
    % Decisions
    util_bht:del(Slot, Proposal, Decisions),
    ?ASYNC_MSG(?LEADER, {slot_decision, Slot}),
    % Cleaning up acceptor directly works here becase we maintain 2N+1
    % replicas instead of just N+1 (as in paper)
    ?ASYNC_MSG(?ACCEPTOR, {slot_decision, Slot}),
    % Move to the next slot
    State#state{slot_num = Slot + 1}.

%% Check if we have any decisions that can be run
check_decisions(#state{proposals = Proposals,
                       decisions = Decisions,
                       slot_num = CurrSlot} = State) ->
    % Check if we have a decision for the current slot
    case util_bht:keyget(CurrSlot, Decisions) of
        % No decision for the current slot, nothing to change
        not_found ->
            State;
        % We have a decision for the current slot
        CurrDecision ->
            % Check if we have a proposal for this slot
            case util_bht:keyget(CurrSlot, Proposals) of
                % We dont have a proposal for this slot
                % Execute the decision for this slot and continue
                not_found ->
                    NewState = perform(CurrDecision, State),
                    check_decisions(NewState);
                % We have a proposal and it is the same as the decision
                % Execute the decision for this slot and continue
                CurrDecision ->
                    NewState = perform(CurrDecision, State),
                    check_decisions(NewState);
                % We have a proposal, but it is different from the decision
                AltProposal ->
                    % Re-propose it
                    UpdState = propose(AltProposal, State),
                    % Execute the decision
                    NewState = perform(CurrDecision, UpdState),
                    check_decisions(NewState)
            end
    end.