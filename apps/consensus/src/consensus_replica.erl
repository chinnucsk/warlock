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
%% TODO: Store view in local state and ignore decisions from old views
-module(consensus_replica).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, reset/0]).

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
            % Slot is the index of next item (for new decisions)
            slot_num = 1,

            % Minimum available open slot number in the set (for new proposals)
            % (Proposals U Decisions)
            min_slot_num = 1,

            % Type of hash_table used
            hash_table,

            % A set of {slot number, command} pairs for proposals that the
            % replica has made in the past
            % Implemented as a bidirectional hash table (BHT)
            proposals,

            % Another set of {slot number, command} pairs for decided slots
            % Implemented as a bidirectional hash table
            % TODO: It is possible to use proposals BHT itself in combination
            % with slot_num for decisions. Optimize it.
            decisions
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec reset() -> ok.
reset() ->
    gen_server:cast(?MODULE, reset).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),
    HT = conf_helper:get(bht, int_hash_table),
    Options = conf_helper:get(bht_options, int_hash_table),
    {ok, #state{hash_table=HT,
                proposals=HT:new(Options),
                decisions=HT:new(Options)}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
%% Returns proplist of state
handle_call(debug, _From, State) ->
    Reply = lists:zip(record_info(fields, state), tl(tuple_to_list(State))),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
% READ request sent from client
handle_cast({request, #dop{type=?LOCAL}=Proposal}, State) ->
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
% Note: Requests forwarded only to local leader since it is broadcast
handle_cast({request, #rop{type=Type}=Proposal}, State) ->
    ?LDEBUG("REP ~p::Received message ~p", [self(), {request, Proposal}]),
    Slot = consensus_rcfg:get_slot(Type),
    Message = {propose_rcfg, {Slot, Proposal}},
    ?ASYNC_MSG(?LEADER, Message),
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
            #state{hash_table=HT,decisions = Decisions} = State) ->
    ?LDEBUG("REP ~p::Received message ~p", [self(),
                                            {decision, {Slot, Proposal}}]),
    % Add the decision to the set of decisions

    % A safety check to see if this is a duplicate decision
    case HT:valget(Proposal, Decisions) of
        not_found ->
            % Save the decision
            Decisions1 = HT:set(Slot, Proposal, Decisions),
            % Go through decisions and update state if needed
            % TODO: check_decisions could take arbitrarily long time. Spawn
            % helper process? Perhaps blocking is essential for algo. Check
            NewState = check_decisions(State#state{decisions=Decisions1}),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
handle_cast(reset, #state{hash_table=HT,
                          proposals=Proposals,
                          decisions = Decisions}) ->
    {noreply, #state{proposals=HT:reset(Proposals),
                     decisions=HT:reset(Decisions)}};
%% Unknown message
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
propose(Proposal, #state{hash_table=HT,
                         proposals = Proposals,
                         decisions = Decisions,
                         min_slot_num = MinSlot} = State) ->
    % Check if there has been a decision of this proposal
    case HT:valget(Proposal, Decisions) of
        % We haven't seen this proposal before
        % Get the next available slot number, add it to proposals and
        % Let master know about it
        not_found ->
            Proposals1 = HT:set(MinSlot, Proposal, Proposals),
            Message = {propose, {MinSlot, Proposal}},
            ?ASYNC_MSG(master_leader, Message),
            State#state{min_slot_num = MinSlot + 1,
                        proposals=Proposals1};
        % If already decided, ignore it
        _ ->
            State
    end.

%% Executes the decision for the specified slot
perform(Proposal, #state{hash_table=HT,
                         slot_num = Slot,
                         min_slot_num = MinSlot,
                         proposals = Proposals,
                         decisions = Decisions} = State) ->
    % Let the consensus client handle the callback execution
    ?CLIENT:exec(Proposal),
    % Perform cleanup functions
    % Once the slot is filled, all actors no longer need to maintain
    % data for that slot
    Proposals1 = HT:del(Slot, Proposal, Proposals),
    % We use Decisions map for managing concurrent proposals and to
    % avoid duplicates (multiple leaders)
    % Since the current design allows for atmost 1 leader, we can cleanup
    % Decisions
    Decisions1 = HT:del(Slot, Proposal, Decisions),
    ?ASYNC_MSG(?LEADER, {slot_decision, Slot}),
    % Cleaning up acceptor directly works here becase we maintain 2N+1
    % replicas instead of just N+1 (as in paper)
    ?ASYNC_MSG(?ACCEPTOR, {slot_decision, Slot}),
    % Move to the next slot
    NewSlot = Slot + 1,
    % When not master, MinSlot lags. This is the fix
    NewMinSlot = case MinSlot < NewSlot of
        true ->
            NewSlot;
        false ->
            MinSlot
    end,
    State#state{slot_num=NewMinSlot,
                min_slot_num=NewMinSlot,
                proposals=Proposals1,
                decisions=Decisions1}.

%% Check if we have any decisions that can be run
check_decisions(#state{hash_table=HT,
                       proposals = Proposals,
                       decisions = Decisions,
                       slot_num = CurrSlot} = State) ->
    % Check if we have a decision for the current slot
    case HT:keyget(CurrSlot, Decisions) of
        % No decision for the current slot, nothing to change
        not_found ->
            State;
        % We have a decision for the current slot
        CurrDecision ->
            % Check if we have a proposal for this slot
            case HT:keyget(CurrSlot, Proposals) of
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