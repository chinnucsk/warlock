%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Leader
%%%
%%% Paxos leader
%%%
%%% @end
%%%
%%% @since : 06 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_leader).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, reset/0, incr_view/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% -------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

-define(SELF, self()).
%% Ballot format {view number, incrementing id, unique leader id}
-define(FIRST_BALLOT, {1, 0, ?SELF}).
%% When the leader is preempted => there is a leader with higher ballot. In
%% order to allow that leader to progress, we can wait for below time.
%% Should ideally be
%% http://en.wikipedia.org/wiki/Additive_increase/multiplicative_decrease
-define(BACKOFF_TIME, 100).  % In milli seconds

-record(state, {
            % Monotonically increasing unique ballot number
            ballot_num = ?FIRST_BALLOT,

            % State of the leader
            active = false,

            % A map of slot numbers to proposals in the form of a set
            % At any time, there is at most one entry per slot number in the set
            proposals = util_ht:new()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Reset the leaders local state
-spec reset() -> ok.
reset() ->
    gen_server:cast(?MODULE, reset).

%% Increment the leader's view by changing the ballot and restarting election
-spec incr_view() -> ok.
incr_view() ->
    gen_server:cast(?MODULE, incr_view).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),
    % Start scout
    consensus_scout_sup:create({?SELF, ?FIRST_BALLOT}),
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
%% propose request from replica
handle_cast({propose, {Slot, Proposal}},
            #state{proposals = Proposals,
                   active = Active,
                   ballot_num = Ballot} = State) ->
    ?LDEBUG("LEA ~p::Received message ~p", [self(),
                                            {propose, {Slot, Proposal}}]),
    % Add the proposal if we do not have a command for the proposed spot
    case util_ht:get(Slot, Proposals) of
        not_found ->
            util_ht:set(Slot, Proposal, Proposals),
            case Active of
                true ->
                    PValue = {Ballot, Slot, Proposal},
                    consensus_commander_sup:create({?SELF, PValue});
                false ->
                    ok
            end;
        _Proposal ->
            ok
    end,
    {noreply, State};
%% propose request from replica, for reconfiguration
handle_cast({propose_rcfg, {Slot, Proposal}},
            #state{active = Active,
                   ballot_num = Ballot} = State) ->
    ?LDEBUG("LEA ~p::Received message ~p", [self(),
                                            {propose, {Slot, Proposal}}]),
    % Don't store the proposal, proceed with the algorithm
    case Active of
        true ->
            PValue = {Ballot, Slot, Proposal},
            spawn_commander(PValue);
        false ->
            ok
    end,
    {noreply, State};
%% adopted message sent by a scout,this message signifies that the current
%% ballot number ballot num has been adopted by a majority of acceptors
%% Note: The adopted ballot_num has to match current ballot_num!
handle_cast({adopted, {CurrBallot, PValues}},
            #state{proposals = Proposals,
                   ballot_num = CurrBallot} = State) ->
    ?LDEBUG("LEA ~p::Received message ~p", [self(),
                                            {adopted, {CurrBallot, PValues}}]),

    % Get all the proposals in PValues update our proposals with this data
    % PValues returned by Acceptor is of the format {Slot, {Ballot, Proposal}}
    intersect(Proposals, PValues),

    % Now that the ballot is accepted, make self as the master
    % We pass Ballot around to make sure the ballot is valid when
    % we receive master_adopted
    consensus_rcfg:set_master(CurrBallot),

    {noreply, State#state{active=true}};
%% master_adopted message sent by master_commander when everyone has accepted
%% this leader as their master
handle_cast({master_adopted, CurrBallot},
            #state{proposals = Proposals,
                   ballot_num = CurrBallot} = State) ->
    ?LDEBUG("LEA ~p::Received message ~p", [self(),
                                            {master_adopted, CurrBallot}]),

    % Set a timer to renew the lease
    erlang:send_after(?RENEW_LEASE_TIME, ?SELF, renew_master),

    % Spawn a commander for every proposal
    spawn_commanders(CurrBallot, Proposals),
    {noreply, State#state{active = true}};
%% preempted message sent by either a scout or a commander, it means that some
%% acceptor has adopted some other ballot
handle_cast({preempted, ABallot}, #state{ballot_num = CurrBallot} = State) ->
    ?LDEBUG("LEA ~p::Received message ~p", [self(), {preempted, ABallot}]),

    % If the new ballot number is bigger, increase ballot number and scout for
    % the next adoption
    NewBallot = case consensus_util:ballot_greater(ABallot, CurrBallot) of
        true ->
            NextBallot = consensus_util:incr_ballot(CurrBallot, ABallot),
            erlang:send_after(?BACKOFF_TIME, ?SELF, spawn_scout),
            NextBallot;
        false ->
            CurrBallot
    end,
    {noreply, State#state{active=false, ballot_num=NewBallot}};
%% TODO: Timeouts most probably happen due to partition (check)
%% Just restart them from now
%% Scout has timed out after waiting for replies
handle_cast(scout_timeout, #state{ballot_num = Ballot} = State) ->
    check_master_start_scout(Ballot),
    {noreply, State};
%% Commander has timed out after waiting for replies
handle_cast({commander_timeout, PValue}, #state{ballot_num = Ballot} = State) ->
    {_OldBallot, Slot, Proposal} = PValue,
    ?LINFO("LEA::timeout::~p", Proposal),
    NewPValue = {Ballot, Slot, Proposal},
    check_master_start_commander(NewPValue),
    {noreply, State};
%% Garbage collection: Remove decided slots from proposals
handle_cast({slot_decision, Slot}, #state{proposals=Proposals}=State) ->
    util_ht:del(Slot, Proposals),
    {noreply, State};
%% Deactivate leader when node is joining a cluster
handle_cast(cluster_join, State) ->
    {noreply, State#state{active=false}};
%% Reset leader local state
handle_cast(reset, #state{proposals=Proposals}=State) ->
    util_ht:reset(Proposals),
    {noreply, State};
%% Increment leader view
handle_cast(incr_view, #state{ballot_num=Ballot, proposals=Proposals}=State) ->
    NewBallot = consensus_util:incr_view(Ballot),
    util_ht:reset(Proposals),
    % Run scout round to push it to acceptors
    % TODO: Test possibility of two scouts running
    consensus_scout_sup:create({?SELF, NewBallot}),
    {noreply, State#state{ballot_num=NewBallot}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info(spawn_scout, #state{ballot_num=Ballot}=State) ->
    check_master_start_scout(Ballot),
    {noreply, State};
handle_info(renew_master, #state{ballot_num=Ballot}=State) ->
    % Check if we are still master, if yes extend lease
    case consensus_state:is_master() of
        true ->
            consensus_rcfg:set_master(Ballot);
        false ->
            check_master_start_scout(Ballot)
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
%% intersect replaces all local proposals with ones from PValues and also
%% adds additional entries in MaxPValues to its proposals set
intersect(Proposals, PVals) ->
    lists:foreach(fun({Slot, {_Ballot, Proposal}}) ->
                          util_ht:set(Slot, Proposal, Proposals)
                  end,
                  PVals).

spawn_commander(PValue) ->
    consensus_commander_sup:create({?SELF, PValue}).

spawn_commanders(Ballot, Proposals) ->
    spawn_commanders_lst(Ballot, util_ht:to_list(Proposals)).

spawn_commanders_lst(_Ballot, []) ->
    ok;
spawn_commanders_lst(Ballot, [H|L]) ->
    {Slot, Proposal} = H,
    PValue = {Ballot, Slot, Proposal},
    consensus_commander_sup:create({?SELF, PValue}),
    spawn_commanders_lst(Ballot, L).

% Start Scout only if we do not have a master with valid lease
% This is equivalent to monitoring the master / failure detection
check_master_start_scout(Ballot) ->
    case consensus_state:is_status(valid) of
        true ->
            % If lease is going to timeout and we are not the master
            % then start scout
            LeaseTime = consensus_state:get_lease_validity(),
            case {(LeaseTime =< ?MIN_LEASE), consensus_state:is_master()} of
                {true, false} ->
                    ?LDEBUG("Master expired, start scout"),
                    consensus_scout_sup:create({?SELF, Ballot});
                {false, false} ->
                    ?LDEBUG("Master still running, try after some time"),
                    erlang:send_after(LeaseTime, ?SELF, spawn_scout);
                {_, true} ->
                    ?LWARNING("Master failed to renew lease")
            end;
        % Not a valid member, try after some time
        % TODO: Calibrate this time
        false ->
            erlang:send_after(?LEASE_TIME, ?SELF, spawn_scout)
    end.


% Start Commander only if we do not have a master with valid lease
check_master_start_commander(NewPValue) ->
    LeaseTime = consensus_state:get_lease_validity(),
    % Restart the commander only if still master and lease valid
    case consensus_state:is_master() andalso (LeaseTime > ?MIN_LEASE) of
        true ->
            consensus_commander_sup:create({?SELF, NewPValue});
        false ->
            ok
    end.
