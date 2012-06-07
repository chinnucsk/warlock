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
-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% -------------------------------------------------------------------
-include_lib("util/include/config.hrl").

-define(SELF, self()).
-define(FIRST_BALLOT, {0, ?SELF}).

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    ?LINFO("Starting " ++ erlang:atom_to_list(?MODULE)),

    % Spawn scout with the first ballot
    consensus_scout_sup:create({?SELF, ?FIRST_BALLOT}),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% propse request from replica
handle_cast({propose, {Slot, Proposal}},
            #state{proposals = Proposals,
                   active = Active,
                   ballot_num = Ballot} = State) ->
    ?LINFO("Received message ~p", [{propose, {Slot, Proposal}}]),
    % Add the proposal if we do not have a command for the proposed spot
    ?LINFO("Leader state ~p", [State]),
    ?LINFO("util_ht:get(~p, ~p) ~p", [Slot, Proposals, Proposal]),
    case util_ht:get(Slot, Proposals) of
        not_found ->
            % Note: we could make Slot as the key (better performance), but just
            % following similar structure as replica to keep things consistent
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
%% adopted message sent by a scout,this message signifies that the current
%% ballot number ballot num has been adopted by a majority of acceptors
%% Note: The adopted ballot_num has to match current ballot_num!
handle_cast({adopted, {CurrBallot, PValues}},
            #state{proposals = Proposals,
                   ballot_num = CurrBallot} = State) ->
    ?LINFO("Received message ~p", [{adopted, {CurrBallot, PValues}}]),

    % Get all the proposals in PValues with max ballot number and update our
    % proposals with this data
    Pmax = pmax(PValues),
    intersect(Proposals, Pmax),
    % Spawn a commander for every proposal
    spawn_commanders(CurrBallot, Proposals),
    {noreply, State#state{active = true}};
%% preempted message sent by either a scout or a commander, it means that some
%% acceptor has adopted some other ballot
handle_cast({preempted, ABallot}, #state{ballot_num = CurrBallot} = State) ->
    ?LINFO("Received message ~p", [{preempted, ABallot}]),

    % If the new ballot number is bigger, increase ballot number and scout for
    % the next adoption
    NewBallot = case consensus_util:ballot_greater(ABallot, CurrBallot) of
        true ->
            NextBallot = consensus_util:incr_ballot(CurrBallot, ABallot),
            consensus_scout_sup:create({?SELF, NextBallot}),
            NextBallot;
        false ->
            CurrBallot
    end,
    {noreply, State#state{active = false, ballot_num = NewBallot}};
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
% TODO: Naive implementation!! Make it better!
pmax(PValues) ->
    PList = sets:to_list(PValues),
    PDict = dict:new(),
    NewPDict = pmax(PList, PDict),
    NewPList = lists:map(fun({Key, Value}) -> get_obj(Key, Value) end,
                             dict:to_list(NewPDict)),
    sets:from_list(NewPList).

pmax([], PDict) ->
    PDict;
pmax([PVal|PList], PDict) ->
    {Key, Val} = get_keyval(PVal),
    NewPDict = case dict:find(Key, PDict) of
        % We already have a proposal with a different ballot num
        {ok, AltVal} ->
            % Get one with max ballot num
            case Val > AltVal of
                true ->
                    dict:store(Key, Val, PDict);
                false ->
                    PDict
            end;
        % Its a new proposal for the specified ballot
        error ->
            dict:store(Key, Val, PDict)
    end,
    pmax(PList, NewPDict).

get_keyval({A, B, C}) ->
    {{B, C}, A}.

get_obj({B, C}, A) ->
    {A, B, C}.

% TODO: Try to make this faster
intersect(Proposals, MaxPValues) ->
    intersect_lst(Proposals, sets:to_list(MaxPValues)).

intersect_lst(_, []) ->
    ok;
intersect_lst(Proposals, [PVal|PList]) ->
    {_Ballot, Slot, Proposal} = PVal,
    util_ht:set(Slot, Proposal, Proposals),
    intersect_lst(Proposals, PList).

spawn_commanders(Ballot, Proposals) ->
    spawn_commanders_lst(Ballot, util_ht:to_list(Proposals)).

spawn_commanders_lst(_Ballot, []) ->
    ok;
spawn_commanders_lst(Ballot, [H|L]) ->
    {Slot, Proposal} = H,
    PValue = {Ballot, Slot, Proposal},
    consensus_commander_sup:create({?SELF, PValue}),
    spawn_commanders_lst(Ballot, L).