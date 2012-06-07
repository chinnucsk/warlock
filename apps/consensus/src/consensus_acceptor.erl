%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Acceptor
%%%
%%% Paxos acceptor
%%% Acts as the distributed memory of the consensus service
%%%
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_acceptor).
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
-include_lib("util/include/config.hrl").

-record(state, {
            ballot_num = {0, 0},

            % Set of pvalues where
            % pvalue = {Ballot number, Slot number, Proposal}
            accepted = sets:new()
}).

-define(SELF, self()).
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
%% phase 1 a message from some leader
handle_cast({p1a, {Leader, LBallot}}, #state{ballot_num = CurrBallot,
                                             accepted = Accepted} = State) ->
    ?LINFO("Received message ~p", [{p1a, {Leader, LBallot}}]),
    Ballot = case consensus_util:ballot_greater(LBallot, CurrBallot) of
        true ->
            LBallot;
        false ->
            CurrBallot
    end,
    NewState = State#state{ballot_num = Ballot},
    % Response = {p1b, self(), ballot_num, accepted} /From paper
    Response = {p1b, {?SELF, Ballot, Accepted}},
    consensus_msngr:cast(Leader, Response),
    {noreply, NewState};
%% phase 2 a message from some leader
handle_cast({p2a, {Leader, {LBallot, _Slot, _Proposal} = PValue}},
            #state{ballot_num = CurrBallot, accepted = Accepted} = State) ->
    ?LINFO("Received message ~p", [{p2a, {Leader, PValue}}]),
    {Ballot, NewAccepted} =
        case consensus_util:ballot_greateq(LBallot, CurrBallot) of
            true ->
                {LBallot, sets:add_element(PValue, Accepted)};
        false ->
            {CurrBallot, Accepted}
    end,
    NewState = State#state{ballot_num = Ballot, accepted = NewAccepted},
    % Response = {p2b, self(); ballot num} /From paper
    Response = {p2b, {?SELF, Ballot}},
    consensus_msngr:cast(Leader, Response),
    {noreply, NewState};
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
