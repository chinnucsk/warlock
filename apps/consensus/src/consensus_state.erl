%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus State
%%%
%%% Wraps the consensus state of the node. Uses ETS internally.
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Write specs
-module(consensus_state).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([new/0, del/0,
         get_members/0, set_members/1,
         get_master/0, set_master/1, is_master/0, get_valid_master/0
        ]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-define(NODE, node()).

-define(INITIAL_STATUS, {valid, election}).

%% Master state is in the format of {node(), lease()}
-define(INITIAL_MASTER, {none, none}).

%% Time window before lease expiry we disallow master requests
%% To be tuned as per clock drift rate
-define(MIN_LEASE, 100000). % In microseconds, 0.1s

%% Name of the ets table
-define(STATE, cons_state).

%% The possible states of the master election FSM
%% A node can either be a master, a member or in election
%-type fsm_state() :: election | master | member.

%% Lease is a time in the future, until which the master has lease
%-type lease() :: {Megaseconds::Integer,
%                  Seconds::Integer,
%                  Microseconds::Integer}.

%% Type of members
%% valid - Node is part of the cluster
%% invalid - Node is removed the the cluster and is to be ignored
%% joining - Node is trying to join the cluster
%% Second parameter used for fine grained status
%-type member_status() ::
%          {valid, election} |
%          {valid, member} |
%          {valid, master} |
%          {valid, unknown} |
%          {invalid, unknown} |
%          {joining, unknown}.

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
new() ->
    ets:new(?STATE, [set, named_table, public]),
    initialize().

del() ->
    ets:delete(?STATE).

% TODO: Improve the backend storage DS
get_members() ->
    MembersState = get_state(members),
    get_members(MembersState).

set_members(Members) ->
    set(members, Members).

get_master() ->
    {Master, _Lease} = get_state(master),
    Master.

get_valid_master() ->
    {Master, Lease} = get_state(master),
    case is_lease_valid(Lease) of
        true ->
            Master;
        false ->
            {error, no_master}
    end.

set_master(Master) ->
    set(master, Master).

% Check if current node is the master
is_master() ->
    catch check_master().

%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------

%% Initialize all keys in the state
initialize() ->
    Objs = [
        %% Name of the current node and its status
        {node, {?NODE, ?INITIAL_STATUS}},
        %% Members of the cluster
        {members, [{?NODE, ?INITIAL_STATUS}]},
        %% Master node of the cluster
        {master, ?INITIAL_MASTER}],
    ets:insert(?STATE, Objs).

get_state(Key) ->
    [{Key, Val}] = ets:lookup(?STATE, Key),
    Val.

set(Key, Value) ->
    ets:insert(?STATE, {Key, Value}).

check_master() ->
    {Node, NodeStatus} = get_state(node),
    {Master, Lease} = get_state(master),

    case NodeStatus of
        {valid, master} ->
            ok;
        _ ->
            throw(false)
    end,

    case Node =:= Master of
        true ->
            ok;
        _ ->
            throw(false)
    end,

    case is_lease_valid(Lease) of
        true ->
            ok;
        false ->
            throw(false)
    end,

    true.

is_lease_valid(Lease) ->
    timer:now_diff(erlang:now(), Lease) > ?MIN_LEASE.


get_members(MembersState) ->
    get_members(MembersState, []).

get_members([], Acc) ->
    Acc;
get_members([{Member, _State} | MembersState], Acc) ->
    get_members(MembersState, [Member | Acc]).
