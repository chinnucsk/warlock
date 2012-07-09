%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus Messenger
%%%
%%% Helper module used to send different types of messages
%%% @end
%%%
%%% @since : 05 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Get the registered names of services from a central config file
-module(consensus_msngr).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([sync/2, async/2]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
-spec sync(atom(), term()) -> term().
sync(Target, Msg) ->
    TargetAdd = get_add(Target),
    gen_server:call(TargetAdd, Msg).

-spec async(atom(), term()) -> ok.
async(Target, Msg) when is_pid(Target) ->
    ?LDEBUG("MSG {PID, Msg}:: {~p, ~p}", [Target, Msg]),
    gen_server:cast(Target, Msg);
async(Target, Msg) when
  Target == ?LEADER;
  Target == ?ACCEPTOR;
  Target == ?REPLICA ->
    gen_server:cast(Target, Msg);
async(Target, Msg) ->
    {Name, Nodes} = get_add(Target),
    ?LDEBUG("MSG {TARGET, NODES, Msg}:: {~p, ~p, ~p}", [Name, Nodes, Msg]),
    gen_server:abcast(Nodes, Name, Msg).

%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------

get_add(Target) ->
    case Target of
        % Leader process on the master node
        leaders ->
            Master = consensus_state:get_members(),
            {?LEADER, Master};
        % Replica process on the master node
        master_replica ->
            Master = consensus_state:get_master(),
            {?REPLICA, Master};
        % Leader process on the master node
        master_leader ->
            Master = consensus_state:get_master(),
            {?LEADER, Master};
        % Acceptors on valid set of nodes
        acceptors ->
            Acceptors = consensus_state:get_members(),
            {?ACCEPTOR, Acceptors};
        % Replicas on valid set of nodes
        replicas ->
            Replicas = consensus_state:get_members(),
            {?REPLICA, Replicas}
    end.
