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
%% TODO: Write specs
%% TODO: Get the registered names of services from a central config file
-module(consensus_msngr).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([call/2, cast/2]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------

call(Target, Msg) ->
    TargetAdd = get_add(Target),
    gen_server:call(TargetAdd, Msg).

cast(Target, Msg) ->
    TargetAdd = get_add(Target),
    gen_server:cast(TargetAdd, Msg).








%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------

get_add(Target) ->
    case Target of
        % Leader process on the master node
        leader ->
            Master = consensus_state:get_master(),
            {consensus_leader, Master};
        % Replica process on the master node
        master_replica ->
            Master = consensus_state:get_master(),
            {consensus_replica, Master};
        _ ->
            {}
    end.

