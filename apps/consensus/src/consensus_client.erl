%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Consensus client
%%%
%%% As a Paxos client, it is responsible for sending the first request
%%% to the replicas.
%%% In this modified version, we only send a message to the replica on the
%%% "master" (node with leader process running).
%%% Another modification is that the response is sent directly to the caller
%%% by the master and this client is bypassed.
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(consensus_client).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([propose/1, exec/1]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/config.hrl").

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Propose sends a request to the replica on the master
%% ------------------------------------------------------------------
propose(Operation) ->
    Msg = {request, Operation},
    consensus_msngr:cast(master_replica, Msg).

%% ------------------------------------------------------------------
%% Executes the callback function in the operation
%% Called by the replica
%% ------------------------------------------------------------------
exec(#dop{type = Type,
          module = M,
          function = F,
          args = A,
          client = Client} = _Operation) ->
    ?LINFO("Executing operation"),
    case Type of
        % Only master needs to respond to reads
        read ->
            case consensus_state:is_master() of
                true ->
                    exec(M, F, A, Client);
                false ->
                    ok
            end;
        % All nodes need to execute writes
        write ->
            exec(M, F, A, Client)
    end.

%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------

exec(M, F, A, Client) ->
    Result = M:F(A),
    Response = {response, Result},
    ?LINFO("RESULT ==>> ~p", [Result]),
    consensus_msngr:cast(Client, Response).