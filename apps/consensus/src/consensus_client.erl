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
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Propose sends a request to the replica on the master
%% Note: Here we are assuming Operation is uniquely identified
%% ------------------------------------------------------------------
propose(Operation) ->
    Msg = {request, Operation},
    ?ASYNC_MSG(replicas, Msg).

%% ------------------------------------------------------------------
%% Executes the callback function in the operation
%% Called by the replica
%%
%% Note: This can be extended to handle different kinds of operation
%% other than #dop
%% ------------------------------------------------------------------
exec(#dop{type = Type,
          module = M,
          function = F,
          args = A,
          client = Client} = _Operation) ->
    ?LDEBUG("Executing operation ~p:~p(~p)", [M, F, A]),

    case {consensus_state:is_master(), Type} of
        {true, _} ->
            respond(Client, exec(M, F, A));
        {false, write} ->
            exec(M, F, A);
        {false, read} ->
            ok
    end.

%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------

exec(M, F, A) ->
    Result = M:F(A),
    Response = {response, Result},
    ?LDEBUG("RESULT ==>> ~p", [Result]),
    Response.

respond(Client, Response) ->
    case Client of
        undefined ->
            ok;
        _ ->
            ?ASYNC_MSG(Client, Response)
    end.