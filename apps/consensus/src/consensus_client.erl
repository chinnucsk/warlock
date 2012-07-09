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
%%% In this modified version, we only send a message to the local replica.
%%% Another modification is that the response is sent directly to the caller
%%% by the local client on the master node.
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
-export([propose/1, propose_rcfg/1,
         exec/1]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("consensus.hrl").

%% ------------------------------------------------------------------
%% Public functions
%% ------------------------------------------------------------------
%% Send read request to the master replica
%% Note: Here we are assuming Operation is uniquely identified
-spec propose(#dop{}) -> ok.
propose(#dop{type=read}=Operation) ->
    %% Check if lease is valid, if yes, execute on local replica
    %% TODO: Add option to allow master only reads
    LeaseTime = consensus_state:get_lease_validity(),
    case (LeaseTime > ?MIN_LEASE) of
        true ->
            ?MODULE:exec(Operation);
        false ->
            {error, out_of_sync}
    end;
%% Send other requests to the local replica
propose(Operation) ->
    Msg = {request, Operation},
    % TODO: Make this configurable
    ?ASYNC_MSG(?REPLICA, Msg).

%% Send the request to all replicas
%% Only the ones with active leaders will succeed, rest are ignored
%% Paxos will handle multiple active leaders using ballots to make sure
%% only one master exists
-spec propose_rcfg(#rop{}) -> ok.
propose_rcfg(Operation) ->
    Msg = {request, Operation},
    % TODO: Make this configurable
    ?ASYNC_MSG(replicas, Msg).

%% Execute the callback function in the operation
%% Called by the replica
-spec exec(#dop{} | #rop{}) -> ok.
exec(#dop{type = Type,
          module = M,
          function = F,
          args = A,
          client = {Client, Ref}}) ->
    ?LDEBUG("Executing operation ~p:~p(~p)", [M, F, A]),

    Result = M:F(Type, A),
    ?LDEBUG("RESULT ==>> ~p", [Result]),

    case Type of
        read ->
            ?ASYNC_MSG(Client, {response, Ref, Result});
        write ->
            case consensus_state:is_master() of
                true ->
                    ?ASYNC_MSG(Client, {response, Ref, Result});
                false ->
                    ok
            end
    end,
    Result;
exec(#rop{}=ROp) ->
    consensus_rcfg:callback(ROp).

%% ------------------------------------------------------------------
%% Internal function
%% ------------------------------------------------------------------
