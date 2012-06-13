%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Records and macros for consensus app
%%% @end
%%%-------------------------------------------------------------------

%% Registered names of a consensus cluster actors
-define(LEADER, consensus_leader).
%-define(SCOUT, consensus_scout).
%-define(COMMANDER, consensus_commander).
-define(REPLICA, consensus_replica).
-define(ACCEPTOR, consensus_acceptor).

%% Consensus Messenger
-define(SYNC_MSG(Target, Msg), consensus_msngr:sync(Target, Msg)).
-define(ASYNC_MSG(Target, Msg), consensus_msngr:async(Target, Msg)).
