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
-define(REPLICA, consensus_replica).
-define(ACCEPTOR, consensus_acceptor).

-define(STATE_MGR, consensus_state).

%% Consensus Messenger
-define(SYNC_MSG(Target, Msg), consensus_msngr:sync(Target, Msg)).
-define(ASYNC_MSG(Target, Msg), consensus_msngr:async(Target, Msg)).

%% Unique slot used only by master
-define(MASTER_SLOT, 0).

%% Default lease time for master node
-define(LEASE_TIME, 5000). % Master lease, 5s

%% Time window before lease expiry we disallow master requests
%% To be tuned as per clock drift rate
-define(MIN_LEASE, 10). % In milli seconds, 10ms
