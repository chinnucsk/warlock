%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB ETS timer
%%%
%%% Implements key expiry for db_ets_backend
%%%
%%% @end
%%%
%%% @since : 09 July 2012
%%% @end
%%%-------------------------------------------------------------------
-module(db_ets_timer).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/0, start_link/0,
         expire_after/2, expire_at/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").

-record(state, {
                %% Keeps track of all the requests
                manager
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start() -> {error, _} | {ok, pid()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    {ok, #state{manager=ets_ht:new()}}.

%% ------------------------------------------------------------------
%% Delete key after given Time
%% ------------------------------------------------------------------
-spec expire_after(integer(), term()) -> ok.
expire_after(Time, Key) ->
    gen_server:call(?MODULE, {expire_after, Time, Key}).

%% ------------------------------------------------------------------
%% Delete key at given Time
%% ------------------------------------------------------------------
-spec expire_at(integer(), term()) -> ok.
expire_at(Time, Key) ->
    gen_server:call(?MODULE, {expire_at, Time, Key}).

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
% TODO: Better timer implementation for more performance,
% e.g. timer per second instead of timer per request
handle_call({expire_after, Time, Key}, _From, #state{manager=Manager}=State) ->
    Msg = {key, Key},
    Reply = case ets_ht:get(Key, Manager) of
        not_found ->
            {ok, start_timer(Time, Key, Msg, Manager)};
        OldRef ->
            {ok, restart_timer(OldRef, Time, Key, Msg, Manager)}
    end,
    {reply, Reply, State};
handle_call({expire_at, Time, Key}, _From, #state{manager=Manager}=State) ->
    Msg = {key, Key},
    ExpireTime = {Time div 1000000, Time rem 1000000, 0},
    ExpireAfter = erlang:trunc(timer:now_diff(ExpireTime, erlang:now()) / 1000),

    case ets_ht:get(Key, Manager) of
        not_found ->
            ok;
        OldRef ->
            stop_timer(OldRef)
    end,

    Reply = case ExpireAfter > 0 of
        true ->
            {ok, start_timer(Time, Key, Msg, Manager)};
        false ->
            expire_key(Key)
    end,

    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info({timeout, TRef, {key, Key}}, #state{manager=Manager}=State) ->
    case ets_ht:get(Key, Manager) of
        not_found ->
            ?LINFO("Error, timeout req not found");
        TRef ->
            expire_key(Key),
            ets_ht:del(Key, Manager)
    end,
    {noreply, State};
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
expire_key(Key) ->
    db:x([del, Key]).

start_timer(Time, Key, Msg, Manager) ->
    TRef = erlang:start_timer(Time, self(), Msg),
    ets_ht:set(Key, TRef, Manager),
    TRef.

stop_timer(OldRef) ->
    erlang:cancel_timer(OldRef).

restart_timer(OldRef, Time, Key, Msg, Manager) ->
    stop_timer(OldRef),
    start_timer(Time, Key, Msg, Manager).
