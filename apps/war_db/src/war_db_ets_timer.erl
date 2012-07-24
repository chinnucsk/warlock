%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB ETS timer
%%%
%%% Implements key expiry for war_db_ets_backend
%%%
%%% @end
%%%
%%% @since : 09 July 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_db_ets_timer).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, expire_at/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").

-define(TABLE, war_db_ets_timer).
-define(INTERVAL, 10000).                           % In milli seconds
-define(BATCH, 100).

-record(state, {
                %% We use only one timer for periodic runs
                timer_ref,

                %% Maintain a pointer to the next key slot that is to be deleted
                curr_key,

                %% Table: bag
                %% Key: Time mod interval
                %% Value:: Data to be expired + expiry time
                table,

                %% List of lists used for storing data marked for deletion
                exp_data=[]
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {error, _} | {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    TRef = erlang:start_timer(?INTERVAL, self(), tick),
    {ok, #state{timer_ref=TRef,
                curr_key=get_current_key(),
                table=ets:new(?TABLE, [bag, named_table, public])}}.

%% ------------------------------------------------------------------
%% Delete key after given Time
%% ------------------------------------------------------------------
-spec expire_at(integer(), term()) -> ok.
expire_at(ExpireTime, DBKey) ->
    Key = get_after_key(ExpireTime),
    ets:insert(?TABLE, {Key, {DBKey, ExpireTime}}).

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
handle_cast(process_exp_data, #state{exp_data=ExpData}=State) ->
    NewExpData = process_exp(ExpData, ?BATCH),
    case NewExpData of
        [] ->
            ok;
        _ ->
            gen_server:cast(?MODULE, process_exp_data)
    end,
    {noreply, State#state{exp_data=NewExpData}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info({timeout, _TRef, tick}, #state{timer_ref=OldTRef, curr_key=CurrKey,
                                          table=Table,
                                          exp_data=ExpData}=State) ->
    case get_current_key() > CurrKey of
        true ->
            ExpList = ets:lookup(Table, CurrKey),
            ets:delete(Table, CurrKey),
            NewExpData = lists:append(ExpData, [ExpList]),
            NewTRef = restart_timer(OldTRef, ?INTERVAL, tick),
            gen_server:cast(?MODULE, process_exp_data),
            {noreply, State#state{timer_ref=NewTRef,
                                  curr_key=CurrKey+1,
                                  exp_data=NewExpData}};
        false ->
            TRef = restart_timer(OldTRef, ?INTERVAL div 2, tick),
            {noreply, State#state{timer_ref=TRef}}
    end;
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
%% Returns slot of
get_key({Mega, Sec, Micro}) ->
    (trunc((Mega * 1000000 + Sec + Micro / 1000000) * 1000) div ?INTERVAL).

get_current_key() ->
    get_key(erlang:now()).

get_after_key(Time) ->
    Mega = Time div 1000000,
    Seconds = Time rem 1000000,
    get_key({Mega, Seconds, 0}).

restart_timer(OldRef, Time, Msg) ->
    erlang:cancel_timer(OldRef),
    erlang:start_timer(Time, self(), Msg).


process_exp([], _Batch) ->
    [];
process_exp(List, 0) ->
    List;
process_exp([[] | Tail], _Batch) ->
    Tail;
process_exp([[{_Key, Val} | STail] | Tail], Batch) ->
    war_db:x([del_expired, Val]),
    process_exp([STail | Tail], Batch-1).
