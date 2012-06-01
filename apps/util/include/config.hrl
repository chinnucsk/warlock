%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Project level config settings 
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------

%% Config params

-define(APP, dlock).

%% Macro for debugging code

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

%% Macro for timing code sections

-ifndef(TIMEON).
% Yes, these need to be on a single line to work...
-define(TIMEON, erlang:put(debug_timer, [now()|case erlang:get(debug_timer) == undefined of true -> []; false -> erlang:get(debug_timer) end])).
-define(TIMEOFF(Var), io:format("~s :: ~10.2f ms : ~p~n", [string:copies(" ", length(erlang:get(debug_timer))), (timer:now_diff(now(), hd(erlang:get(debug_timer)))/1000), Var]), erlang:put(debug_timer, tl(erlang:get(debug_timer)))).
-endif.

%% Logging macros

-define(LDEBUG(Msg),
    lager:debug(Msg)).
-define(LDEBUG(Msg, Args),
    lager:debug(Msg, Args)).

-define(LINFO(Msg),
    lager:info(Msg)).
-define(LINFO(Msg, Args),
    lager:info(Msg, Args)).

-define(LNOTICE(Msg),
    lager:notice(Msg)).
-define(LNOTICE(Msg, Args),
    lager:notice(Msg, Args)).

-define(LWARNING(Msg),
    lager:warning(Msg)).
-define(LWARNING(Msg, Args),
    lager:warning(Msg, Args)).

-define(LERROR(Msg),
    lager:error(Msg)).
-define(LERROR(Msg, Args),
    lager:error(Msg, Args)).

-define(LCRITICAL(Msg),
    lager:critical(Msg)).
-define(LCRITICAL(Msg, Args),
    lager:critical(Msg, Args)).

-define(LALERT(Msg),
    lager:alert(Msg)).
-define(LALERT(Msg, Args),
    lager:alert(Msg, Args)).

-define(LEMERGENCY(Msg),
    lager:emergency(Msg)).
-define(LEMERGENCY(Msg, Args),
    lager:emergency(Msg, Args)).


%% Record definitions

-record(dlock_backend_client, {
                           %% Type of the client
                           type,
             
                           %% Instance of the client
                           inst
}).

-record(dlock_op, {
               %% Type of operation :: read, write 
               %% TODO: More types?
               type,
               
               %% Actual operation
               op
}).