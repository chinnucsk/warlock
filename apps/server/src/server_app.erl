%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server application
%%% @end
%%%-------------------------------------------------------------------
-module(server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% Application callbacks
%% ------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    server_sup:start_link().

stop(_State) ->
    ok.
