-module(war_consensus_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    war_consensus_sup:start_link().

stop(_State) ->
    % Clear consensus state
    war_consensus_state:del(),
    ok.
