-module(tcp_warlock_ets).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, {client}).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
  {ok, C} = eredis:start_link("localhost", 9736, 1, "password"),
  {ok, #state{client=C}}.


run(get, KeyGen, _ValueGen, #state{client=C}=State) ->
    Key = KeyGen(),
    Cmd = [loc, get, Key],
    case eredis:q(C, Cmd) of
        {ok, _} ->
            {ok, State};
        {error, notfound} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(put, KeyGen, ValueGen, #state{client=C}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    Cmd = [clu, set, Key, Val],
    case eredis:q(C, Cmd) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(del, KeyGen, _ValueGen, #state{client=C}=State) ->
    Key = KeyGen(),
    Cmd = [clu, del, Key],
    case eredis:q(C, Cmd) of
        {ok, _} ->
            {ok, State};
        {error, notfound} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.
