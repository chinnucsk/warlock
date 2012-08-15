-module(warlock_ets).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").

-record(state, {nodes=[]}).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
    Nodes   = basho_bench_config:get(nodes),
    Cookie  = basho_bench_config:get(cookie, 'warlock'),
    MyNode  = basho_bench_config:get(mynode, [basho_bench, longnames]),

    %% Try to spin up net_kernel
    case net_kernel:start(MyNode) of
        {ok, _} ->
          ?INFO("Net kernel started as ~p\n", [node()]);
      {error, {already_started, _}} ->
          ok;
      {error, Reason} ->
          ?FAIL_MSG("Failed to start net_kernel for ~p: ~p\n", [?MODULE, Reason])
  end,

  %% Initialize cookie for each of the nodes
  erlang:set_cookie(hd(MyNode), Cookie),

  %% Try to ping each of the nodes
  ping_each(Nodes),

  {ok, #state{nodes=Nodes}}.


run(get, KeyGen, _ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Node = hd(Nodes),
    Cmd = [get, Key],
    case rpc:call(Node, war_server, x, [loc, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, notfound} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(puta, KeyGen, ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    Node = hd(Nodes),
    Cmd = [set, Key, Val],
    case rpc:call(Node, war_server, x, [clu, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(putb, KeyGen, ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
  Node = hd(tl(Nodes)),
    Cmd = [set, Key, Val],
    case rpc:call(Node, war_server, x, [clu, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(putc, KeyGen, ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    Node = hd(tl(tl(Nodes))),
    Cmd = [set, Key, Val],
    case rpc:call(Node, war_server, x, [clu, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(del, KeyGen, _ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Node = hd(Nodes),
    Cmd = [del, Key],
    case rpc:call(Node, war_server, x, [clu, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, notfound} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(putenxa, KeyGen, ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    Node = hd(Nodes),
    Cmd = [setenx, 100000, Key, Val],
    case rpc:call(Node, war_server, x, [clu, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(test, KeyGen, ValueGen, #state{nodes=Nodes}=State) ->
    Key = KeyGen(),
    Val = ValueGen(),
    Node = hd(Nodes),
    Cmd = [setenx, 60, Key, Val],
    case rpc:call(Node, war_server, x, [clu, Cmd]) of
        {ok, _} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(ping, _KeyGen, _ValueGen, #state{nodes=Nodes}=State) ->
  Node=hd(Nodes),
  case rpc:call(Node, war_server, ping, []) of
      pong ->
        {ok, State};
      _ ->
        {error, pang, State}
  end.

ping_each([]) ->
      ok;
ping_each([Node | Rest]) ->
      case net_adm:ping(Node) of
            pong ->
                  ping_each(Rest);
            pang ->
                  ?FAIL_MSG("Failed to ping node ~p\n", [Node])
          end.
