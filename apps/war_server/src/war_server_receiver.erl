%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server receiver
%%%
%%% Once started
%%% * Send SYNC command to sender
%%% * Recive data from sender
%%% * Restore DB
%%% * Send final sync signal
%%% * Stop
%%% @end
%%%
%%% @since : 22 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_server_receiver).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").
-include("war_server.hrl").

%% --------------------------------------------------------------------
%% Public functions
%% --------------------------------------------------------------------
-spec start_link(term()) -> {ok, pid()}.
start_link(Args) ->
    Pid = spawn_link(fun()-> start(Args) end),
    {ok, Pid}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
start(Args) ->
    ?LDEBUG("war_server_receiver::Start"),
    IpAddr = war_util_conf:get(ip, replication),
    Port = war_util_conf:get(port, replication),

    % Open connection to sender
    ?LDEBUG("war_server_receiver::Connecting to sender at ~p:~p", [IpAddr, Port]),
    {ok, Socket} = gen_tcp:connect(IpAddr, Port,
                                   [binary, {packet, 0}, {active, false}]),

    % Send initial sync signal
    ?LDEBUG("Sending sync to socket ~p", [Socket]),
    gen_tcp:send(Socket, ?SYNC),

    % Receive war_db data
    Data = recv_data(<<>>, Socket),
    ?LDEBUG("Received data from sender"),

    % Restore war_db
    FileName = war_util_conf:get(file, replication),
    ok = file:write_file(FileName, Data),
    war_db:restore(FileName),

    % Cleanup
    ?LDEBUG("war_server_receiver::Process complete, cleaning up"),
    gen_tcp:close(Socket),
    war_server:receive_complete(Args).

recv_data(Buffer, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      recv_data(<<Buffer/binary, Data/binary>>, Socket);
    {error, closed} ->
      gen_tcp:close(Socket),
      Buffer
  end.
