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
-module(server_receiver).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("server.hrl").

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
    ?LDEBUG("server_receiver::Start"),
    IpAddr = conf_helper:get(ip, replication),
    Port = conf_helper:get(port, replication),

    % Open connection to sender
    ?LDEBUG("server_receiver::Connecting to sender at ~p:~p", [IpAddr, Port]),
    {ok, Socket} = gen_tcp:connect(IpAddr, Port,
                                   [binary, {packet, 0}, {active, false}]),

    % Send initial sync signal
    ?LDEBUG("Sending sync to socket ~p", [Socket]),
    gen_tcp:send(Socket, ?SYNC),

    % Receive db data
    %{ok, Data} = gen_tcp:recv(Socket, 0),
    Data = recv_data(<<>>, Socket),
    ?LDEBUG("Received data from sender"),

    % Restore db
    FileName = conf_helper:get(file, replication),
    ok = file:write_file(FileName, Data),
    db:restore(FileName),

    % Send final sync signal and cleanup
    ?LDEBUG("server_receiver::Process complete, cleaning up"),
    gen_tcp:close(Socket),
    server:receive_complete(Args).

recv_data(Buffer, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      recv_data(<<Buffer/binary, Data/binary>>, Socket);
    {error, closed} ->
      gen_tcp:close(Socket),
      Buffer
  end.
