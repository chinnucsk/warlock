%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server sender
%%%
%%% In lines of redis replication,
%%% * Client (receiver) connects and issues SYNC command
%%% * Sender starts queuing all decisions and adds client as a subscriber
%%% * Sender makes a copy of the data and transfers it to receiver
%%% * Once client has all the data, server starts processing local queue
%%% * Once both are in sync, server stops sending decisions and returns to
%%%   active state once local queue is completely processed
%%% @end
%%%
%%% @since : 22 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Have a timeout
-module(war_server_sender).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").
-include("war_server.hrl").

%% --------------------------------------------------------------------
%% Public functions
%% --------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    Me = self(),
    Pid = spawn_link(fun()-> start(Me) end),
    % Block till we have started the listener
    receive ok -> ok end,
    {ok, Pid}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------
start(ReplyPid) ->
    ?LDEBUG("war_server_sender::Start"),
    Port = war_util_conf:get(port, replication),
    FileName = war_util_conf:get(file, replication),

    % Start listener
    {ok, Listen} = gen_tcp:listen(Port, [{active,false}, {reuseaddr, true}]),
    erlang:send(ReplyPid, ok),
    {ok, Accept} = gen_tcp:accept(Listen),

    % Wait for sync signal
    inet:setopts(Accept, [{active, once}]),
    Socket = receive
        {tcp, Sock, ?SYNC_LIST} ->
            ?LDEBUG("war_server_sender::Got first sync"),
            Sock
    end,

    % Create war_db dump file
    war_db:backup(FileName),
    {ok, File} = file:read_file(FileName),
    file:delete(FileName),

    % Send file to receiver
    ?LDEBUG("war_server_sender::Sending file to receiver"),
    gen_tcp:send(Socket, File),

    % Cleanup
    gen_tcp:close(Socket),
    gen_tcp:close(Listen).
