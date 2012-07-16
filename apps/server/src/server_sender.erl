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
%%% * Sender starts queuing all decisions
%%% * Sender makes a copy of the data and transfers it to receiver
%%% * Once transfer complete
%%%     * Read log
%%%     * Execute command
%%%     * Parallely transfer command to receiver
%%%     * Remove from queue
%%% * Server keeps transferring to receiver unless asked to stop
%%% @end
%%%
%%% @since : 22 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_sender).

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("util/include/common.hrl").
-include("server.hrl").

-define(TIMEOUT, 5000).

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
    ?LDEBUG("server_sender::Start"),
    Port = util_conf:get(port, replication),
    FileName = util_conf:get(file, replication),

    % Start listener
    {ok, Listen} = gen_tcp:listen(Port, [{active,false}, {reuseaddr, true}]),
    erlang:send(ReplyPid, ok),
    {ok, Accept} = gen_tcp:accept(Listen),

    % Wait for sync signal
    inet:setopts(Accept, [{active, once}]),
    Socket = receive
        {tcp, Sock, ?SYNC_LIST} ->
            ?LDEBUG("server_sender::Got first sync"),
            Sock
    end,

    % Create db dump file
    db:backup(FileName),
    {ok, File} = file:read_file(FileName),
    file:delete(FileName),

    % Send file to receiver
    ?LDEBUG("server_sender::Sending file to receiver"),
    gen_tcp:send(Socket, File),

    gen_tcp:close(Socket),
    gen_tcp:close(Listen).
