%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server configs and common macro definitions
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------

%% dop = Distributed operation
-record(dop, {
          %% Type of operation determine what has to be done
          %% The selective execution is done by the consensus "client"
          %% read, write
          %% TODO: TYPE of command. Use something else?
          type,

          %% Usually server_callback
          module,

          function,

          %% Has data about the request (actual command)
          %% sent to the server
          args,

          %% Pid of the client to whom reply has to be sent to
          client
}).


%% Client request
%%      Command + Data = []
%%      Args
%%          Client id = Pid of the spawned worker

%% Client request - another possible request type
%%     status -> return master and cluster info



%% Server request/operation
%%      Type - what consensus client should do will depend on this
%%      Module - server_callback
%%      Function - server_callback fun
%%      Args - Command + Data
%%      Client - Pid of the client to respond to, ignore if empty
