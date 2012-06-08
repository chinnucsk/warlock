%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server callback
%%%
%%% The consensus module calls this module
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_callback).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([handle/1]).

%% -----------------------------------------------------------------
%% Include files and private macros
%% -----------------------------------------------------------------

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%% Callback function is of the format [Command, Data]
handle([get, Key]) ->
    db:get(Key);
handle([set, {Key, Value}]) ->
    db:set([Key, Value]);
handle([del, Key]) ->
    db:del(Key).


%%-------------------------------------------------------------------
%% This is the first connection from the client
%% Send data about the cluster to the client
%%-------------------------------------------------------------------

% TODO: Implement this