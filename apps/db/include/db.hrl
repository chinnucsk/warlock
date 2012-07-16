%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB app level config settings
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------------
-type key() :: binary().
-type value() :: term().
-type tid() :: integer().
-type table() :: atom() | tid().

%% ------------------------------------------------------------------
%% Record definitions
%% ------------------------------------------------------------------

%% This is used in case we have more connection parameters
-record(client, {
             %% Type of the client
             %% type,  TODO: Remove if this is not required

             %% Instance of the client
             inst :: table()
}).
