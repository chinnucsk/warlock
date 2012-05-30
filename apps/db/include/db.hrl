%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc DB app level config settings 
%%% @end
%%%
%%% @since : 30 May 2012 by Wooga GmbH
%%% @end
%%%-------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Type definitions
%% ------------------------------------------------------------------
-type key() :: binary().

-type value() :: term(). 

%% ------------------------------------------------------------------
%% Record definitions
%% ------------------------------------------------------------------
-record(client, {
             %% Type of the client
             %% type,  TODO: Remove if this is not required
 
             %% Instance of the client
             inst
}).
