%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Config Helper
%%%
%%% Helper functions module to get config values
%%% @end
%%%
%%% @since : 30 May 2012
%%% @end
%%%-------------------------------------------------------------------
-module(conf_helper).

%% ------------------------------------------------------------------
%% Include files
%% ------------------------------------------------------------------
-include("common.hrl").

%% ------------------------------------------------------------------
%% Function Exports
%% ------------------------------------------------------------------
-export([get/1, get/2, get/3]).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec get(Group::atom()) -> term().
get(Group) ->
    proplists:get_value(Group, get_all()).

-spec get(Key::atom(), Group::atom()) -> term().
get(Key, Group) ->
    proplists:get_value(Key, conf_helper:get(Group)).

-spec get(Key::atom(), Group::atom(), Default::term()) -> term().
get(Key, Group, Default) ->
    proplists:get_value(Key, conf_helper:get(Group), Default).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%% TODO: Find a better solution for the config file problem
get_all() ->
    File = case code:priv_dir(?APP) of
        {error,bad_name} ->
            % Assuming the app is being run from the app folder
            AppFile = "../../priv/dlock.term",
            case filelib:is_file(AppFile) of
                true ->
                    AppFile;
                % For running EUnit tests
                false ->
                    "../../../priv/dlock.term"
            end;
        Priv ->
            filename:join([Priv, "dlock.term"])
    end,
    {ok, [AllConfig]} = file:consult(File),
    AllConfig.
