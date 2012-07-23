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
-module(war_util_conf).

%% ------------------------------------------------------------------
%% Include files
%% ------------------------------------------------------------------
-include("war_common.hrl").

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
    proplists:get_value(Key, war_util_conf:get(Group)).

-spec get(Key::atom(), Group::atom(), Default::term()) -> term().
get(Key, Group, Default) ->
    proplists:get_value(Key, war_util_conf:get(Group), Default).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%% TODO: Find a better solution for the config file problem. Very bad code!
get_all() ->
    File = case code:priv_dir(?APP) of
        {error,bad_name} ->
            get_config_file();
        Priv ->
            filename:join([Priv, "warlock.term"])
    end,
    {ok, [AllConfig]} = file:consult(File),
    AllConfig.

get_config_file() ->
    %% In release folder
    Try1 = "priv/warlock.term",
    % Assuming the app is being run from the app folder
    Try2 = "../../priv/warlock.term",
    % For running EUnit tests
    Try3 = "../../../priv/warlock.term",

    case filelib:is_file(Try1) of
        true ->
            Try1;
        false ->
            case filelib:is_file(Try2) of
                true ->
                    Try2;
                false ->
                    case filelib:is_file(Try3) of
                        true ->
                            Try3;
                        false ->
                            {error, file_not_found}
                    end
            end
    end.

