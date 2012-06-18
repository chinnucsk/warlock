%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server console
%%%
%%% Used to setup connections with other nodes
%%%
%%% @end
%%%
%%% @since : 14 June 2012
%%% @end
%%%-------------------------------------------------------------------
%% TODO: Write specs
-module(server_console).

%% -----------------------------------------------------------------
%% Public interface
%% -----------------------------------------------------------------
-export([join/1]).

%% -----------------------------------------------------------------
%% Private macros
%% -----------------------------------------------------------------
-define(SELF, node()).

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Join an existing cluster
%%-------------------------------------------------------------------
join([NodeStr]) ->
    Node = str_to_node(NodeStr),
    %% Connect to the node
    case net_adm:ping(Node) of
        pang ->
            {error, not_reachable};
        pong ->
            %% Register the cluster members locally and add self to all the
            %% members of the cluster and
            %% Increase the cluster_size
            Members = rpc:call(Node, consensus_state, get_members, []),
            CSize = rpc:call(Node, consensus_state, get_cluster_size, []),
            lists:foreach(fun(Member) ->
                                  consensus_state:set_node_status(Member,
                                                                  valid),
                                  rpc:call(Member, consensus_state,
                                           set_node_status, [?SELF, valid]),
                                  rpc:call(Member, consensus_state,
                                           set_cluster_size, [CSize+1])
                          end, Members),
            consensus_state:set_cluster_size(CSize+1)
    end.



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%% Convert string to node
str_to_node(Node) when is_atom(Node) ->
    str_to_node(atom_to_list(Node));
str_to_node(NodeStr) ->
    case string:tokens(NodeStr, "@") of
        [NodeName] ->
            %% Node name only; no host name. If the local node has a hostname,
            %% append it
            case node_hostname() of
                [] ->
                    list_to_atom(NodeName);
                Hostname ->
                    list_to_atom(NodeName ++ "@" ++ Hostname)
            end;
        _ ->
            list_to_atom(NodeStr)
    end.

node_hostname() ->
    NodeStr = atom_to_list(node()),
    case string:tokens(NodeStr, "@") of
        [_NodeName, Hostname] ->
            Hostname;
        _ ->
            []
    end.
