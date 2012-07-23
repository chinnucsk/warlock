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
-export([join/1, repl/1, leave/1, remove/1, replace/1]).

%% -----------------------------------------------------------------
%% Public functions
%% -----------------------------------------------------------------

%%-------------------------------------------------------------------
%% @doc
%% Join an existing cluster
%%-------------------------------------------------------------------
-spec join([string()]) -> ok.
join([NodeStr]) ->
    system_check(),
    Node = str_to_node(NodeStr),
    %% Connect to the node
    case net_adm:ping(Node) of
        pang ->
            throw({error, not_reachable});
        pong ->
            consensus:rcfg_join(Node)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Replicate from some (non-master) node in the cluster
%%-------------------------------------------------------------------
-spec repl([string()]) -> ok.
repl([NodeStr]) ->
    system_check(),
    Node = str_to_node(NodeStr),
    %% Connect to the node
    case net_adm:ping(Node) of
        pang ->
            throw({error, not_reachable});
        pong ->
            server:repl(Node)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Current node tries to leaves the cluster
%%-------------------------------------------------------------------
-spec leave([]) -> ok.
leave([]) ->
    consensus:rcfg_leave().

%%-------------------------------------------------------------------
%% @doc
%% Remove given member from the cluster
%%-------------------------------------------------------------------
-spec remove([string()]) -> ok.
remove([NodeStr]) ->
    system_check(),
    Node = str_to_node(NodeStr),
    %% Connect to the node
    case net_adm:ping(Node) of
        pang ->
            throw({error, not_reachable});
        pong ->
            consensus:rcfg_remove(Node)
    end.

%%-------------------------------------------------------------------
%% @doc
%% Replace given member from the cluster using meta info from seed node
%% Size of the cluster remains same
%%-------------------------------------------------------------------
-spec replace([string() | string()]) -> ok.
replace([TargetNodeStr, SeedNodeStr]) ->
    SeedNode = str_to_node(SeedNodeStr),
    TargetNode = str_to_node(TargetNodeStr),
    %% Connect to the node
    case net_adm:ping(SeedNode) of
        pang ->
            throw({error, not_reachable});
        pong ->
            % Remove target node from the cluster
            rpc:call(SeedNode, consensus, rcfg_remove, [TargetNode]),
            % If TargetNode is self, then consensus app may be down
            replace_self(TargetNode),
            % Replicate using seed node
            server:repl(SeedNode)
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

%% Checks if all the parts of the system are running
system_check() ->
    case server:ping() of
        pong ->
            ok;
        _ ->
            throw({error, server_unavailable})
    end,
    case server:ping_backend() of
        pong ->
            ok;
        _ ->
            throw({error, backend_unavailable})
    end,
    case server:ping_service() of
        pong ->
            ok;
        _ ->
            throw({error, consensus_unavailable})
    end.

replace_self(Target) ->
    case Target =:= node() of
        true ->
            % Wait for cluster to sync, start app, give it time to start
            timer:sleep(1000),
            application:start(consensus),
            timer:sleep(100);
        false ->
            ok
    end.

