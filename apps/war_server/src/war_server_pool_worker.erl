%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server worker in pool
%%%
%%% We use one pool worker per client
%%%
%%% Server worker creates a timer and a unique reference for every command.
%%% Timeouts need to be greater than that of the consensus module (commander).
%%%
%%% The erlang redis protocol is mostly from eredis and
%%% https://github.com/athoune/erlang-redis-protocol
%%%
%%% @end
%%%
%%% @since : 01 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(war_server_pool_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Include files and macros
%% --------------------------------------------------------------------
-include_lib("war_util/include/war_common.hrl").
-include_lib("eredis/include/eredis.hrl").

-record(state, {
                %% Connection socket1
                socket,

                %% Connection protocol
                transport,

                %% Other options
                opts,

                %% Command parser
                parser_state
}).

%% How long the process waits for the reply from consensus module
-define(TIMEOUT, 5000).
-define(CALL_TIMEOUT, 6000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link(ListenerPid::pid(), Socket::pid(),
                 Transport::pid(), Opts::list()) -> {error, _} | {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Opts], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([_ListenerPid, Socket, Transport, Opts]) ->
    ok = Transport:setopts(Socket, [binary, {active, once}]),
    {ok, #state{socket=Socket,
                transport=Transport,
                opts=Opts,
                parser_state=eredis_parser:init()}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% Response is sent by consensus client
handle_cast({response, null, Result},
            #state{socket=Socket, transport=Transport} = State) ->
    Transport:send(Socket, redis_protocol_encoder:encode(Result)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info({tcp, _Socket, Bs}, #state{transport=Transport}=State) ->
    NewState = handle_request(Bs, State),
    Transport:setopts(State#state.socket, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:terminate/2
%% ------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% ------------------------------------------------------------------
%% gen_server:code_change/3
%% ------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_operation(Type, Cmd, ClientId) ->
    #dop{type = Type,
         module = war_server_callback,
         function = handle,
         args = Cmd,
         client = ClientId}.

-spec handle_request(Data::binary(), State::#state{}) -> NewState::#state{}.
%% Handle the request coming from client. This includes parsing
%% and replying to the correct client, handling partial responses,
%% handling too much data and handling continuations.
handle_request(Data, #state{parser_state=ParserState,
                             socket=Socket, transport=Transport} = State) ->
    case eredis_parser:parse(ParserState, Data) of
        {ok, Return, NewParserState} ->
            request(Return, Socket, Transport),
            State#state{parser_state=NewParserState};
        {ok, Return, Rest, NewParserState} ->
            request(Return, Socket, Transport),
            handle_request(Rest, State#state{parser_state = NewParserState});
        {continue, NewParserState} ->
            State#state{parser_state=NewParserState};
        {error,unknown_response} ->
            case get_newline_pos(Data) of
                undefined ->
                    State;
                Pos ->
                    <<Value:Pos/binary, ?NL, Rest/binary>> = Data,
                    Ret = redis_auth(Value),
                    Transport:send(Socket, redis_protocol_encoder:encode(Ret)),
                    case Rest of
                        <<>> ->
                            State;
                        _ ->
                            handle_request(Rest, State)
                    end
            end
    end.

redis_auth(<<"AUTH", _Password/binary>>) ->
    % TODO: Authenticate password
    ok;
redis_auth(<<"SELECT", _Db/binary>>) ->
    ok;
redis_auth(Data) ->
    ?LINFO("Unknown data:: ~p", [Data]),
    ok.

get_newline_pos(B) ->
    case re:run(B, ?NL) of
        {match, [{Pos, _}]} -> Pos;
        nomatch -> undefined
    end.

request([?LOCALB | Cmd], Socket, Transport) ->
    Return = war_consensus:propose(get_operation(?LOCAL, Cmd, null)),
    Transport:send(Socket, redis_protocol_encoder:encode(Return));
request([?CLUSTERB | Cmd], _Socket, _Transport) ->
    Operation = get_operation(?CLUSTER, Cmd, {self(), null}),
    war_consensus:propose(Operation).
