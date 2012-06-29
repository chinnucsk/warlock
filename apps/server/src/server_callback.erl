%%%-------------------------------------------------------------------
%%% @copyright
%%% @end
%%%-------------------------------------------------------------------
%%% @author Sukumar Yethadka <sukumar@thinkapi.com>
%%%
%%% @doc Server callback
%%%
%%% Functionalities:
%%% 1. Depending on the mode (active), execute or queue the decisions
%%% 2. Allow subcribers, who will receive the decisions
%%%
%%% @end
%%%
%%% @since : 04 June 2012
%%% @end
%%%-------------------------------------------------------------------
-module(server_callback).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         handle/1,
         trig_active/0, set_inactive/0, is_active/0,
         add_subscriber/1, remove_subscriber/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% -----------------------------------------------------------------
%% Include files and private macros
%% -----------------------------------------------------------------
-include_lib("util/include/common.hrl").

%% Check previously empty queue for decisions every X seconds
-define(SELF, self()).
-define(MARKER, sync_marker).
-record(state, {
            % Flag to determine where the decisions are sent
            % true - all decisions are committed to subcribers and db module
            % false - all decisions are queued
            active = true,

            % Queue to store decisions when active = false
            queue = queue:new(),

            % All decisions executed out of the queue is also sent to this node
            subscriber = []
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {error, _} | {ok, atom()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callback function is of the format [Command, Data]
-spec handle(list()) -> term().
handle([Func, Data]) ->
    gen_server:call(?MODULE, {Func, Data}).

-spec set_inactive() -> ok.
set_inactive() ->
    gen_server:call(?MODULE, set_inactive).

-spec trig_active() -> ok.
trig_active() ->
    gen_server:call(?MODULE, trig_active).

-spec is_active() -> boolean().
is_active() ->
    gen_server:call(?MODULE, is_active).

-spec add_subscriber(node()) -> ok.
add_subscriber(Node) ->
    gen_server:call(?MODULE, {add_subscriber, Node}).

-spec remove_subscriber(node()) -> ok.
remove_subscriber(Node) ->
    gen_server:call(?MODULE, {remove_subscriber, Node}).

%% This is the first connection from the client
%% Send data about the cluster to the client
% TODO: Implement this

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% Initialize gen_server
%% ------------------------------------------------------------------
init([]) ->
    ?LDEBUG("Starting " ++ erlang:atom_to_list(?MODULE)),
    {ok, #state{}}.

%% ------------------------------------------------------------------
%% gen_server:handle_call/3
%% ------------------------------------------------------------------
handle_call({get, Key}, _From,
            #state{active=Active}=State) ->
    case Active of
        true ->
            Reply = db:get(Key),
            {reply, Reply, State};
        false ->
            % TODO: Should this be an error instead?
            Reply = {ok, inactive},
            {reply, Reply, State}
    end;
handle_call({set, [Key, Value]}=Msg, _From,
            #state{active=Active, queue=Queue}=State) ->
    case Active of
        true ->
            Reply = db:set([Key, Value]),
            {reply, Reply, State};
        false ->
            NewQueue = queue:in(Msg, Queue),
            Reply = {ok, queued},
            {reply, Reply, State#state{queue=NewQueue}}
    end;
handle_call({del, Key}=Msg, _From,
            #state{active=Active, queue=Queue}=State) ->
    case Active of
        true ->
            Reply = db:del(Key),
            {reply, Reply, State};
        false ->
            NewQueue = queue:in(Msg, Queue),
            Reply = {ok, queued},
            {reply, Reply, State#state{queue=NewQueue}}
    end;
handle_call(is_active, _From, #state{active=Active}=State) ->
    Reply = Active,
    {reply, Reply, State};
handle_call(set_inactive, _From, State) ->
    Reply = ok,
    {reply, Reply, State#state{active=false}};
handle_call(trig_active, _From, #state{queue=Queue}=State) ->
    % The subscriber is now in the cluster
    % Create a marker in the queue and start processing queue
    Msg = ?MARKER,
    NewQueue = queue:in(Msg, Queue),
    gen_server:cast(?MODULE, process_queue),
    Reply = ok,
    {reply, Reply, State#state{queue=NewQueue}};
handle_call({add_subscriber, Node}, _From, State) ->
    {reply, ok, State#state{subscriber=Node}};
handle_call({remove_subscriber, _Node}, _From, State) ->
    {reply, ok, State#state{subscriber=[]}};
handle_call(queue_processed, _From, State) ->
    % Sender has completed its queue, start processing local queue
    gen_server:cast(?MODULE, process_queue),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_cast/2
%% ------------------------------------------------------------------
%% Process the queue asynchronously
handle_cast(process_queue,
            #state{active=false, queue=Queue, subscriber=Subscriber}=State) ->
    case queue:out(Queue) of
        {{value, ?MARKER}, NewQueue} ->
            % Found marker. Inform subscriber and remove it
            send_subscriber(Subscriber, queue_processed),
            gen_server:cast(?MODULE, process_queue),
            {noreply, State#state{queue=NewQueue, subscriber=[]}};
        {{value, {Func, Data}}, NewQueue} ->
            % Process queue. Exec on local, on subscriber and continue
            db:Func(Data),
            send_subscriber(Subscriber, {Func, Data}),
            gen_server:cast(?MODULE, process_queue),
            {noreply, State#state{queue=NewQueue}};
        {empty, Queue} ->
            % Queue processing complete, back to normal state
            {noreply, State#state{queue=Queue, subscriber=[], active=true}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% gen_server:handle_info/2
%% ------------------------------------------------------------------
handle_info(process_queue, State) ->
    gen_server:cast(?MODULE, process_queue),
    {noreply, State};
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
%% Send the decision as a message to the subscriber
send_subscriber(Subscriber, Msg) when is_atom(Subscriber) ->
    % TODO: Use cast?
    gen_server:call({?MODULE, Subscriber}, Msg);
send_subscriber(_Subscriber, _Msg) ->
    ok.