%%% ===================================================================
%%% @doc rd_host_resource takes care of caching each hosts resources.
%%% The server takes care of time to live, gossiping with the remote
%%% host, and monitoring for change events for that host.

%%% The service has a TTL. At expiration it verifies the resources for
%%% the host it is tracking.
%%% ===================================================================

-module(rd_host_resource).
-behaviour(gen_stomp).

-include("resource.hrl").
-include_lib("handyman/include/jsonerl.hrl").

%% API
-export([start_link/6, start_link/7, start_self_link/6, start/1, start/2,
            start/3, fetch/1, update/1, delete_resource/2, add_resource/2,
            st/0, stself/0, jt/0]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
            code_change/3]).

-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)). % 1 Day by DEFAULT_LEASE_TIME

-type seconds() :: integer().

-record(state,
    {
        rd,
        count,
        host :: string(),
        command_queue :: string(),
        broadcast_queue :: string(),
        lease_time :: seconds(),
        start_time :: seconds()
    }).

%% ===================================================================
%% API
%% ===================================================================

st() ->
    start_link("141.212.111.19", 61613, "guest", "guest", "141.212.111.19", 10).

stself() ->
    start_self_link("141.212.111.19", 61613, "guest", "guest",
        [#resource{host="a",type="a",name="a",attrs="a"}], "141.212.111.19").

jt() ->
    Jr = ?record_to_json(resource, #resource{host="a", type="a", name="a", attrs="a"}),
    io:format("~s~n", [Jr]),
    J = ?json_to_record(resource, Jr).
    %Rs = jsonerl:decode(J).

%% @doc start the server
start_link(StompHost, Port, Username, Password, ResourceHost, LeaseTime) ->
    start_link(StompHost, Port, Username, Password, [], ResourceHost, LeaseTime).

start_link(StompHost, Port, Username, Password, Resources, ResourceHost, LeaseTime) ->
    HostBroadcastTopic = string:concat("/topic/rd_", ResourceHost),
    HostCommandQueue = string:concat("/queue/rd_command_", ResourceHost),
    gen_stomp:start_link(?MODULE, StompHost, Port, Username, Password,
        [{HostBroadcastTopic, []}],
        [HostBroadcastTopic, HostCommandQueue, Resources, LeaseTime]).

start_self_link(StompHost, Port, Username, Password, Resources, ResourceHost) ->
    HostBroadcastTopic = string:concat("/topic/rd_", ResourceHost),
    HostCommandQueue = string:concat("/queue/rd_command_", ResourceHost),
    gen_stomp:start_link(?MODULE, StompHost, Port, Username, Password,
        [{HostCommandQueue, []}],
        [HostBroadcastTopic, HostCommandQueue, Resources, infinity]).


start(Host, LeaseTime) ->
    rd_sup:start_child(Host, [], LeaseTime).

start(Host) ->
    start(Host, ?DEFAULT_LEASE_TIME).

start(Host, Resources, LeaseTime) ->
    rd_sup:start_child(Host, Resources, LeaseTime).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

delete_resource(Pid, #resource{} = Resource) ->
    gen_server:cast(Pid, {delete_resource, Resource}).

update(Pid) ->
    gen_server:cast(Pid, update).

add_resource(Pid, #resource{} = Resource) ->
    gen_server:cast(Pid, {add_resource, Resource}).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @doc No resources specified, need to query host for them.
init([HostBroadcastTopic, HostCommandQueue, Resources, LeaseTime]) ->
    Now = seconds_now(),
    Rd = rd_resource:new(),
    %% Do initialization outside of init.
    gen_server:cast(self(), {startup, Resources}),
    %gen_server:cast(self(), {startup2, "Hello"}),
    State = #state{lease_time = LeaseTime, start_time = Now,
                    command_queue = HostCommandQueue,
                    broadcast_queue = HostBroadcastTopic, rd = Rd, count = 0},
    TimeLeft = time_left(Now, LeaseTime),
    io:format("init timeout set to: ~p~n", [TimeLeft]),
    {ok, State, TimeLeft}. %, time_left(Now, LeaseTime)}.


%% @doc Retrieve all resources
handle_call(fetch, _From,
        #state{lease_time = LeaseTime, start_time = StartTime, rd = Rd} = State) ->
    Resources = rd_resource:all(Rd),
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, Resources}, State, TimeLeft}.

%% @doc
handle_cast(update,
        #state{lease_time = LeaseTime, start_time = StartTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

handle_cast([{message, Message}, {queue, Queue}],
        #state{start_time = StartTime, lease_time = LeaseTime,
                broadcast_queue = BroadcastQueue, rd = Rd} = State) ->
    handle_message(Rd, Message, Queue, BroadcastQueue),
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

handle_cast({add_resource, Resource},
        #state{start_time = StartTime, lease_time = LeaseTime,
                rd = Rd, broadcast_queue = BroadcastQueue} = State) ->
    add_resources(Rd, [Resource]),
    broadcast_resource(Resource, BroadcastQueue),
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

%% @doc complete startup when no resources were given
handle_cast({startup, []}, #state{start_time = StartTime,
                                lease_time = LeaseTime,
                                command_queue = CommandQueue} = State) ->
    io:format("Empty start~n", []),
    gen_stomp:send(CommandQueue, "RESOURCES", []),
    {noreply, State, time_left(StartTime, LeaseTime)};

handle_cast({startup, Resources},
        #state{start_time = StartTime,
                lease_time = LeaseTime, rd = Rd} = State) ->
    io:format("handle_start {startup, Resources}", []),
    add_resources(Rd, Resources),
    io:format("startup time_left: ~p~n", [time_left(StartTime, LeaseTime)]),
    {noreply, State, time_left(StartTime, LeaseTime)};

handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{lease_time = LeaseTime, rd = Rd,
                            command_queue = CommandQueue, count = Count} = State) ->
    % On timeout we go and query for the resources.
    io:format("timeout called~n", []),
    Now = seconds_now(),
    NewTimeout = time_left(Now, LeaseTime),
    rd_resource:delete_all(Rd),
    gen_stomp:send(CommandQueue, "RESOURCES", []),
    io:format("timeout setting ~p time_left: ~p~n", [Count, NewTimeout]),
    {noreply, State#state{count = Count + 1}, NewTimeout}.

terminate(_Reason, _State) ->
    % Remove key from ETS
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

%% @doc TTL never expires
time_left(_StartTime, infinity) ->
    infinity;
%% @doc Convert TTL to milliseconds
time_left(StartTime, LeaseTime) ->
    CurrentTimeInSeconds = seconds_now(),
    TimeElapsed = CurrentTimeInSeconds - StartTime,
    lease_time_left_in_milliseconds(LeaseTime, TimeElapsed).

%% @doc Get seconds now
seconds_now() ->
    Now = calendar:local_time(),
    calendar:datetime_to_gregorian_seconds(Now).

%% @doc convert seconds to milliseconds
lease_time_left_in_milliseconds(LeaseTime, TimeElapsed) ->
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time -> Time * 1000 % Convert to milliseconds
    end.

%% @doc handle messages on the message queues
handle_message(Rd, [{type, "MESSAGE"}, {header, _Header}, {body, Body}], Queue, BroadcastQueue) ->
    io:format("handle_message Queue: ~p~n", [Queue]),
    io:format("  Message: ~p~n", [Body]),
    case from_broadcast_queue(Queue) of
        true -> add_message_as_resource(Rd, Body);
        false -> handle_commands(Body, BroadcastQueue, Rd)
    end;
%% @doc Handle non-message types
handle_message(_Rd, _Message, _Queue, _BroadcastQueue) ->
    ok.

%% @doc check if its from the topic queue
from_broadcast_queue(QueueName) ->
    string:str(QueueName, "/topic") =/= 0.

add_resources(_Rd, []) ->
    ok;
add_resources(Rd, [R|T]) ->
    rd_resource:insert(Rd, R),
    add_resources(Rd, T).

add_message_as_resource(Rd, Message) ->
    R = ?json_to_record(resource, Message),
    rd_resource:insert(Rd, R).

handle_commands("RESOURCES", BroadcastQueue, Rd) ->
    io:format("handle_commands RESOURCES~n", []),
    broadcast_resources(BroadcastQueue, Rd),
    %io:format("sending: ~p~n", [binary_to_list(term_to_binary(Resources))]),
    %gen_stomp:send(BroadcastQueue, binary_to_list(term_to_binary(Resources)), []),
    ok;
handle_commands(Command, Queue, _Rd) ->
    io:format("handle_commands fall through ~p:~p~n", [Command, Queue]),
    ok.

broadcast_resources(BroadcastQueue, Rd) ->
    Resources = rd_resource:all(Rd),
    lists:foreach(
        fun (Resource) ->
            broadcast_resource(Resource, BroadcastQueue)
        end, Resources).

broadcast_resource(Resource, BroadcastQueue) ->
    J = ?record_to_json(resource, Resource),
    gen_stomp:send(BroadcastQueue, J, []).


