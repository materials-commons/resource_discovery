%%% ===================================================================
%%% @doc rd_host_server
%%% ===================================================================

-module(rd_host_server).
-behaviour(gen_stomp).

-include("resource.hrl").
-include_lib("handyman/include/jsonerl.hrl").

%% API
-export([start_link/6, fetch/0, delete_resource/1, add_resource/1,
            stop/0, stop/1]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-export([convertrecord/1]).

-define(SERVER, ?MODULE).

-record(state,
    {
        rd,
        host :: string(),
        command_queue :: string(),
        broadcast_queue :: string(),
        ss_queue = "/topic/rd_host_startup_shutdown" :: string()
    }).

convertrecord(R) ->
    ?record_to_json(hostevent, R).

%% ===================================================================
%% API
%% ===================================================================

%% @doc starts the main server that holds the real view of the resources
start_link(StompHost, Port, Username, Password, ResourceHost, Resources) ->
    HostBroadcastTopic = string:concat("/topic/rd_", ResourceHost),
    HostCommandQueue = string:concat("/queue/rd_command_", ResourceHost),
    gen_stomp:start_link({local, ?SERVER}, ?MODULE, StompHost, Port, Username, Password,
        [{HostCommandQueue, []}],
        [HostBroadcastTopic, HostCommandQueue, ResourceHost, Resources]).

fetch() ->
    gen_stomp:call(?SERVER, fetch).

stop() ->
    gen_stomp:cast(?SERVER, stop).

stop(Pid) ->
    gen_stomp:cast(Pid, stop).

delete_resource(#resource{} = Resource) ->
    gen_stomp:cast(?SERVER, {delete_resource, Resource}).

add_resource(#resource{} = Resource) ->
    gen_stomp:cast(?SERVER, {add_resource, Resource}).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @doc No resources specified, need to query host for them.
init([HostBroadcastTopic, HostCommandQueue, ResourceHost, Resources]) ->
    Rd = rd_resource_db:new(),

    %% Do initialization outside of init.
    gen_stomp:cast(self(), {startup, Resources}),
    rd_store:insert(ResourceHost, self()),

    State = #state{ command_queue = HostCommandQueue, host = ResourceHost,
                    broadcast_queue = HostBroadcastTopic, rd = Rd},

    {ok, State}.

%% @doc Retrieve all resources
handle_call(fetch, _From, #state{rd = Rd} = State) ->
    Resources = rd_resource_db:all(Rd),
    {reply, {ok, Resources}, State}.

handle_cast([{message, Message}, {queue, Queue}],
        #state{broadcast_queue = BroadcastQueue, rd = Rd} = State) ->
    handle_message(Rd, Message, Queue, BroadcastQueue),
    {noreply, State};

handle_cast({add_resource, Resource},
        #state{rd = Rd, broadcast_queue = BroadcastQueue} = State) ->
    add_resources(Rd, [Resource]),
    broadcast_resource(Resource, BroadcastQueue),
    {noreply, State};

%% @doc Add resources that were specified in startup.
handle_cast({startup, Resources},
                #state{rd = Rd, ss_queue = SSQueue, host = Host} = State) ->
    io:format("rd_host_server startup~n"),
    add_resources(Rd, Resources),
    UpEvent = handyterm:term_to_string(#hostevent{host = Host, event = up}),
    gen_stomp:send(SSQueue, UpEvent, []),
    %% Wait two minutes before broadcasting our resources
    TimeToWait = 2 * 2 * 1000,
    {noreply, State, 2000};

%% @doc Stop the server.
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{broadcast_queue = BroadcastQueue, rd = Rd} = State) ->
    io:format("rd_host_server handle_info timeout~n",[]),
    broadcast_resources(BroadcastQueue, Rd),
    {noreply, State};

%% @doc On timeout go out and query for the resources.
handle_info(_Message, State) ->
    %% Error log bad message
    {noreply, State}.

terminate(_Reason, #state{ss_queue = SSQueue, host = Host}) ->
    % Remove mapping for process
    DownEvent = handyterm:term_to_string(#hostevent{host = Host, event = down}),
    gen_stomp:send(SSQueue, DownEvent, []),
    rd_store:delete_by_pid(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

%% @doc handle messages on the message queues
handle_message(Rd, [{type, "MESSAGE"},
                    {header, _Header}, {body, Body}],
                _Queue, BroadcastQueue) ->
    handle_commands(Body, BroadcastQueue, Rd);

%% @doc Handle non-message types
handle_message(_Rd, _Message, _Queue, _BroadcastQueue) ->
    ok.

%% @doc Add a list of resources to our database of resources
add_resources(_Rd, []) ->
    ok;
add_resources(Rd, [R|T]) ->
    rd_resource_db:insert(Rd, R),
    add_resources(Rd, T).

%% @doc Handle different types of commands.
handle_commands("RESOURCES", BroadcastQueue, Rd) ->
    broadcast_resources(BroadcastQueue, Rd),
    ok;
handle_commands(Command, Queue, _Rd) ->
    % Add error logging here.
    io:format("handle_commands fall through ~p:~p~n", [Command, Queue]),
    ok.

%% @doc Send each resource one at a time.
broadcast_resources(BroadcastQueue, Rd) ->
    Resources = rd_resource_db:all(Rd),
    ResourcesAsString = handyterm:term_to_string(Resources),
    gen_stomp:send(BroadcastQueue, ResourcesAsString, []).

%% @doc Transform resource to list of Records, turn into string and send
broadcast_resource(Resource, BroadcastQueue) ->
    %% Other side expects a list of records.
    RecordsListAsString = handyterm:term_to_string([Resource]),
    gen_stomp:send(BroadcastQueue, RecordsListAsString, []).