%%% ===================================================================
%%% @doc Tracks the resources for this host.
%%%
%%% Copyright (c) 2013, Regents of the University of Michigan.
%%% All rights reserved.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% ===================================================================

-module(rd_host_server).
-behaviour(gen_stomp).

-include("resource.hrl").
-include("rd.hrl").
-include_lib("handyman/include/jsonerl.hrl").

%% API
-export([start_link/6, fetch/0, delete_resource/1, add_resource/1,
            stop/0, stop/1]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-export([start_link/0]).

-define(SERVER, ?MODULE).

-define(DEFAULT_WAIT, (2*60*1000)). % Wait 2 minutes

-record(state,
    {
        rd,
        host :: string(),
        command_queue :: string(),
        broadcast_queue :: string(),
        ss_queue :: string()
    }).

start_link() ->
    rd_store:init(),
    start_link("localhost", 61613, "guest", "guest", "141.212.111.19", []).

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
                    broadcast_queue = HostBroadcastTopic, rd = Rd,
                    ss_queue = ?SS_QUEUE},

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
    add_resources(Rd, Resources),
    UpEvent = handyterm:term_to_string(#hostevent{host = Host, event = up}),
    %% Wait before broadcasting our resources
    TimeToWait = handyconfig:get_env_default(resource_discovery, rd_host_wait, ?DEFAULT_WAIT),
    gen_stomp:send(SSQueue, UpEvent, [], TimeToWait),
    {noreply, State, TimeToWait};

%% TimeToWait Stop the server.
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{broadcast_queue = BroadcastQueue, rd = Rd} = State) ->
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