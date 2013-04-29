%%% ===================================================================
%%% @doc Implements caching each external hosts resources.
%%%
%%%      The server takes care of time to live, gossiping with the remote
%%%      host, and monitoring for change events for that host.
%%%
%%%      The service has a TTL. At expiration it verifies the resources for
%%%      the host it is tracking.
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

%%% ===================================================================
%%% @doc rd_host_resource takes care of caching each hosts resources.
%%% The server takes care of time to live, gossiping with the remote
%%% host, and monitoring for change events for that host.

%%% The service has a TTL. At expiration it verifies the resources for
%%% the host it is tracking.
%%% ===================================================================

-module(rd_resource_server).
-behaviour(gen_stomp).

-include("resource.hrl").
-include_lib("handyman/include/jsonerl.hrl").

%% API
-export([start_link/6, start_link/7, start/1, start/2, fetch/1,
            update/1, stop/1]).


%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
            code_change/3]).

-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)). % 1 Day by DEFAULT_LEASE_TIME

-type seconds() :: integer().

-record(state,
    {
        rd,
        host :: string(),
        command_queue :: string(),
        broadcast_queue :: string(),
        lease_time :: seconds(),
        start_time :: seconds()
    }).

%% ===================================================================
%% API
%% ===================================================================

%% @doc start the server
start_link(StompHost, Port, Username, Password, LeaseTime, ResourceHost) ->
    start_link(StompHost, Port, Username, Password, LeaseTime, ResourceHost, []).

start_link(StompHost, Port, Username, Password, LeaseTime, ResourceHost, Resources) ->
    HostBroadcastTopic = string:concat("/topic/rd_", ResourceHost),
    HostCommandQueue = string:concat("/queue/rd_command_", ResourceHost),
    gen_stomp:start_link(?MODULE, StompHost, Port, Username, Password,
        [{HostBroadcastTopic, []}],
        [HostBroadcastTopic, HostCommandQueue, Resources, LeaseTime]).

start(Host) ->
    start(Host, []).

start(Host, Resources) ->
    rd_resource_sup:start_child(Host, Resources).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

stop(Pid) ->
    gen_server:cast(Pid, stop).

update(Pid) ->
    gen_server:cast(Pid, update).


%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @doc No resources specified, need to query host for them.
init([HostBroadcastTopic, HostCommandQueue, Resources, LeaseTime]) ->
    Now = lease:seconds_now(),
    Rd = rd_resource_db:new(),

    %% Do initialization outside of init.
    gen_server:cast(self(), {startup, Resources}),

    State = #state{lease_time = LeaseTime, start_time = Now,
                    command_queue = HostCommandQueue,
                    broadcast_queue = HostBroadcastTopic, rd = Rd},
    TimeLeft = lease:time_left(Now, LeaseTime),
    {ok, State, TimeLeft}.


%% @doc Retrieve all resources
handle_call(fetch, _From,
        #state{lease_time = LeaseTime, start_time = StartTime, rd = Rd} = State) ->
    Resources = rd_resource_db:all(Rd),
    TimeLeft = lease:time_left(StartTime, LeaseTime),
    {reply, {ok, Resources}, State, TimeLeft}.

%% @doc
handle_cast(update,
        #state{lease_time = LeaseTime, start_time = StartTime,
                command_queue = CommandQueue} = State) ->
    gen_stomp:send(CommandQueue, "RESOURCES", []),
    TimeLeft = lease:time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

handle_cast([{message, Message}, {queue, _Queue}],
        #state{start_time = StartTime, lease_time = LeaseTime, rd = Rd} = State) ->
    handle_message(Rd, Message),
    TimeLeft = lease:time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

%% @doc Complete startup when no resources were given by asking for
%% for the resources from the server.
handle_cast({startup, []}, #state{start_time = StartTime,
                                lease_time = LeaseTime,
                                command_queue = CommandQueue} = State) ->
    gen_stomp:send(CommandQueue, "RESOURCES", []),
    {noreply, State, lease:time_left(StartTime, LeaseTime)};

%% @doc Add resources that were specified in startup.
handle_cast({startup, Resources},
        #state{start_time = StartTime,
                lease_time = LeaseTime, rd = Rd} = State) ->
    add_resources(Rd, Resources),
    {noreply, State, lease:time_left(StartTime, LeaseTime)};

%% @doc Stop the server.
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @doc On timeout go out and query for the resources.
handle_info(timeout, #state{lease_time = LeaseTime, rd = Rd,
                            command_queue = CommandQueue} = State) ->
    % On timeout we go and query for the resources.
    Now = lease:seconds_now(),
    NewTimeout = lease:time_left(Now, LeaseTime),
    rd_resource_db:delete_all(Rd),
    gen_stomp:send(CommandQueue, "RESOURCES", []),
    {noreply, State#state{start_time = Now}, NewTimeout}.

terminate(_Reason, _State) ->
    % Remove mapping for process
    rd_store:delete_by_pid(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

%% @doc handle messages on the message queues
handle_message(Rd, [{type, "MESSAGE"},
                    {header, _Header}, {body, Body}]) ->
    %% Resources comes in as a Erlang term turned into a string,
    %% turn back into a list of records and add them to database.
    RList = handyterm:string_to_term(Body),
    lists:foreach(
        fun (Resource) ->
            rd_resource_db:insert(Rd, Resource)
        end, RList);

%% @doc Handle non-message types
handle_message(_Rd, _Message) ->
    ok.

%% @doc Add a list of resources to our database of resources
add_resources(_Rd, []) ->
    ok;
add_resources(Rd, [R|T]) ->
    rd_resource_db:insert(Rd, R),
    add_resources(Rd, T).





