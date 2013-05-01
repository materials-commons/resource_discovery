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
-export([start_link/6, fetch/0, delete_resources/1, add_resource/1,
            update_resources/1, stop/0, stop/1]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_WAIT, (2*60*1000)). % Wait 2 minutes

-record(state,
    {
        rd :: rd_resource_db:descriptor(), % descriptor to resource_db
        host :: string(), % Our host
        ss_queue :: string() %% Startup/Shutdown queue.
    }).

%% ===================================================================
%% API
%% ===================================================================

%% @doc starts the main server that holds the real view of the resources
start_link(StompHost, Port, Username, Password, ResourceHost, Resources) ->
    ResourceRd = #resource{host = ResourceHost, name = "resource_discovery",
                            type = "resource_discovery"},
    gen_stomp:start_link({local, ?SERVER}, ?MODULE, StompHost, Port,
        Username, Password, [], [ResourceHost, [ResourceRd | Resources]]).

%% @doc Fetch resources for this server.
-spec fetch() -> [resource()].
fetch() ->
    gen_stomp:call(?SERVER, fetch).

%% @doc Stops server.
-spec stop() -> ok.
stop() ->
    gen_stomp:cast(?SERVER, stop).

%% @doc You can also stop this server using its pid.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_stomp:cast(Pid, stop).

%% @doc Delete a resource.
-spec delete_resources([resource()]) -> ok.
delete_resources(Resources) ->
    gen_stomp:cast(?SERVER, {delete_resources, Resources}).

%% @doc Update a resource
-spec update_resources([resource()]) -> ok.
update_resources(Resources) ->
    gen_stomp:cast(?SERVER, {update_resources, Resources}).

%% @doc Add a resource.
-spec add_resource(resource()) -> ok.
add_resource(#resource{} = Resource) ->
    gen_stomp:cast(?SERVER, {add_resource, Resource}).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @doc No resources specified, need to query host for them.
init([ResourceHost, Resources]) ->
    Rd = rd_resource_db:new(),

    %% Do initialization outside of init.
    gen_stomp:cast(self(), {startup, Resources}),
    rd_store:insert(ResourceHost, self()),

    State = #state{ host = ResourceHost, rd = Rd,ss_queue = ?SS_QUEUE},
    {ok, State}.

%% @doc Retrieve all resources
handle_call(fetch, _From, #state{rd = Rd} = State) ->
    Resources = rd_resource_db:all(Rd),
    {reply, {ok, Resources}, State}.

handle_cast({add_resource, Resource}, #state{rd = Rd, host = MyHost} = State) ->
    rd_resource_db:add_resources(Rd, [Resource]),
    broadcast_to_remotes(MyHost, [Resource], send_resources),
    {noreply, State};

%% @doc Add resources that were specified in startup.
handle_cast({startup, Resources},
                #state{rd = Rd, ss_queue = SSQueue, host = Host} = State) ->
    rd_resource_db:add_resources(Rd, Resources),
    UpEvent = handyterm:term_to_string(#hostevent{host = Host, event = up}),

    %% Wait before broadcasting our resources
    TimeToWait = handyconfig:get_env_default(resource_discovery, rd_host_wait, ?DEFAULT_WAIT),
    gen_stomp:send(SSQueue, UpEvent, [], TimeToWait),
    {noreply, State, TimeToWait};

handle_cast({update_resources, Resources}, #state{rd = Rd, host = MyHost} = State) ->
    rd_resource_db:update_resources(Rd, Resources),
    broadcast_to_remotes(MyHost, Resources, update_resources),
    {noreply, State};

handle_cast({delete_resources, Resources}, #state{rd = Rd, host = MyHost} = State) ->
    rd_resource_db:delete_resources(Rd, Resources),
    broadcast_to_remotes(MyHost, Resources, delete_resources),
    {noreply, State};

%% TimeToWait Stop the server.
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{rd = Rd, host = MyHost} = State) ->
    Resources = rd_resource_db:all(Rd),
    broadcast_to_remotes(MyHost, Resources, send_resources),
    {noreply, State};

%% @doc On timeout go out and query for the resources.
handle_info(_Message, State) ->
    %% Error log bad message
    {noreply, State}.

terminate(_Reason, #state{ss_queue = SSQueue, host = Host}) ->
    % Remove mapping for process and tell everyone we are going down!
    DownEvent = handyterm:term_to_string(#hostevent{host = Host, event = down}),
    gen_stomp:send(SSQueue, DownEvent, []),
    rd_store:delete_by_pid(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

broadcast_to_remotes(_MyHost, [], _Method) ->
    ok;
broadcast_to_remotes(MyHost, Resources, Method) ->
    lists:foreach(
        fun({Host, _Pid}) ->
            case Host =:= MyHost of
                true -> ok;
                false ->
                    spawn(rd_host_request, Method, [Host, MyHost, Resources])
            end
        end, rd_store:all()).