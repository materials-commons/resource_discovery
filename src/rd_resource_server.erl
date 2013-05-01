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
-behaviour(gen_server).

-include("resource.hrl").

%% API
-export([start_link/2, start_link/3, start/1, start/2, fetch/1,
            update/1, add/2, stop/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
            code_change/3]).

%% spawn export
-export([get_resources/1]).

-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)). % 1 Day by DEFAULT_LEASE_TIME

-type seconds() :: integer().

-record(state,
    {
        rd :: rd_resource_db:descriptor(), % descriptor to resource_db
        host :: string(), % Host we are monitoring.
        lease_time :: seconds(), % How long before refresh
        start_time :: seconds() % Used to determine timeout
    }).

%% ===================================================================
%% API
%% ===================================================================

%% @doc start the server
start_link(LeaseTime, Host) ->
    start_link(LeaseTime, Host, []).

%% @doc starts the server
start_link(LeaseTime, Host, Resources) ->
    gen_server:start_link(?MODULE, [LeaseTime, Host, Resources], []).

%% @doc starts server by asking supervisor to start us.
-spec start(string()) -> {ok, pid()}.
start(Host) ->
    start(Host, []).

%% @doc starts server by asking supervisor to start us.
-spec start(string(), [resource()] | []) -> {ok, pid()}.
start(Host, Resources) ->
    rd_resource_sup:start_child(Host, Resources).

%% @doc Return known resources
-spec fetch(pid()) -> [resource()] | [].
fetch(Pid) ->
    gen_server:call(Pid, fetch).

%% @doc stop server.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

%% @doc Force server to resync its resource view.
-spec update(pid()) -> ok.
update(Pid) ->
    gen_server:cast(Pid, update).

%% @doc Add resources to server
-spec add(pid(), [resource()]) -> ok.
add(Pid, Resources) ->
    gen_server:cast(Pid, {add, Resources}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc No resources specified, need to query host for them.
init([LeaseTime, Host, Resources]) ->
    process_flag(trap_exit, true),

    Now = lease:seconds_now(),
    Rd = rd_resource_db:new(),

    %% Do initialization outside of init.
    gen_server:cast(self(), {startup, Resources}),

    State = #state{lease_time = LeaseTime, start_time = Now,
                    rd = Rd, host = Host},
    TimeLeft = lease:time_left(Now, LeaseTime),
    {ok, State, TimeLeft}.


%% @doc Retrieve all resources
handle_call(fetch, _From,
        #state{lease_time = LeaseTime, start_time = StartTime, rd = Rd} = State) ->
    Resources = rd_resource_db:all(Rd),
    TimeLeft = lease:time_left(StartTime, LeaseTime),
    {reply, {ok, Resources}, State, TimeLeft}.

%% @doc Update view of resources for this host.
handle_cast(update,
        #state{lease_time = LeaseTime, start_time = StartTime,
                host = Host} = State) ->
    start_resource_request(Host),
    TimeLeft = lease:time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

%% @doc Add resources for this host.
handle_cast({add, Resources},
        #state{lease_time = LeaseTime, start_time = StartTime,
                rd = Rd} = State) ->
    add_resources(Rd, Resources),
    TimeLeft = lease:time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};

%% @doc Complete startup when no resources were given by asking for
%% for the resources from the server.
handle_cast({startup, []}, #state{start_time = StartTime, host = Host,
                                    lease_time = LeaseTime} = State) ->
    start_resource_request(Host),
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

handle_info({'EXIT', _Pid, _Reason}, _State) ->
    ok;

%% @doc On timeout go out and query for the resources.
handle_info(timeout, #state{lease_time = LeaseTime, rd = Rd,
                            host = Host} = State) ->
    % On timeout we go and query for the resources.
    Now = lease:seconds_now(),
    NewTimeout = lease:time_left(Now, LeaseTime),
    rd_resource_db:delete_all(Rd),
    start_resource_request(Host),
    {noreply, State#state{start_time = Now}, NewTimeout}.

%% @private
%%
%% Upon termination we need to clean up the Pid/Host mapping
%% for our server.
terminate(_Reason, _State) ->
    % Remove mapping for process
    rd_store:delete_by_pid(self()),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

%% Add a list of resources to our database of resources
add_resources(_Rd, []) ->
    ok;
add_resources(Rd, [R|T]) ->
    rd_resource_db:insert(Rd, R),
    add_resources(Rd, T).

%% Spawn resource retrieval method so we don't block.
start_resource_request(Host) ->
    spawn(?MODULE, get_resources, [Host]).

%% Spawned method to retrieve resources.
get_resources(Host) ->
    Resources = rd_host_request:request_resources(Host),
    resource_discovery:insert(Host, Resources).





