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

%% API
-export([start_link/5, start_link/6, start/1, start/2, start/3, fetch/1,
            update/1, delete_resource/2, add_resource/2]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
            code_change/3]).

-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)). % 1 Day by DEFAULT_LEASE_TIME

-type seconds() :: integer().

-record(state,
    {
        host :: string(),
        lease_time :: seconds(),
        start_time :: seconds()
    }).

%% ===================================================================
%% API
%% ===================================================================

%% @doc start the server
start_link(Host, Port, Username, Password, LeaseTime) ->
    HostQueue = string:concat("/topic/rd_", Host),
    gen_stomp:start_link(?MODULE, Host, Port, Username, Password, [{HostQueue, []}],
        [[], LeaseTime]).

start_link(Host, Port, Username, Password, Resources, LeaseTime) ->
    HostQueue = string:concat("/topic/rd_", Host),
    gen_stomp:start_link(?MODULE, Host, Port, Username, Password, [{HostQueue, []}],
        [Resources, LeaseTime]).

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
init([Resources, LeaseTime]) ->
    Now = seconds_now(),
    %% Do initialization outside of init.
    gen_server:cast(self(), {startup, Resources}),
    State = #state{ lease_time = LeaseTime, start_time = Now},
    {ok, State, time_left(Now, LeaseTime)}.


%% @doc Retrieve all resources
handle_call(fetch, _From,
        #state{lease_time = LeaseTime, start_time = StartTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    {reply, {ok, []}, State, TimeLeft}.

%% @doc
handle_cast(update,
        #state{lease_time = LeaseTime, start_time = StartTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};
handle_cast([{message, _Message}, {queue, _Queue}],
        #state{start_time = StartTime, lease_time = LeaseTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};
handle_cast({add_resource, Resource},
        #state{start_time = StartTime, lease_time = LeaseTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State, TimeLeft};
%% @doc complete startup when no resources were given
handle_cast({startup, []}, State) ->
    {noreply, State, 0};
handle_cast({startup, Resources},
        #state{start_time = StartTime, lease_time = LeaseTime} = State) ->
    %lists:foreach()
    {noreply, State, 0};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, #state{lease_time = LeaseTime} = State) ->
    % On timeout we go and query for the resources.
    Now = seconds_now(),
    {noreply, State, time_left(Now, LeaseTime)}.

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
