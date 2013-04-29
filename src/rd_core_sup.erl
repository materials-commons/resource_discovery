-module(rd_core_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, supervisor, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc starts the supervisor.
start_link(Arguments) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Arguments).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
init(Arguments) ->
    PingPongPort = get_value(Arguments, ping_pong_port),
    PingHeartBeat = get_value(Arguments, ping_heartbeat),
    StompHost= get_value(Arguments, stomp_host),
    HostIpAddress = get_value(Arguments, hostip),
    StompPort = get_value(Arguments, stomp_port),
    StompUser = get_value(Arguments, stomp_user),
    StompPassword = get_value(Arguments, stomp_password),
    RdLease = get_value(Arguments, rd_lease),
    LSockPong = get_value(Arguments, lsock_pong),
    LSockRH = get_value(Arguments, lsock_rh),

    Supervisors = [
        ?CHILD(rd_resource_sup, [StompHost, StompPort, StompUser,
                                    StompPassword, RdLease]),
        ?CHILD(rd_host_sup, [StompHost, StompPort, StompUser,
                                StompPassword, HostIpAddress, []]),
        ?CHILD(rd_monitor_sup, [PingHeartBeat, PingPongPort, StompHost,
                                StompPort, StompUser, StompPassword]),
        ?CHILD(rd_pong_sup, [LSockPong]),
        ?CHILD(rd_host_request_sup, [LSockRH])
    ],
    {ok, { {one_for_one, 5, 10}, Supervisors } }.

get_value(List, Key) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.
