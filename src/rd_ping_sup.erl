-module(rd_ping_sup).

-behaviour(supervisor).

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, brutal_kill, worker, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc starts the supervisor.
start_link(PingHeartBeat, PingPongPort, StompHost, StompPort,
                StompUser, StompPassword) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
        [PingHeartBeat, PingPongPort, StompHost, StompPort,
            StompUser, StompPassword]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
init([PingHeartBeat, PingPongPort, StompHost, StompPort,
        StompUser, StompPassword]) ->
    RdPingServer = ?CHILD(rd_ping_server, [PingHeartBeat, PingPongPort]),
    RdNewHostMonitor = ?CHILD(rd_new_host_monitor, [StompHost, StompPort,
                                StompUser, StompPassword]),
    {ok, { {one_for_one, 5, 10}, [RdPingServer, RdNewHostMonitor] } }.
