-module(rd_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/7]).

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
                StompUser, StompPassword, ThisHost) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
        [PingHeartBeat, PingPongPort, StompHost, StompPort,
            StompUser, StompPassword, ThisHost]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
init([PingHeartBeat, PingPongPort, StompHost, StompPort,
        StompUser, StompPassword, ThisHost]) ->
    RdPingServer = ?CHILD(rd_ping_server, [PingHeartBeat, PingPongPort]),
    RdNewHostMonitor = ?CHILD(rd_new_host_monitor, [StompHost, StompPort,
                                StompUser, StompPassword, ThisHost]),
    {ok, { {one_for_one, 5, 10}, [RdPingServer, RdNewHostMonitor] } }.
