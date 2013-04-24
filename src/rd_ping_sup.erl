-module(rd_ping_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, brutal_kill, worker, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc starts the supervisor.
start_link(PingHeartBeat, PingPongPort) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PingHeartBeat, PingPongPort]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
init([PingHeartBeat, PingPongPort]) ->
    RdPingServer = ?CHILD(rd_ping_server, [PingHeartBeat, PingPongPort]),
    {ok, { {one_for_one, 5, 10}, [RdPingServer] } }.
