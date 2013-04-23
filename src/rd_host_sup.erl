-module(rd_host_sup).

-behaviour(supervisor).

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_host_link, []}, permanent, brutal_kill, worker, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc starts the supervisor.
start_link(StompHost, StompPort, StompUser, StompPassword, Host, Resources) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
                [StompHost, StompPort, StompUser, StompPassword, Host, Resources]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
init([StompHost, StompPort, StompUser, StompPassword, Host, Resources]) ->
    RdHostServer = ?CHILD(rd_resource_server,
            [StompHost, StompPort, StompUser, StompPassword, Host, Resources]),
    {ok, { {one_for_one, 5, 10}, [RdHostServer] } }.
