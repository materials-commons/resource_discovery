
-module(rd_resource_sup).

-behaviour(supervisor).

%% API
-export([start_link/5, start_child/2, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, temporary, brutal_kill, Type, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StompHost, StompPort, StompUser, StompPassword, RdLease) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
                [StompHost, StompPort, StompUser, StompPassword, RdLease]).

start_child(Host, Resources) ->
    supervisor:start_child(?SERVER, [Host, Resources]).

start_child(Host) ->
    start_child(Host, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([StompHost, StompPort, StompUser, StompPassword, RdLease]) ->
    RdServer = ?CHILD(rd_resource_server, worker,
            [StompHost, StompPort, StompUser, StompPassword, RdLease]),
    {ok, { {simple_one_for_one, 0, 1}, [RdServer]} }.


