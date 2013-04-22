
-module(rd_resource_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, brutal_kill, Type, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Host, Resources, LeaseTime) ->
    {StompHost, StompPort, StompUser, StompPassword} = get_stomp_settings(),
    supervisor:start_child(?SERVER,
        [StompHost, StompPort, StompUser, StompPassword, Resources, Host, LeaseTime]).

start_child(Host, LeaseTime) ->
    start_child(Host, [], LeaseTime).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RdServer = ?CHILD(rd_resource_server, worker),
    {ok, { {simple_one_for_one, 0, 1}, [RdServer]} }.

%% ===================================================================
%% Private
%% ===================================================================
get_stomp_settings() ->
    % For now while we test.
    {"localhost", 61613, "guest", "guest"}.


