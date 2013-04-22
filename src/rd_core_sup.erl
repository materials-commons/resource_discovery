-module(rd_core_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, [Args]}, permanent, 5000, supervisor, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc starts the supervisor.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private
init(Arguments) ->
    Supervisors = [
        ?CHILD(rd_sup, Arguments),
        ?CHILD(rd_host_sup, Arguments),
        ?CHILD(rd_ping_sup, Arguments),
        ?CHILD(rd_pong_sup, Arguments)
    ],
    {ok, { {one_for_one, 5, 10}, Supervisors } }.
