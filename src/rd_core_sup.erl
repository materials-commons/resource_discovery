%%% ===================================================================
%%% @doc The core supervisor for the application. Starts all other
%%%      supervisors.
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
                        StompPort, StompUser, StompPassword, HostIpAddress]),
        ?CHILD(rd_pong_sup, [LSockPong]),
        ?CHILD(rd_host_request_sup, [LSockRH])
    ],
    {ok, { {one_for_one, 5, 10}, Supervisors } }.

get_value(List, Key) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.
