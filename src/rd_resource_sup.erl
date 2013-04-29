%%% ===================================================================
%%% @doc Supervisor for rd_resource_server.
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


