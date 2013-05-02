%%% ===================================================================
%%% @doc Supervisor for rd_host_server.
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

-module(rd_host_sup).

-behaviour(supervisor).

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 2000, worker, [I]}).

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
    RdHostServer = ?CHILD(rd_host_server,
            [StompHost, StompPort, StompUser, StompPassword, Host, Resources]),
    {ok, { {one_for_one, 5, 10}, [RdHostServer] } }.
