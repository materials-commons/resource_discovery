%%% ===================================================================
%%% @doc Ping known servers to see if they are still alive. If no
%%%      response then shutdown are local view of that host.
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

-module(rd_ping_server).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, ping/1, ping_all/0]).

%% Exported for spawn
-export([ping_host/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_HEARTBEAT, (60 * 60)). % Check every hour

-record(state, {
        heartbeat :: integer(), %% How often do we ping known hosts
        start_time :: integer(), %% Used in timeout determination (heartbeat)
        port :: port() %% Socket port to connect on.
    }).

%% ===================================================================
%% API
%% ===================================================================

%% @doc start server - uses default heartbeat time
start_link(Port) ->
    start_link(?DEFAULT_HEARTBEAT, Port).

%% @doc start server
start_link(Heartbeat, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Heartbeat, Port], []).

%% @doc Cause server to ping specified host outside of heartbeat
-spec ping(string()) -> ok.
ping(Host) ->
    gen_server:cast(?SERVER, {ping, Host}).

%% @doc Cause server to ping all known hosts outside of heartbeat
-spec ping_all() -> ok.
ping_all() ->
    gen_server:cast(?SERVER, ping_all).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @private
init([Heartbeat, Port]) ->
    %% Trap exits and handle ourselves.
    process_flag(trap_exit, true),
    Now = lease:seconds_now(),
    TimeLeft = lease:time_left(Now, Heartbeat),
    State = #state{heartbeat = Heartbeat, start_time = Now, port = Port},
    {ok, State, TimeLeft}.

%% @private
%%
%% Handle unknown messages
handle_call(_Msg, _From, #state{heartbeat = Heartbeat,
                                start_time = StartTime} = State) ->
    % Error log unknown message
    TimeLeft = lease:time_left(StartTime, Heartbeat),
    {reply, ok, State, TimeLeft}.

%% @private
%%
%% Implements the ping(Host) API call.
handle_cast({ping, Host}, #state{heartbeat = Heartbeat, port = Port,
                                    start_time = StartTime} = State) ->
    case rd_store:lookup(Host) of
        {ok, Pid} -> start_pinger(Host, Pid, Port);
        {error, not_found} -> ok
    end,
    TimeLeft = lease:time_left(StartTime, Heartbeat),
    {noreply, State, TimeLeft};
%% @private
%%
%% Implements the ping_all() API all.
handle_cast(ping_all, #state{heartbeat = Heartbeat, port = Port,
                                start_time = StartTime} = State) ->
    start_pingers(Port),
    TimeLeft = lease:time_left(StartTime, Heartbeat),
    {noreply, State, TimeLeft};
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @private
%%
%% Handle process exit trap. Ignore it, we don't really care if the
%% process worked (Come back and reconsider this statement)
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

%% @private
%%
%% On timeout we ping all known hosts, then restart heartbeat count down.
handle_info(timeout, #state{heartbeat = Heartbeat, port = Port} = State)->
    start_pingers(Port),
    Now = lease:seconds_now(),
    NewTimeout = lease:time_left(Now, Heartbeat),
    {noreply, State#state{start_time = Now}, NewTimeout}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

%% Starts a pinger for each known host.
start_pingers(Port) ->
    lists:foreach(
            fun(Entry) ->
                start_pinger(Entry, Port)
            end, rd_store:all()).

%% Starts a pinger for the specified host on port. Pid is local process
%% holding resources for the host.
start_pinger({Host, Pid}, Port) ->
    start_pinger(Host, Pid, Port).

%% Starts a pinger for the specified host on port. Pid is local process
%% holding resources for the host.start_pinger(Host, Pid, Port) ->
    spawn(?MODULE, ping_host, [Host, Pid, Port]).

%% Function we spawn. Tries to ping host. On anything but success we
%% shutdown server mapping this.
ping_host(Host, Pid, Port) ->
    try
        {ok, Sock} = gen_tcp:connect(Host, Port, [{active, false}]),
        ok = gen_tcp:send(Sock, "PING"),
        % wait 10 seconds for a response
        {ok, _Response} = gen_tcp:recv(Sock, 0, 10 * 1000)
    catch
        _What:_Reason ->
            %% Socket communication error. Host must be down.
            %% Remove host by having its resource tracker exit.
            %io:format("Ping failed ~p:~p, calling stop on ~p~n", [What,Reason,Pid]),
            rd_resource_server:stop(Pid)
    end.




