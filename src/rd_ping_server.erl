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

-record(state, {heartbeat, start_time, port}).

%% ===================================================================
%% API
%% ===================================================================

start_link(Port) ->
    start_link(?DEFAULT_HEARTBEAT, Port).

start_link(Heartbeat, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Heartbeat, Port], []).

ping(Host) ->
    gen_server:cast(?SERVER, {ping, Host}).

ping_all() ->
    gen_server:cast(?SERVER, ping_all).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Heartbeat, Port]) ->
    %% Trap exits and handle ourselves.
    process_flag(trap_exit, true),
    Now = lease:seconds_now(),
    TimeLeft = lease:time_left(Now, Heartbeat),
    State = #state{heartbeat = Heartbeat, start_time = Now, port = Port},
    {ok, State, TimeLeft}.

handle_call(_Msg, _From, #state{heartbeat = Heartbeat,
                                start_time = StartTime} = State) ->
    % Error log unknown message
    TimeLeft = lease:time_left(StartTime, Heartbeat),
    {reply, ok, State, TimeLeft}.

handle_cast({ping, Host}, #state{heartbeat = Heartbeat, port = Port,
                                    start_time = StartTime} = State) ->
    case rd_store:lookup(Host) of
        {ok, Pid} -> start_pinger(Host, Pid, Port);
        {error, not_found} -> ok
    end,
    TimeLeft = lease:time_left(StartTime, Heartbeat),
    {noreply, State, TimeLeft};
handle_cast(ping_all, #state{heartbeat = Heartbeat, port = Port,
                                start_time = StartTime} = State) ->
    start_pingers(Port),
    TimeLeft = lease:time_left(StartTime, Heartbeat),
    {noreply, State, TimeLeft};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(timeout, #state{heartbeat = Heartbeat, port = Port} = State)->
    start_pingers(Port),
    Now = lease:seconds_now(),
    NewTimeout = lease:time_left(Now, Heartbeat),
    {noreply, State#state{start_time = Now}, NewTimeout}.

 terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

start_pingers(Port) ->
    lists:foreach(
            fun(Entry) ->
                start_pinger(Entry, Port)
            end, rd_store:all()).

start_pinger({Host, Pid}, Port) ->
    start_pinger(Host, Pid, Port).

start_pinger(Host, Pid, Port) ->
    spawn(?MODULE, ping_host, [Host, Pid, Port]).

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




