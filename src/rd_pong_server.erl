%%% ===================================================================
%%% @doc Respond to resource ping requests (heartbeat) for checking if
%%%      host is still alive.
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

-module(rd_pong_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {lsock}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc start server.
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @private
init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.


%% @private
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @private
%%
%% Handle request from socket.
handle_info({tcp, Socket, RawData}, State) ->
	case handle_request(Socket, RawData) of
		ok -> {stop, normal, State};
		{error, Reason} -> {stop, {error, Reason}, State}
	end;
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
%% Handle expensive startup in timeout (socket accept and re-spawn)
handle_info(timeout, #state{lsock = LSock} = State)->
    {ok, _Sock} = gen_tcp:accept(LSock),
    rd_pong_sup:start_child(),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

%% Received a PING send PONG response.
handle_request(Socket, "PING") ->
    gen_tcp:send(Socket, "PONG");
%% Unknown command on socket.
handle_request(_Socket, _Request) ->
    %% Error log an unknown command.
    {error, unknown_command}.



