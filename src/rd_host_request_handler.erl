%%% ===================================================================
%%% @doc Server to handle TCP based requests for resources to the hosts
%%%      resources.
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

-module(rd_host_request_handler).
-behaviour(gen_server).

%% API
-export([start_link/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, {
        lsock :: port() % Socket we are listening on.
    }).

%% ===================================================================
%% API
%% ===================================================================

%% @doc Starts server
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
%% Handles commands sent over the socket.
handle_info({tcp, Socket, RawData}, State) ->
    RawDataBin = list_to_binary(RawData),
    Request = binary_to_term(RawDataBin),
    handle_request(Socket, Request),
    {stop, normal, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
%% Wait for a connection, start new child and then go on to
%% handle requests.
handle_info(timeout, #state{lsock = LSock} = State)->
    {ok, _Sock} = gen_tcp:accept(LSock),
    rd_host_request_sup:start_child(),
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

%% Send back all the resources for our local host.
handle_request(Socket, resources) ->
    {ok, Resources} = rd_host_server:fetch(),
    gen_tcp:send(Socket, term_to_binary(Resources));

%% Receive resources from host
handle_request(_Socket, {myresources, Host, Resources}) ->
    resource_discovery:insert(Host, Resources);

%% Delete resources we are tracking
handle_request(_Socket, {delete_resources, Host, Resources}) ->
    resource_discovery:delete_resources_for(Host, Resources);

%% Update resources we are tracking
handle_request(_Socket, {update_resources, Host, Resources}) ->
    resource_discovery:update_resources_for(Host, Resources);

%% Unknown command handler.
handle_request(_Socket, Request) ->
    io:format("rd_host_request_handler - Unknown Request: ~p~n", [Request]),
    ok.