%%% ===================================================================
%%% @doc A resource discovery system with timeouts.
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

-module(rd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%% API
-export([start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc Short cut to start application and all dependencies explicitly.
start() ->
    application:start(handyman),
    application:start(ossp_uuid),
    application:start(stomp_client),
    application:start(resource_discovery).

%% @doc Application callback for starting app.
start(_StartType, _StartArgs) ->
    rd_store:init(),
    SupervisorArguments = construct_supervisor_arguments_list(),
    case rd_core_sup:start_link(SupervisorArguments) of
        {ok, Pid} ->
            start_children(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

%% @doc Prepare to stop - Broadcast we are going down!
prep_stop(_State) ->
    try
        io:format("rd_app:prep_stop called~n"),
        rd_host_server:send_down()
    catch
        Type:Reason ->
            io:format("Stopping application resource_discovery: ~p~p.~n", [Type, Reason])
    end,
    stopping.

%% @doc Application callback called when stopping application.
stop(_State) ->
    io:format("rd_app:stop called~n"),
    ok.

%% ===================================================================
%% Local
%% ===================================================================

%% Creates a list of key value pairs that are the configuration
%% entries that different supervisors need.
construct_supervisor_arguments_list() ->
    ConfigEntries = get_app_config_entries(),
    ListenSocks = get_listen_socket_entries(ConfigEntries),
    HostExternalAddressEntry = get_host_external_address(),
    ConfigEntries ++ ListenSocks ++ [HostExternalAddressEntry].

%% Get all the configuration entries for the resource_discovery application,
%% filtering out included_applications (not used by us).
get_app_config_entries() ->
    AllEntries = application:get_all_env(resource_discovery),
    lists:filter(fun({Key, _}) -> Key =/= included_applications end, AllEntries).

%% Get sockets we are listening on and create key/value pairs for these socks.
%% These are used by other servers in the application.
get_listen_socket_entries(ConfigEntries) ->
    PongSockKV = socket_kv_for_port_entry(ping_pong_port, lsock_pong,
                                ConfigEntries),

    ReqHandlerSockKV = socket_kv_for_port_entry(rd_request_handler_port,
                                lsock_rh, ConfigEntries),
    [PongSockKV, ReqHandlerSockKV].

%% Find port for key, open socket on port, return key-value for socket
socket_kv_for_port_entry(PortEntryKey, SocketKeyToUse, ConfigEntries) ->
    {PortEntryKey, Port} = lists:keyfind(PortEntryKey, 1, ConfigEntries),
    {ok, Socket} = socket_listen(Port),
    {SocketKeyToUse, Socket}.

%% Get the first external ip address for this host. Turn into key/value config
%% entry for use later on.
get_host_external_address() ->
    {ok, Address} = handynet:get_address(handynet:get_external_addrs(), 1),
    {hostip, Address}.

%% Start child processes after their respective supervisors have started.
%% These children are simple_one_for_one servers.
start_children() ->
    Children = [rd_pong_sup, rd_host_request_sup],
    lists:foreach(
        fun (Module) ->
            {ok, _Pid} = Module:start_child()
        end, Children).

%% Consolidate how we call gen_tcp into a single place to make modifying easier.
socket_listen(Port) ->
    gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]).

