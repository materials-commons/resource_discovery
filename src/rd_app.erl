-module(rd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(handyman),
    application:start(ossp_uuid),
    application:start(stomp_client),
    application:start(resource_discovery).

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

stop(_State) ->
    ok.

%% ===================================================================
%% Local
%% ===================================================================

construct_supervisor_arguments_list() ->
    ConfigEntries = get_app_config_entries(),
    ListenSocks = get_listen_socket_entries(ConfigEntries),
    HostExternalAddressEntry = get_host_external_address(),
    ConfigEntries ++ ListenSocks ++ [HostExternalAddressEntry].

get_app_config_entries() ->
    AllEntries = application:get_all_env(resource_discovery),
    lists:filter(fun({Key, _}) -> Key =/= included_applications end, AllEntries).

get_listen_socket_entries(ConfigEntries) ->
    {ping_pong_port, PingPongPort} = lists:keyfind(ping_pong_port, 1, ConfigEntries),
    {ok, LPongSock} = sock_listen(PingPongPort),

    {rd_request_handler_port, ReqHandlerPort} =
            lists:keyfind(rd_request_handler_port, 1, ConfigEntries),
    {ok, LReqHandlerSock} = sock_listen(ReqHandlerPort),

    [{lsock_pong, LPongSock}, {lsock_rh, LReqHandlerSock}].

get_host_external_address() ->
    {ok, Address} = handynet:get_address(handynet:get_external_addrs(), 1),
    {hostip, Address}.

start_children() ->
    Children = [rd_pong_sup, rd_host_request_sup],
    lists:foreach(
        fun (Module) ->
            {ok, _Pid} = Module:start_child()
        end, Children).

sock_listen(Port) ->
    gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}]).

