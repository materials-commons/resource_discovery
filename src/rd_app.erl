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
    ConfigEntries = get_app_config_entries(),
    {ping_pong_port, Port} = lists:keyfind(ping_pong_port, 1, ConfigEntries),
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    ExternalAddress = get_host_external_address(),
    NewConfigEntries = ConfigEntries ++ [{lsock, LSock}, {hostip, ExternalAddress}],
    Value = rd_core_sup:start_link(NewConfigEntries),
    io:format("rd_core_sup returned: ~p~n", [Value]),
    Value.

stop(_State) ->
    ok.

%% ===================================================================
%% Local
%% ===================================================================
get_app_config_entries() ->
    Entries = [
        stomp_host,
        stomp_port,
        stomp_user,
        stomp_password,
        rd_lease,
        ping_pong_port,
        ping_heartbeat
    ],
    lists:map(fun entry_2_kv/1, Entries).

entry_2_kv(What) ->
    {ok, Value} = rd_get_env(What),
    {What, Value}.

rd_get_env(What) ->
    application:get_env(resource_discovery, What).

get_host_external_address() ->
    {ok, Address} = handynet:get_address(handynet:get_external_addrs(), 1),
    Address.
