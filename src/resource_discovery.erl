%%% ===================================================================
%%% @doc Client API for resource discovery. Services local to a node
%%% can use this API to discover and add resources for different hosts.
%%% ===================================================================
-module(resource_discovery).
-export([insert/2, lookup/1, delete/1, create/1]).

-include("resource.hrl").

insert(Host, #resource{} = Resource) ->
    case rd_store:lookup(Host) of
        {ok, Pid} ->
            rd_resource_server:add_resource(Pid, Resource);
        {error, _} ->
            {ok, Pid} = rd_resource_server:start(Host, [Resource]),
            rd_store:insert(Host, Pid)
    end.

lookup(Host) ->
    try
        {ok, Pid} = rd_store:lookup(Host),
        {ok, _Resources} = rd_resource_server:fetch(Pid)
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

create(Host) ->
    case rd_store:lookup(Host) of
        {error, not_found} ->
            {ok, Pid} = rd_resource_server:start(Host, []),
            rd_store:insert(Host, Pid),
            ok;
        {ok, _Pid} ->
            {error, exists}
    end.

delete(Host) ->
    case rd_store:lookup(Host) of
        {ok, Pid} -> rd_resource_server:stop(Pid);
        {error, _Reason} -> ok
    end.
