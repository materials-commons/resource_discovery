%%% ===================================================================
%%% @doc Client API for resource discovery. Services local to a node
%%%      can use this API to discover and add resources for different hosts.
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

-module(resource_discovery).
-export([insert/2, lookup/1, delete_resources_for/2,
            update_resources_for/2, delete_host/1, create/1, all/0]).

-include("resource.hrl").

%% @doc Add resources for host.
%%
%% If the host doesn't exist then create the host otherwise add
%% the resources.
-spec insert(string(), resource() | [resource()]) -> ok.
insert(Host, Resources) when is_list(Resources) ->
    case rd_store:lookup(Host) of
        {ok, Pid} ->
            rd_resource_server:add_resources(Pid, Resources);
        {error, _} ->
            {ok, Pid} = rd_resource_server:start(Host, Resources),
            rd_store:insert(Host, Pid)
    end;
insert(Host, #resource{} = Resource) ->
    insert(Host, [Resource]).

%% @doc Lookup the resources for a host.
-spec lookup(string()) -> {ok, [resource()]} | {error, not_found}.
lookup(Host) ->
    try
        {ok, Pid} = rd_store:lookup(Host),
        {ok, Resources} = rd_resource_server:fetch(Pid),
        {ok, Resources}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

%% @doc Creates a new host for holding resources if it doesn't already exist.
-spec create(string()) -> ok | {error, exists}.
create(Host) ->
    case rd_store:lookup(Host) of
        {error, not_found} ->
            {ok, Pid} = rd_resource_server:start(Host, []),
            rd_store:insert(Host, Pid),
            ok;
        {ok, _Pid} ->
            {error, exists}
    end.

%% @doc deletes a host and all resources. If host doesn't exist just ignores it.
-spec delete_host(string()) -> ok.
delete_host(Host) ->
    case rd_store:lookup(Host) of
        {ok, Pid} -> rd_resource_server:stop(Pid);
        {error, _Reason} -> ok
    end.

%% @doc Get a list of host resources in the form [{Host, [resources]}].
-spec all() -> [{string(), [resource()] | []}] | [].
all() ->
    lists:map(
        fun({Host, Pid}) ->
            {ok, HostResources} = rd_resource_server:fetch(Pid),
            {Host, HostResources}
        end, rd_store:all()).

%% @doc Delete a list of resources from the specified host.
-spec delete_resources_for(string(), [resource()] | []) -> ok.
delete_resources_for(_Host, []) ->
    ok;
delete_resources_for(Host, Resources) ->
    case rd_store:lookup(Host) of
        {ok, Pid} -> rd_resource_server:delete(Pid, Resources);
        {error, _Reason} -> ok
    end.

%% @doc Update a list of resources from the specified host.
-spec update_resources_for(string(), [resource()] | []) -> ok.
update_resources_for(_Host, []) ->
    ok;
update_resources_for(Host, Resources) ->
    case rd_store:lookup(Host) of
        {ok, Pid} -> rd_resource_server:update(Pid, Resources);
        {error, _Reason} -> ok
    end.
