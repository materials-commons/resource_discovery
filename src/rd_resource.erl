-module(rd_resource).

-include("resource.hrl").

% API
-export([find_by/3, new/0, insert/1, delete_by/3]).

new() ->
	resource_by_host = ets:new(resource_by_host, [private, named_table, bag, {keypos, #resource.host}]),
	resource_by_type = ets:new(resource_by_type, [private, named_table, bag, {keypos, #resource.type}]).

insert(Resource) ->
	true = ets:insert(resource_by_host, Resource),
	true = ets:insert(resource_by_type, Resource).

delete_by(host, Host, Resource) ->
	ok;
delete_by(type, Type, Resource) ->
	ok.

find_by(_, _, []) ->
	{error, not_found};
find_by(host, Host, Resources) ->
	{ok, lists:filter(
		fun (#resource{host = ResourceHost}) ->
			string:equal(ResourceHost, Host)
		end, Resources)};
find_by(type, Type, Resources) ->
	{ok, lists:filter(
			fun (#resource{type = ResourceType}) ->
				string:equal(ResourceType, Type)
			end, Resources)};
find_by(_, _, _) ->
	{error, badarg}.