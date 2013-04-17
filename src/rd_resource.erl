-module(rd_resource).

-include("resource.hrl").

% API
-export([new/0, insert/1, delete_by_name/1, delete_by_type/1, 
            find_by_name/1, find_by_type/1, all/0]).

new() ->
    resources = ets:new(resources, [private, named_table, bag, {keypos, #resource.name}]).

insert(Resource) ->
	true = ets:insert(resources, Resource).

delete_by_name(Name) ->
    ets:match_delete(resources, #resource{name = Name, _ = '_'}).

delete_by_type(Type) ->
    ets:match_delete(resources, #resource{type = Type, _ = '_'}).

find_by_name(Name) ->
    ets:match_object(resources, #resource{name = Name, _ = '_'}).

find_by_type(Type) ->
    ets:match_object(resources, #resource{type = Type, _ = '_'}).

all() ->
    ets:match_object(resources, #resource{_ = '_'}).
