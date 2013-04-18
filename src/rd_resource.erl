-module(rd_resource).

-include("resource.hrl").

% API
-export([new/0, insert/2, delete_by_name/2, delete_by_type/2,
            delete_all/1, find_by_name/2, find_by_type/2, all/1]).

new() ->
    ets:new(resources, [private, bag, {keypos, #resource.name}]).

insert(Descriptor, Resource) ->
	true = ets:insert(Descriptor, Resource).

delete_by_name(Descriptor, Name) ->
    ets:match_delete(Descriptor, #resource{name = Name, _ = '_'}).

delete_by_type(Descriptor, Type) ->
    ets:match_delete(Descriptor, #resource{type = Type, _ = '_'}).

delete_all(Descriptor) ->
    ets:delete_all_objects(Descriptor).

find_by_name(Descriptor, Name) ->
    ets:match_object(Descriptor, #resource{name = Name, _ = '_'}).

find_by_type(Descriptor, Type) ->
    ets:match_object(Descriptor, #resource{type = Type, _ = '_'}).

all(Descriptor) ->
    ets:match_object(Descriptor, #resource{_ = '_'}).
