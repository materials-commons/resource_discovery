%%% ===================================================================
%%% @doc API for storing/managing host resources. Used by the resource
%%%      servers.
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

-module(rd_resource_db).

-include("resource.hrl").

-type descriptor() :: ets:tid().

% API
-export([new/0, insert/2, delete_by_name/2, delete_by_type/2,
            delete_all/1, find_by_name/2, find_by_type/2, all/1]).

%% @doc Initializes database
-spec new() -> descriptor().
 new() ->
    ets:new(resources, [private, bag, {keypos, #resource.name}]).

%% @doc Inserts a new resource into tabl
-spec insert(descriptor(), resource()) -> true.
insert(Descriptor, Resource) ->
	true = ets:insert(Descriptor, Resource).

%% @doc Delete entry by resource name.
-spec delete_by_name(descriptor(), any()) -> true.
delete_by_name(Descriptor, Name) ->
    ets:match_delete(Descriptor, #resource{name = Name, _ = '_'}).

%% @doc Delete entry by resource type.
-spec delete_by_type(descriptor(), any()) -> true.
delete_by_type(Descriptor, Type) ->
    ets:match_delete(Descriptor, #resource{type = Type, _ = '_'}).

%% @doc Delete all entries.
-spec delete_all(descriptor()) -> true.
delete_all(Descriptor) ->
    ets:delete_all_objects(Descriptor).

%% @doc Finds resources matching resource name
-spec find_by_name(descriptor(), any()) -> [resource()] | [].
find_by_name(Descriptor, Name) ->
    ets:match_object(Descriptor, #resource{name = Name, _ = '_'}).

%% @doc Finds resources matching resource type
-spec find_by_type(descriptor(), any()) -> [resource()] | [].
find_by_type(Descriptor, Type) ->
    ets:match_object(Descriptor, #resource{type = Type, _ = '_'}).

%% @doc Returns all known resource entries.
-spec all(descriptor()) -> [resource()] | [].
all(Descriptor) ->
    ets:match_object(Descriptor, #resource{_ = '_'}).
