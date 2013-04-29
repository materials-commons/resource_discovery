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
