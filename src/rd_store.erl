%%% ===================================================================
%%% @doc Maps resource processes to pids. Based on code used
%%% in Erlang and OTP in Action for simple_cache.
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

-module(rd_store).
-export([init/0, insert/2, lookup/1, delete_by_pid/1,
            delete_by_host/1, all/0]).

-define(TABLE, ?MODULE).

%% @doc Creates shared database.
-spec init() -> atom().
init() ->
    ?TABLE = ets:new(?TABLE, [public, named_table]).

%% @doc Create a pid to host resource mapping
-spec insert(string(), pid()) -> true.
insert(Host, Pid) ->
    ets:insert(?TABLE, {Host, Pid}).

%% @doc Lookup the Pid for the Host resource
-spec lookup(string()) -> {ok, pid()} | {error, not_found}.
lookup(Host) ->
    case ets:lookup(?TABLE, Host) of
        [{Host, Pid}] -> {ok, Pid};
        [] -> {error, not_found}
    end.

%% @doc Returns all entries.
-spec all() -> [tuple(string(), pid())] | [].
all() ->
    [Entry || [Entry] <- ets:match(?TABLE, '$1')].


%% @doc Remove the host to pid mapping (by pid)
-spec delete_by_pid(pid()) -> true.
delete_by_pid(Pid) ->
    ets:match_delete(?TABLE, {'_', Pid}).

%% @doc Remove the host to pid mapping (by host)
-spec delete_by_host(string()) -> true.
delete_by_host(Host) ->
    ets:match_delete(?TABLE, {Host, '_'}).
