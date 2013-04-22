%%% ===================================================================
%%% @doc rd_store maps resource processes to pids. Based on code used
%%% in Erlang and OTP in Action for simple_cache.
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
lookup(Host) ->
    case ets:lookup(?TABLE, Host) of
        [{Host, Pid}] -> {ok, Pid};
        [] -> {error, not_found}
    end.

%% @doc Returns all entries.
all() ->
    [Entry || [Entry] <- ets:match(?TABLE, '$1')].


%% @doc Remove the host to pid mapping (by pid)
delete_by_pid(Pid) ->
    ets:match_delete(?TABLE, {'_', Pid}).

%% @doc Remove the host to pid mapping (by host)
delete_by_host(Host) ->
    ets:match_delete(?TABLE, {Host, '_'}).
