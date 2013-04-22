%%% ===================================================================
%%% @doc
%%% ===================================================================

-module(rd_ping_server).
-behaviour(gen_server).

%% API
-export([start_link/0, ping/1, ping_all/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {lease_time, start_time}).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, [], []).

ping(Host) ->
    gen_server:cast(?SERVER, {ping, Host}).

ping_all() ->
    gen_server:cast(?SERVER, ping_all).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([LeaseTime]) ->
    Now = lease:seconds_now(),
    TimeLeft = lease:time_left(Now, LeaseTime),
    {ok, #state{lease_time = LeaseTime, start_time = Now}, TimeLeft}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({ping, _Host}, State) ->
    {noreply, State};
handle_cast(ping_all, State) ->
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(timeout, #state{lease_time = LeaseTime} = State)->
    Now = lease:seconds_now(),
    NewTimeout = lease:time_left(Now, LeaseTime),
    {noreply, State#state{start_time = Now}, NewTimeout}.

 terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

