-module(rd_new_host_monitor).
-behaviour(gen_stomp).

%% API
-export([start_link/4]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StompHost, StompPort, StompUser, StompPassword) ->
    gen_stomp:start_link({local, ?SERVER}, ?MODULE,
                [StompHost, StompPort, StompUser, StompPassword], []).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @private
init([]) ->
    ok.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Ignore, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
