%%% ===================================================================
%%% @doc
%%% ===================================================================

-module(rd_timeout).
-behaviour(gen_stomp).

%% API
-export([start_link/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {lsock}).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    gen_stomp:start_link(?MODULE, "localhost", 61613, "guest", "guest", [], []).
    %gen_server:start_link(?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    gen_stomp:cast(self(), startup),
    {ok, #state{lsock = 1}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(startup, State) ->
    io:format("startup now timeout for 2 seconds~n"),
    gen_stomp:send("/topic/echo", "Hello", [], 2000),
    {noreply, State, 2000};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    {stop, normal, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(bob, State) ->
    io:format("Received Bob~n"),
    {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State)->
    io:format("rd_timeout timeout~n"),
    {noreply, State, 2000};
handle_info(Msg, State) ->
    io:format("rd_timeout handle everything: ~p~n", [Msg]),
    {noreply, State}.

 terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================




