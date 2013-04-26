%%% ===================================================================
%%% @doc
%%% ===================================================================

-module(rd_pong_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {lsock}).

%% ===================================================================
%% API
%% ===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
	case handle_request(Socket, RawData) of
		ok -> {stop, normal, State};
		{error, Reason} -> {stop, {error, Reason}, State}
	end;
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State)->
    {ok, _Sock} = gen_tcp:accept(LSock),
    rd_pong_sup:start_child(),
    {noreply, State}.

 terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

handle_request(Socket, "PING") ->
    gen_tcp:send(Socket, "PONG");
handle_request(_Socket, _Request) ->
    %% Error log an unknown command.
    {error, unknown_command}.



