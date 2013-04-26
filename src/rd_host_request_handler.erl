%%% ===================================================================
%%% @doc
%%% ===================================================================

-module(rd_host_request_handler).
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
    handle_request(Socket, RawData),
    {stop, normal, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State)->
    {ok, _Sock} = gen_tcp:accept(LSock),
    rd_host_request_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local
%% ===================================================================

handle_request(Socket, "RESOURCES") ->
    Resources = gen_host_server:fetch(),
    ResourcesAsString = handyterm:term_to_string(Resources),
    gen_tcp:send(Socket, ResourcesAsString);
handle_request(_Socket, _Request) ->
    %% Error log an unknown command.
    ok.



