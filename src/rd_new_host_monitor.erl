-module(rd_new_host_monitor).
-behaviour(gen_stomp).

-include("rd.hrl").

%% API
-export([start_link/4]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

% Nothing right now, but keeping for future expansion.
-record(state, {}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StompHost, StompPort, StompUser, StompPassword) ->
    StartupShutdownQueue = ?SS_QUEUE,
    gen_stomp:start_link({local, ?SERVER}, ?MODULE, StompHost, StompPort,
            StompUser, StompPassword, [{StartupShutdownQueue, []}], []).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast([{message, Message}, {queue, _Queue}], State)->
    handle_message(Message),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Ignore, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

handle_message([{type, "MESSAGE"}, {header, _Header}, {body, Body}]) ->
    handle_event(handyterm:string_to_term(Body));
handle_message(_Message) ->
    ok.

handle_event(#hostevent{host = Host, event = up}) ->
    resource_discovery:create(Host);
handle_event(#hostevent{host = Host, event = down}) ->
    resource_discovery:delete(Host).
