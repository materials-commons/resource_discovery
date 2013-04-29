-module(rd_new_host_monitor).
-behaviour(gen_stomp).

-include("rd.hrl").

%% API
-export([start_link/5]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

% Nothing right now, but keeping for future expansion.
-record(state, {this_host}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StompHost, StompPort, StompUser, StompPassword, ThisHost) ->
    StartupShutdownQueue = ?SS_QUEUE,
    gen_stomp:start_link({local, ?SERVER}, ?MODULE, StompHost, StompPort,
            StompUser, StompPassword, [{StartupShutdownQueue, []}], [ThisHost]).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================

%% @private
init([ThisHost]) ->
    {ok, #state{this_host = ThisHost}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast([{message, Message}, {queue, _Queue}],
        #state{this_host = ThisHost} = State)->
    handle_message(Message, ThisHost),
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

handle_message([{type, "MESSAGE"}, {header, _Header}, {body, Body}], ThisHost) ->
    handle_event(handyterm:string_to_term(Body), ThisHost);
handle_message(_Message, _ThisHost) ->
    ok.

handle_event(#hostevent{host = Host, event = up}, ThisHost) ->
    when_not_this_host(create, Host, ThisHost);
handle_event(#hostevent{host = Host, event = down}, ThisHost) ->
    when_not_this_host(delete, Host, ThisHost).

when_not_this_host(Command, Host, ThisHost) ->
    case Host =:= ThisHost of
        true -> ok;
        false -> resource_discovery:Command(Host)
    end.
