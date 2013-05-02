%%% ===================================================================
%%% @doc Listens for host up/down events and starts/stops new resource
%%%      servers for the host.
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

-module(rd_new_host_monitor).
-behaviour(gen_stomp).

-include("rd.hrl").

%% API
-export([start_link/5]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-record(state, {
        this_host :: string() % The host we are running on.
    }).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Starts a new server, connecting to the specified STOMP server
%%      listening on the broacast start/shutdown queue for events
%%      to handle.
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

%% @private
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast([{message, Message}, {queue, _Queue}],
        #state{this_host = ThisHost} = State)->
    handle_message(Message, ThisHost),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Ignore, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Local functions
%% ===================================================================

%% Handles messages from the startup/shutdown queue.
handle_message([{type, "MESSAGE"}, {header, _Header}, {body, Body}], ThisHost) ->
    handle_event(handyterm:string_to_term(Body), ThisHost);
%% Ignore messages that are not type {type, "MESSAGE"}
handle_message(_Message, _ThisHost) ->
    ok.

%% Handle host up
handle_event(#hostevent{host = Host, event = up}, ThisHost) ->
    case Host =:= ThisHost of
        true -> ok;
        false ->
            resource_discovery:create(Host),
            {ok, Resources} = resource_discovery:lookup(ThisHost),
            rd_host_request:send_resources(Host, ThisHost, Resources)
    end;

%% Handle host down
handle_event(#hostevent{host = Host, event = down}, ThisHost) ->
    io:format("hostevent down for host ~p~n", [Host]),
    case Host =:= ThisHost of
        true -> ok;
        false -> resource_discovery:delete_host(Host)
    end.
