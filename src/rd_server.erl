%%% ===================================================================
%%% Server for tracking and sharing resources. We use a simple gossip
%%% protocol that uses a STOMP message queue to announce changes. The
%%% sequence protocol
%%% ===================================================================

-module(rd_server).
-behaviour(gen_stomp).

%% API
-export([start_link/4, add_local_resource/2, fetch_resources/1]).

%% gen_stomp callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(resource, {host, type, resource}).

-record(state, {local_resources, known_resources}).

%% ===================================================================
%% API
%% ===================================================================
start_link(Server, Port, Username, Password) ->
    gen_stomp:start_link({local, ?SERVER}, Server, Port, Username, Password,
        [{"/topic/rd_node_announce", []}, {"/topic/rd_node_down", []}], []).

add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

%% ===================================================================
%% gen_stomp callbacks
%% ===================================================================
init([]) ->
    {ok, #state{local_resources = dict:new(), known_resources = dict:new()}}.

handle_call(Message, _From, State) ->
    {reply, {ok, Message}, State}.

handle_cast([{message, _Message}, {queue, "/topic/rd_node_announce"}], State) ->
    {noreply, State};
handle_cast([{message, _Message}, {queue, "/topic/rd_node_down"}], State) ->
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Ignore, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
