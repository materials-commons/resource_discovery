%%% ===================================================================
%%% @doc Server to handle TCP based requests for resources to the hosts
%%%      resources.
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

-module(rd_host_request).
-export([request_resources/2, send_resources/3]).

request_resources(Host, MyHost) ->
    Socket = open_connection_to_handler(Host),
    send(Socket, {resources, MyHost}),
    Resources = recv(Socket),
    gen_tcp:close(Socket),
    Resources.

send_resources(Host, MyHost, MyResources) ->
    Socket = open_connection_to_handler(Host),
    send(Socket, {myresources, MyHost, MyResources}),
    gen_tcp:close(Socket).

open_connection_to_handler(Host) ->
    {ok, Port} = application:get_env(resource_discovery, rd_request_handler_port),
    {ok, Sock} = gen_tcp:connect(Host, Port,[{active, false}]),
    Sock.

send(Socket, Term) ->
    ok = gen_tcp:send(Socket, handyterm:term_to_string(Term)).

recv(Socket) ->
    {ok, Response} = gen_tcp:recv(Socket, 0),
    handyterm:string_to_term(Response).