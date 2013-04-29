%%% ===================================================================
%%% @doc Implements utility functions for lease timeouts.
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

-module(lease).
-export([time_left/2, seconds_now/0]).

%% @doc TTL never expires
-spec time_left(integer(), infinity | integer()) -> infinity | integer().
time_left(_StartTime, infinity) ->
    infinity;
%% @doc Convert TTL to milliseconds
time_left(StartTime, LeaseTime) ->
    CurrentTimeInSeconds = seconds_now(),
    TimeElapsed = CurrentTimeInSeconds - StartTime,
    lease_time_left_in_milliseconds(LeaseTime, TimeElapsed).

%% @doc Get seconds now
-spec seconds_now() -> integer().
seconds_now() ->
    Now = calendar:local_time(),
    calendar:datetime_to_gregorian_seconds(Now).

%% @doc Lease time left in milliseconds. If lease passed then return 0.
-spec lease_time_left_in_milliseconds(integer(), integer()) -> integer().
lease_time_left_in_milliseconds(LeaseTime, TimeElapsed) ->
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time -> Time * 1000 % Convert to milliseconds
    end.