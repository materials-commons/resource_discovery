%%% ===================================================================
%%% @doc Implements utility functions for lease timeouts.
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