-module(lexical_uuid_clock).

-export([
    get_next_timestamp/1
  ]).

get_next_timestamp(LastTime) ->
  case tick() of
    Tick when Tick > LastTime ->
      Tick;
    _ ->
      Next = LastTime + 1,
      Next
  end.

tick() ->
  {Megasecs, Secs, Microsecs} = erlang:now(),
  (((Megasecs * 1000000) + Secs) * 1000000) + Microsecs.
