-module(lexical_uuid_util).

-export([
    make/2,
    to_string/1,
    to_binary/1
  ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

make(Timestamp, WorkerID) when is_binary(WorkerID) ->
  make(Timestamp, fnv1a:hash(WorkerID));
make(Timestamp, WorkerID) ->
  <<Timestamp:64, WorkerID:64>>.

to_string(UUID) ->
  Parts = split_uuid_for_string(UUID),
  FormatString = "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
  Formatted = io_lib:format(FormatString, Parts),
  lists:flatten(Formatted).

split_uuid_for_string(<<A:32, B:16, C:16, D:8, E:8, F:48>>) ->
  [A, B, C, D, E, F].

to_binary(UUID) when is_binary(UUID) ->
  to_binary(binary_to_list(UUID));
to_binary(UUID) ->
  StringWithoutDashes = lists:filter(fun(Byte) -> Byte /= $- end, UUID),
  string_to_bytes(StringWithoutDashes).

string_to_bytes(UUID) ->
  string_to_bytes(UUID, []).

string_to_bytes([], Converted)->
  list_to_binary(lists:reverse(Converted));
string_to_bytes([X, Y | Tail], Converted)->
  {ok, [Byte], _} = io_lib:fread("~16u", [X, Y]),
  string_to_bytes(Tail, [Byte | Converted]).

-ifdef(TEST).

string_to_binary_test() ->
  ?assertEqual(<<0,4,182,52,223,207,101,87,215,0,75,6,65,148,70,108>>, to_binary(<<"0004b634-dfcf-6557-d700-4b064194466c">>)),
  ?assertEqual(<<0,4,182,53,5,61,70,40,215,0,75,6,65,148,70,108>>, to_binary("0004b635-053d-4628-d700-4b064194466c")),
  ok.
binary_to_string_test() ->
  ?assertEqual("0004b634-dfcf-6557-d700-4b064194466c", to_string(<<0,4,182,52,223,207,101,87,215,0,75,6,65,148,70,108>>)),
  ?assertEqual("0004b635-053d-4628-d700-4b064194466c", to_string(<<0,4,182,53,5,61,70,40,215,0,75,6,65,148,70,108>>)),
  ok.

-endif.
