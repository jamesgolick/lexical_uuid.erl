-module(lexical_uuid_util).

-export([
    make/2,
    get_fqdn/0,
    to_string/1
  ]).

make(Timestamp, WorkerID) when is_binary(WorkerID) ->
  make(Timestamp, fnv1a:hash(WorkerID));
make(Timestamp, WorkerID) ->
  <<Timestamp:64, WorkerID:64>>.

to_string(UUID) ->
  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", split_uuid_for_string(UUID))).

split_uuid_for_string(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
  [TL, TM, THV, CSR, CSL, N].

get_fqdn() ->
  {ok, Hostname} = inet:gethostname(),
  {ok, Hostent} = inet:gethostbyname(Hostname),
  {hostent, FQDN, _Aliases, inet, _, _Addresses} = Hostent,
  list_to_binary(FQDN).
