-module(fqdn).

-compile({no_auto_import,[get/0]}).

-export([get/0]).

get() ->
  {ok, Hostname} = inet:gethostname(),
  {ok, Hostent} = inet:gethostbyname(Hostname),
  {hostent, FQDN, _Aliases, inet, _, _Addresses} = Hostent,
  list_to_binary(FQDN).
