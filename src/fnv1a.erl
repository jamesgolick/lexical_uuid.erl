-module(fnv1a).

-author("Cliff Moon").

-export([hash/1]).

-define(OFFSET_BASIS, 14695981039346656037).
-define(FNV_PRIME, 1099511628211).

hash(Term) when is_binary(Term) ->
  fnv_int(?OFFSET_BASIS, 0, Term);

hash(Term) ->
  fnv_int(?OFFSET_BASIS, 0, term_to_binary(Term)).

fnv_int(Hash, ByteOffset, Bin) when erlang:byte_size(Bin) == ByteOffset ->
  Hash;

fnv_int(Hash, ByteOffset, Bin) ->
  <<_:ByteOffset/binary, Octet:8, _/binary>> = Bin,
  Xord = Hash bxor Octet,
  fnv_int((Xord * ?FNV_PRIME) rem (2 bsl 63), ByteOffset+1, Bin).
