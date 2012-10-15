-module(best_before).
-export([parse_string/1]).

parse_string(S) ->
  parse_bin(list_to_binary(S)).

parse_bin(<<A:2/binary, "/", B:2/binary, "/", C:4/binary>>) ->
  ok;
parse_bin(_) ->
  nok.
