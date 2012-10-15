-module(best_before).
-compile(export_all).

parse_string(S) ->
  parse_bin(list_to_binary(S)).

parse_bin(<<A:2/binary, "/", B:2/binary, "/", Year:4/binary>>) ->
  [YearNum, ANum, BNum] = lists:map(fun bin_to_int/1, [Year,A,B]),
  Date = parse_date(YearNum, [ANum, BNum]),
  case Date of
    { error, _ } ->
      nok;
    _ValidDate   ->
      format_date(Date)
  end;
parse_bin(_) ->
  nok.

parse_date(Year, [A|[B]]) ->
  case eligible_to_month([A|[B]]) of
    [] ->
      { error, invalid_date };
    [_H|_T] -> 
      [Month, Day] = lists:sort([A, B]),
      { Year, Month, Day }
  end.
parse_date([A|[B|[C]]]) ->
  { error, invalid_date }.

format_date({ Year, Month, Day }) ->
  integer_to_list(Year) ++ "/" ++ integer_to_list(Month) ++ "/" ++ integer_to_list(Day).

is_month(Number) ->
  (12 >= Number) and (1 =< Number).

eligible_to_month(L) when is_list(L) ->
  lists:filter(fun is_month/1, L).

bin_to_int(Bin) ->
  { Int, [] } = string:to_integer(binary_to_list(Bin)),
  Int.
  
