-module(best_before).
-compile(export_all).

s(S) when is_list(S) ->
  case bin_to_date(list_to_binary(S)) of
    { error, _ } ->
      S ++ " is illegal";
    Date ->
      format_date(Date)
  end.

bin_to_date(<<Year:4/binary, "/", A:2/binary, "/", B:2/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<Year:4/binary, "/", A:2/binary, "/", B:1/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<Year:4/binary, "/", A:1/binary, "/", B:1/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<Year:4/binary, "/", A:1/binary, "/", B:2/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:2/binary, "/", Year:4/binary, "/", B:2/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:2/binary, "/", Year:4/binary, "/", B:1/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:1/binary, "/", Year:4/binary, "/", B:1/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:1/binary, "/", Year:4/binary, "/", B:2/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:2/binary, "/", B:2/binary, "/", Year:4/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:2/binary, "/", B:1/binary, "/", Year:4/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:1/binary, "/", B:1/binary, "/", Year:4/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:1/binary, "/", B:2/binary, "/", Year:4/binary>>) ->
  known_year_bin_to_date(Year, A, B);
bin_to_date(<<A:2/binary, "/", B:2/binary, "/", C:2/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:2/binary, "/", B:2/binary, "/", C:1/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:2/binary, "/", B:1/binary, "/", C:1/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:2/binary, "/", B:1/binary, "/", C:2/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:1/binary, "/", B:2/binary, "/", C:2/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:1/binary, "/", B:2/binary, "/", C:1/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:1/binary, "/", B:1/binary, "/", C:1/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(<<A:1/binary, "/", B:1/binary, "/", C:2/binary>>) ->
  unknown_year_bin_to_date(A, B, C);
bin_to_date(Bin) when is_binary(Bin) ->
  { error, invalid_bin }.

known_year_bin_to_date(Year, A, B) ->
  [YearNum, ANum, BNum] = lists:map(fun bin_to_int/1, [Year, A, B]),
  parse_date(YearNum, [ANum, BNum]).

unknown_year_bin_to_date(A, B, C) ->
  parse_date(lists:map(fun bin_to_int/1, [A, B, C])).

parse_date(Year, [A, B]) ->
  MonthAndDay = get_month_and_day([A, B]),
  case MonthAndDay of
    [] ->
      { error, invalid_date };
    [Month, Day] -> 
      { Year, Month, Day }
  end.
parse_date([A, B, C]) ->
  MonthAndDay = get_month_and_day([A, B, C]),
  case MonthAndDay of
    [] ->
      { error, invalid_date };
    [Month, Day] -> 
      [Year] = [A, B, C] -- [Month, Day],
      { Year, Month, Day }
  end.

get_month_and_day(L) when is_list(L) ->
  case eligible_to_month(L) of
    [] ->
      [];
    [A, B] ->
      lists:sort([A, B]);
    [A, B, C] ->
      [_Year, Month, Day] = lists:sort([A, B, C]),
      [Month, Day]
  end.

format_date({ Year, Month, Day }) ->
  integer_to_list(Year) ++ "/" ++ integer_to_list(Month) ++ "/" ++ integer_to_list(Day).

is_month(Number) ->
  (12 >= Number) and (1 =< Number).

eligible_to_month(L) when is_list(L) ->
  lists:filter(fun is_month/1, L).

bin_to_int(Bin) ->
  { Int, [] } = string:to_integer(binary_to_list(Bin)),
  Int.
  
