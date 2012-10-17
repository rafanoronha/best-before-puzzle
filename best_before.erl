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
  MonthAndDayArgs = [A, B],
  GetMonthAndDay = fun get_month_and_day_when_year_is_known/2,
  GetYear = fun(_) -> Year end,
  GetTuple = fun date_tuple_from_known_year/3,
  do_parse_date(MonthAndDayArgs, GetMonthAndDay, GetYear, GetTuple).
parse_date([A, B, C]) ->
  MonthAndDayArgs = [A, B, C],
  GetMonthAndDay = fun get_month_and_day_when_year_is_unknown/2,
  GetYear = fun([Month, Day]) ->
    [Year] = [A, B, C] -- [Month, Day],
    Year
  end,
  GetTuple = fun date_tuple_from_deducted_year/3,
  do_parse_date(MonthAndDayArgs, GetMonthAndDay, GetYear, GetTuple).

do_parse_date(MonthAndDayArgs, GetMonthAndDay, GetYear, GetTuple) ->
  EligibleToMonth = eligible_to_month(MonthAndDayArgs),
  case EligibleToMonth of
    [] ->
      { error, month_not_found } ;
    _List ->
      [M, D] = GetMonthAndDay(MonthAndDayArgs, EligibleToMonth),
      Y = GetYear([M, D]),
      GetTuple(Y, M, D)
  end.

get_month_and_day_when_year_is_known([A, B], [M]) ->
  [D] = [A, B] -- [M],
  [M, D];
get_month_and_day_when_year_is_known([A, B], [A, B]) ->
  lists:sort([A, B]).

get_month_and_day_when_year_is_unknown([A, B, C], [M]) ->
  [Z, X] = [A, B, C] -- [M],
  [_Y, D] = lists:sort([Z, X]),
  [M, D];
get_month_and_day_when_year_is_unknown([A, B, C], [Z, X]) ->
  [Y, M] = lists:sort([Z, X]),
  [D] = [A, B, C] -- [Y, M],
  [M, D];
get_month_and_day_when_year_is_unknown([A, B, C], [A, B, C]) ->
  [_Y, M, D] = lists:sort([A, B, C]),
  [M, D].

get_month_and_day(L) when is_list(L) ->
  case eligible_to_month(L) of
    [] ->
      { error, month_not_found } ;
    [A] ->
      [A] ++ [hd(lists:reverse(lists:sort(L -- [A])))];
    [A, B] ->
      UnknownYear = 3 == length(L),
      GetMonth = case UnknownYear of 
        true ->
         fun() -> hd(lists:reverse(lists:sort([A, B]))) end;
        _ ->
         fun() -> hd(lists:sort([A, B])) end
      end,
      Month = GetMonth(),    
      [Month] ++ [hd(lists:reverse(lists:sort(L -- [Month])))];
    [A, B, C] ->
      [_Year, Month, Day] = lists:sort([A, B, C]),
      [Month, Day]
  end.

date_tuple_from_known_year(Y, M, D) ->
  { Y, M, D }.

date_tuple_from_deducted_year(Y, M, D) ->
  { 2000 + Y, M, D }.

format_date({ Year, Month, Day }) ->
  integer_to_list(Year) ++ "/" ++ integer_to_list(Month) ++ "/" ++ integer_to_list(Day).

eligible_to_month(Number) when is_integer(Number) ->
  (12 >= Number) and (1 =< Number);
eligible_to_month(L) when is_list(L) ->
  lists:filter(fun eligible_to_month/1, L).

bin_to_int(Bin) ->
  { Int, [] } = string:to_integer(binary_to_list(Bin)),
  Int.
  
