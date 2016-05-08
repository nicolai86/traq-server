%%%-------------------------------------------------------------------
%% @doc traq public API
%% @end
%%%-------------------------------------------------------------------

-module(traq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = 8080,
    NumOfAcceptors = 100,
    io:format("Listening on ~p~n", [Port]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", entrypoint_handler, []},
            {"/date/:date", [{date, function, fun is_date/1}], date_handler, []},
            {"/weeks/:week", [
                    {week, int},
                    {week, function, fun is_week/1}
                ], week_handler, []},
            {"/months/:month", [
                    {month, int},
                    {month, function, fun is_month/1}
                ], month_handler, []},
            {"/years/:year", [{year, int}], year_handler, []}
        ]}
    ]),
    cowboy:start_http(traq_listener, NumOfAcceptors, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    traq_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

is_month(Month) when Month < 13, Month > 0 -> true;
is_month(_) -> false.

is_week(WeekNum) when WeekNum > 0, WeekNum < 54 -> true;
is_week(_) -> false.

is_date(DateStr) ->
    [YearStr, MonthStr, DayStr] = string:tokens(binary_to_list(DateStr), "-"),
    {Year, _} = string:to_integer(YearStr),
    {Month, _} = string:to_integer(MonthStr),
    {Date, _} = string:to_integer(DayStr),
    calendar:valid_date({Year, Month, Date}).
