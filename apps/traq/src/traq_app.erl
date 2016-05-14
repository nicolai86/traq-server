%%%-------------------------------------------------------------------
%% @doc traq public API
%% @end
%%%-------------------------------------------------------------------

-module(traq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([init_dispatch/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API
%%====================================================================

init_dispatch(DataDirectory) ->
    cowboy_router:compile([
        {'_', [
            {"/", entrypoint_handler, []},
            {"/:project/:year", [
                    {year, int}
                ], year_handler, [{data_directory, DataDirectory}]},
            {"/:project/:year/:month", [
                    {year, int},
                    {month, int},
                    {month, function, fun is_month/1}
                ], month_handler, [{data_directory, DataDirectory}]},
            {"/:project/:year/:month/:date", [
                    {year, int},
                    {month, int},
                    {month, function, fun is_month/1},
                    {date, int}
                ], date_handler, [{data_directory, DataDirectory}]}
        ]}
    ]).

start(_StartType, _StartArgs) ->
    Port = 8080,
    NumOfAcceptors = 100,
    io:format("Listening on ~p~n", [Port]),
    DataDirectory = os:getenv("TRAQ_DATA_DIR"),
    Dispatch = init_dispatch(DataDirectory),
    cowboy:start_http(traq_listener, NumOfAcceptors, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    traq_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

is_month(Month) when Month < 13, Month > 0 -> true;
is_month(_) -> false.

-ifdef(TEST).

is_month_jan_dec_test() ->
    Months = lists:seq(1, 12, 1),
    lists:foreach(fun(M) -> ?assert(is_month(M) =:= true) end, Months).

invalid_is_month_test() ->
    Exceptions = [0, 13, -2],
    lists:foreach(fun(M) -> ?assertNot(is_month(M) =:= true) end, Exceptions).

-endif.
