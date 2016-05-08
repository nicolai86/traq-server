-module(entrypoint_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    BaseURL = "http://" ++ io_lib:format("~s:~w",[extract(cowboy_req:host(Req)), extract(cowboy_req:port(Req))]),

    {Today,_} = erlang:universaltime(),
    {Year, Month, Day} = Today,
    TodayURL = BaseURL ++ "/date/" ++ io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),

    {Year, WeekNum} = calendar:iso_week_number(),
    WeekURL = BaseURL ++ "/weeks/" ++ io_lib:format("~2..0B", [WeekNum]),

    MonthURL = BaseURL ++ "/months/" ++ io_lib:format("~2..0B", [Month]),
    YearURL = BaseURL ++ "/years/" ++ io_lib:format("~4..0B", [Year]),
    Payload = jsx:encode([
        {<<"today_url">>,list_to_bitstring(TodayURL)},
        {<<"week_url">>,list_to_bitstring(WeekURL)},
        {<<"month_url">>,list_to_bitstring(MonthURL)},
        {<<"year_url">>,list_to_bitstring(YearURL)}
    ]),
    {ok, Req2} = cowboy_req:reply(200,
        [
          {<<"content-type">>, <<"application/json">>}
        ],
        Payload,
        Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

extract({Value, _Req}) ->
    Value.
