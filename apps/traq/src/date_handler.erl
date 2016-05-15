-module(date_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, Opts) ->
    {_, DataDirectory} = lists:keyfind(data_directory, 1, Opts),
    {Year, _} = cowboy_req:binding(year, Req),
    {Month, _} = cowboy_req:binding(month, Req),
    {Date, _} = cowboy_req:binding(date, Req),
    case calendar:valid_date(Year, Month, Date) of
        false ->
            {ok, Req2} = cowboy_req:reply(500, [
                    {<<"content-type">>, <<"application/json">>}
                ], "{\"error\":\"date.invalid\"}", Req),
            {shutdown, Req2, no_state};
        true ->
            {ok, Req, [{data_directory, DataDirectory}]}
    end.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    date_handler(Method, Req, State).

date_handler(<<"GET">>, Req, State) ->
    {_, DataDirectory} = lists:keyfind(data_directory, 1, State),
    {Year, _} = cowboy_req:binding(year, Req),
    {Month, _} = cowboy_req:binding(month, Req),
    {Date, _} = cowboy_req:binding(date, Req),
    {Project, _} = cowboy_req:binding(project, Req),
    Payload = jsx:encode([
        {<<"project">>, Project},
        {<<"date">>, list_to_bitstring(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Date]))},
        {<<"entries">>, entry_manager:entries(DataDirectory, Project, Year, Month, Date)}
    ]),
    {ok, Req2} = cowboy_req:reply(200,
        [
          {<<"content-type">>, <<"application/json">>}
        ],
        Payload,
        Req),
    {ok, Req2, State};

date_handler(<<"POST">>, Req, State) ->
    case cowboy_req:has_body(Req) of
        false ->
            {ok, Req2} = cowboy_req:reply(200,
                [
                  {<<"content-type">>, <<"application/json">>}
                ],
                <<"{\"error\":\"missing action\"}">>,
                Req),
            {error, 400, Req2};
        true ->
            {ok, Body, _} = cowboy_req:body(Req),
            Data = jsx:decode(Body),
            {_, Action} = lists:keyfind(<<"action">>,1,Data),
            {_, DataDirectory} = lists:keyfind(data_directory, 1, State),
            {Year, _} = cowboy_req:binding(year, Req),
            {Month, _} = cowboy_req:binding(month, Req),
            {Date, _} = cowboy_req:binding(date, Req),
            {Project, _} = cowboy_req:binding(project, Req),
            entry_manager:append(DataDirectory, Project, Year, Month, Date, Action),
            Payload = jsx:encode([
                {<<"project">>, Project},
                {<<"date">>, list_to_bitstring(io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Date]))},
                {<<"entries">>, entry_manager:entries(DataDirectory, Project, Year, Month, Date)}
            ]),
            {ok, Req2} = cowboy_req:reply(200,
                [
                  {<<"content-type">>, <<"application/json">>}
                ],
                Payload,
                Req),
            {ok, Req2, State}
    end.

terminate(_Req, _State) ->
    ok.
