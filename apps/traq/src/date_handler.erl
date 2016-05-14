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
        _ ->
            {ok, Req, [{data_directory, DataDirectory}]}
    end.

handle(Req, State) ->
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
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
