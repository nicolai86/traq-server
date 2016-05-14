-module(year_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, Opts) ->
    {_, DataDirectory} = lists:keyfind(data_directory, 1, Opts),
    {ok, Req, [{data_directory, DataDirectory}]}.

handle(Req, State) ->
    {_, DataDirectory} = lists:keyfind(data_directory, 1, State),
    {Year, _} = cowboy_req:binding(year, Req),
    {Project, _} = cowboy_req:binding(project, Req),
    Payload = jsx:encode([
        {<<"project">>, Project},
        {<<"date">>, list_to_bitstring(integer_to_list(Year))},
        {<<"entries">>, entry_manager:entries(DataDirectory, Project, Year)}
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
