-module(month_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Year, _} = cowboy_req:binding(year, Req),
    {Month, _} = cowboy_req:binding(month, Req),
    Payload = jsx:encode([
        {<<"date">>, list_to_bitstring(io_lib:format("~4..0B-~2..0B", [Year, Month]))},
        {<<"entries">>, []}
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
