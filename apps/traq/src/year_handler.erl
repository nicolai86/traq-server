-module(year_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Year, _} = cowboy_req:binding(year, Req),
    Payload = jsx:encode([
        {<<"year">>, Year},
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
