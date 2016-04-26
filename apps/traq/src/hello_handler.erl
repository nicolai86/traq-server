-module(hello_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    Payload = jsx:encode([{<<"Hello">>,<<"World">>}]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        Payload,
        Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
