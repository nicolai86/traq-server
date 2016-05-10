-module(entrypoint_handler).
-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    BaseURL = "http://" ++ io_lib:format("~s:~w",[extract(cowboy_req:host(Req)), extract(cowboy_req:port(Req))]),
    EntriesURL = BaseURL ++ "/entries/{year:4}{/month:2}{/date:2}",
    Payload = jsx:encode([
        {<<"entries_url">>,list_to_bitstring(EntriesURL)}
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
