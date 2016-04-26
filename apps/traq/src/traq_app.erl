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
        {'_', [{"/", hello_handler, []}]}
    ]),
    cowboy:start_http(my_http_listener, NumOfAcceptors, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    traq_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
