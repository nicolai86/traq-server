%% Feel free to use, reuse and abuse the code in this file.

-module(traq).

%% API.
-export([start/0]).

%% API.

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(traq).
