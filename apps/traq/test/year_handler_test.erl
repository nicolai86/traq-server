-module(year_handler_test).

-compile(export_all).

-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").

before_suite(Config) ->
    io:format("foo~n",[]),
    application:start(traq_sup),
    Config.

after_suite(_Config) ->
    application:stop(traq_sup),
    ok.

test_happy_path(_Config) ->
    Response = ?perform_get("http://localhost:8080/entries/2016"),
    ?assert_status(200, Response),
    ?assert_body("saymyname", Response),
    ok.

