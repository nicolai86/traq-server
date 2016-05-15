# traq

An OTP application

# Build

    $ rebar3 compile

# Development

    $ rebar3 shell
    > sync:go().

## Tests

    $ rebar3 eunit

## Examples

    $ curl -XPOST -d'{"action": "#work"}' http://localhost:8080/sample/2020/11/17
    $ curl -XPOST -d'{"action": "stop"}' http://localhost:8080/sample/2020/11/17
    $ curl http://localhost:8080/sample/2020/11/17
    $ curl http://localhost:8080/sample/2020/11
    $ curl http://localhost:8080/sample/2020
