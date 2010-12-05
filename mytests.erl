-module (mytests).
-export([test/0]).

-import(make).

test() ->
    make:all([load]),
    eunit:test(gol).
