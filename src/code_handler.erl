-module(code_handler).

-behavior(cowboy_handler).

-export([init/2,prepare_json/0]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        prepare_json(),
        Req0),
    {ok, Req, State}.

prepare_json() ->
    Name = <<"Acarnan">>,
    Age = 24,
    Person = [
                {name,Name},
                {age,Age}
                ],
    R = jsx:encode(Person).

