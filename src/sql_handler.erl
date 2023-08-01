-module(sql_handler).

-behavior(cowboy_handler).

-export([init/2,prepare_query/0]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        prepare_query(),
        %bible_compose:start(),
        Req0),
    {ok, Req, State}.

prepare_query() ->
    Name = <<"query">>,
    Age = 26,
    Person = [
                {name,Name},
                {age,Age}
                ],
    Audits = prepare_audit(),
	io:format("shoe me query data ~p~n",[Audits]),
    R = jsx:encode(Person).

prepare_audit() ->
    Query = <<"select * from mit_ipran_compliance_audit_results20230726 where device_id = ?">>,
    Params = [<<"1988608">>],
    Dbdata = sql_utils:query(Query,Params).

