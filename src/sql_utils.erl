-module(sql_utils).
-author('zouqd@opengoss.com').
-export([query/2]).

init() ->
    io:format("init..........~n"),
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},
                                {password, "123456"}, {database, "economist"},
                                {auth_method,<<"mysql_native_password">>}]),
    Pid.

query(Query,Params) ->
    Pid = init(),
    case mysql:query(Pid, Query, Params) of
        {ok,Keys,Values} -> Map = format(Keys,Values);
        _ -> Map = []
    end.

format(Keys,Values) ->
    Akeys = [binary_to_atom(K) || K <- Keys],
    Result = [lists:zip(Akeys,V) || V <- Values].
