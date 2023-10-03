-module(emysql).
-author('zouqd@opengoss.com').
-export([
            query/2,
            insert/2,
            update/2,
            update/3
        ]).

init() ->
    io:format("init..........~n"),
    {ok, Pid} = mysql:start_link([{host, "159.138.11.4"}, {user, "root"},
                                  {port,3336},
                                {password, "123456"}, {database, "economist"},
                                {auth_method,<<"mysql_native_password">>}]),
    Pid.

query(Query,Params) ->
    Pid = init(),
    case mysql:query(Pid, Query, Params) of
        {ok,Keys,Values} -> Map = format(Keys,Values);
        _ -> Map = []
    end.

insert(Tab, Record) when is_atom(Tab) ->
    Query = encode_insert(Tab, Record),
    sqlquery(Query).

insert(_Tab, _Fields, Values) when length(Values) == 0 ->
    {updated, {0, 0}};

insert(Tab, Fields, Values) when length(Values) > 0 ->
    sqlquery(encode_insert(Tab, Fields, Values)).

encode_insert(Tab, Record) ->
    {Fields, Values} = lists:unzip([{atom_to_list(F), encode(V)}
        || {F, V} <- Record]),
    ["insert into ", atom_to_list(Tab), "(",
         string:join(Fields, ","), ") values(",
         string:join(Values, ","), ");"].

encode_insert(Tab, Fields, Rows) ->
    Encode = fun(Row) -> string:join([encode(V) || V <- Row], ",") end,
    Rows1 = [lists:concat(["(", Encode(Row), ")"]) || Row <- Rows],
    ["insert into ", atom_to_list(Tab), "(",
        string:join([atom_to_list(F) || F <- Fields], ","),
        ") values", string:join(Rows1, ","), ";"].

update(Tab, Record) when is_atom(Tab)
    and is_list(Record) ->
    case proplists:get_value(id, Record) of
    undefined ->
        Updates = string:join([encode_column(Col) || Col <- Record], ","),
        Query = ["update ", atom_to_list(Tab), " set ", Updates, ";"],
        sqlquery(Query);
    Id ->
        update(Tab, lists:keydelete(id, 1, Record), {id, Id})
    end.

update(Tab, Record, Where) ->
    Update = string:join([encode_column(Col) || Col <- Record], ","),
    Query = ["update ", atom_to_list(Tab), " set ", Update,
        " where ", encode_where(Where), ";"],
    sqlquery(Query).

encode_where({'and', L, R}) ->
    encode_where(L) ++ " and " ++ encode_where(R);

encode_where({'and', List}) when is_list(List) ->
    string:join([encode_where(E) || E <- List], " and ");

encode_where({'or', L, R}) ->
    encode_where(L) ++ " or " ++ encode_where(R);

encode_where({'or', List}) when is_list(List) ->
    string:join([encode_where(E) || E <- List], " or ");

encode_where({like, Field, Value}) ->
    atom_to_list(Field) ++ " like " ++ encode(Value);

encode_where({'<', Field, Value}) ->
    atom_to_list(Field) ++ " < " ++ encode(Value);

encode_where({'>', Field, Value}) ->
    atom_to_list(Field) ++ " > " ++ encode(Value);

encode_where({'in', Field, Values}) ->
    InStr = string:join([encode(Value) || Value <- Values], ","),
    atom_to_list(Field) ++ " in (" ++ InStr ++ ")";

encode_where({Field, Value}) ->
    atom_to_list(Field) ++ " = " ++ encode(Value).

encode_column({F, V}) when is_atom(F) ->
    lists:concat([atom_to_list(F), "=", encode(V)]).

sqlquery(QueryList) ->
    Pid = init(),
    Query = iolist_to_binary(QueryList),
    io:format("we got sql query data ~p~n",[Query]),
    case mysql:query(Pid, Query) of
        {ok,Keys,Values} -> Map = format(Keys,Values);
        _ -> Map = []
    end.

encode(Val) ->
    encode(Val, false).
encode(Val, false) when Val == undefined; Val == null ->
    "NULL";
encode(Val, true) when Val == undefined; Val == null ->
    <<"NULL">>;
encode(Val, false) when is_binary(Val) ->
    binary_to_list(quote(Val));
encode(Val, true) when is_binary(Val) ->
    quote(Val);
encode(Val, true) ->
    list_to_binary(encode(Val,false));
encode(Val, false) when is_atom(Val) ->
    quote(atom_to_list(Val));
encode(Val, false) when is_list(Val) ->
    quote(Val);
encode(Val, false) when is_integer(Val) ->
    integer_to_list(Val);
encode(Val, false) when is_float(Val) ->
    [Res] = io_lib:format("~w", [Val]),
    Res;
encode({datetime, Val}, AsBinary) ->
    encode(Val, AsBinary);
encode({{Year, Month, Day}, {Hour, Minute, Second}}, false) ->
    Res = two_digits([Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Res);
encode({TimeType, Val}, AsBinary)
  when TimeType == 'date';
       TimeType == 'time' ->
    encode(Val, AsBinary);
encode({Time1, Time2, Time3}, false) ->
    Res = two_digits([Time1, Time2, Time3]),
    lists:flatten(Res);
encode(Val, _AsBinary) ->
    {error, {unrecognized_value, Val}}.

two_digits(Nums) when is_list(Nums) ->
    [two_digits(Num) || Num <- Nums];
two_digits(Num) ->
    Str = io_lib:format("~b", [Num]),
    case length(Str) of
    1 -> [$0 | Str];
    _ -> Str
    end.

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])]; %% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->      %% 39 is $'
    quote(Rest, [39, $\\ | Acc]);   %% 39 is $'
quote([34 | Rest], Acc) ->      %% 34 is $"
    quote(Rest, [34, $\\ | Acc]);   %% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

format(Keys,Values) ->
    Akeys = [binary_to_atom(K) || K <- Keys],
    Result = [lists:zip(Akeys,V) || V <- Values].

list_compare(LeftList, RightList) ->
    LeftSet = ordsets:from_list(LeftList),
    RightSet = ordsets:from_list(RightList),
    Inter = ordsets:intersection(LeftSet, RightSet),
    Left = ordsets:subtract(LeftSet, Inter),
    Right = ordsets:subtract(RightSet, Inter),
    {Left, Inter, Right}.

