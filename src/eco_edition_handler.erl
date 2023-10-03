-module(eco_edition_handler).
-behavior(cowboy_handler).
-import(extbif, [to_list/1,to_binary/1]).
-export([init/2,prepare_json/0]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        prepare_json(),
        Req0),
    {ok, Req, State}.

prepare_json() ->
    inets:start(),
    Url = "https://www.economist.com/weeklyedition/archive",
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get,
                      {Url, []},
                      [],
                      []),
    RawContent = Body,
    {{Year,_,_},_} = calendar:local_time(),
    DbData = prepare_dbdata(Year),
    Editions = prepare_editions(list_to_binary(RawContent)),
    List=[{to_binary(prepare_rdn(R)), R} || R <- Editions],
    DbList=[{to_binary(prepare_rdn(R)), R} || R <- DbData],
    {AddList, UpdateList, DelList} = list_compare(get_key(List), get_key(DbList)),
    InsRes = [insert_data(proplists:get_value(Rdn,List)) || Rdn <- AddList],
    UpdRes = [update_data(proplists:get_value(Rdn,List)) || Rdn <- UpdateList],
    io:format("we got data ~p~n",[AddList]),
    <<"ok">>.

prepare_rdn(R) ->
    Res = proplists:get_value(edition_id,R,20230101).

prepare_editions(Source) ->
    SourceList = binary:split(Source,<<"\n">>,[global]),
    Editions = lists:last([X || X <- SourceList, prepare_source_check(X,<<"itemListElement">>)]),
    Context = jsx:decode(Editions,[{return_maps, false}]),
    EditionList = proplists:get_value(<<"itemListElement">>,Context),
    EcoEditions = [prepare_format_edition(X) || X <- EditionList].

prepare_format_edition(Edition) ->
    Url = proplists:get_value(<<"url">>,Edition),
    Position = proplists:get_value(<<"position">>,Edition),
    Date = prepare_target_last(prepare_source_match(Url,<<"[0-9]+.*">>),0),
    Year = prepare_target_last(Date,1,<<"-">>),
    Month = prepare_target_last(Date,2,<<"-">>),
    Day = prepare_target_last(Date,0,<<"-">>),
    EditionId = num(iolist_to_binary([Year,Month,Day])),
    Res = [
            {year,num(Year)},
            {edition_id,EditionId},
            {url,Url},
            {position,Position}
            ].

prepare_source_check(Line,Regx) ->
    case re:run(Line, Regx, [{capture, all, binary}]) of
        {match,_} -> G = true;
        _ -> G = false
    end,
    G.

trip(X) ->
    X1 = prepare_target_last(X,0,<<"\[">>),
    X2 = prepare_target_last(X1,1,<<"\r">>),
    X3 = prepare_target_last(X2,1,<<"dBm\]">>),
    X4 = prepare_target_last(X3,1,<<"\"">>),
    X5 = prepare_target_last(X4,1,<<"\]">>),
    X6 = prepare_target_last(X5,1,<<"nm">>),
    X7 = prepare_target_last(X6,1,<<"\n">>),
    X8 = prepare_target_last(X7,1,<<"%">>),
    X9 = prepare_target_last(X8,1,<<"\(">>),
    X9.

prepare_target_last(Line,Posi) ->
    Regx = <<" ">>,
    prepare_target_last(Line,Posi,Regx).

prepare_target_last(Line,Posi,Regx) ->
    ListSource = binary:split(Line,Regx,[global]),
    ListTar = [X || X <- ListSource, length(binary_to_list(X)) > 0],
    case Posi of
        0 -> Res = lists:last(ListTar);
        _ -> Res = lists:nth(Posi,ListTar)
    end,
    Res.

prepare_source_match(Line,Regx) ->
    case re:run(Line, Regx, [{capture, all, binary}]) of
        {match,V} -> G = lists:nth(1,V);
        _ -> G = <<"null">>
    end.

num(Source) ->
    Result = lists:nth(1,binary:split(Source,<<"%">>,[global])),
    case prepare_source_check(Result,<<"\\.">>) of
        true -> Res = list_to_float(binary_to_list(Result));
        false -> Res = list_to_integer(binary_to_list(Result))
    end,
    Res.

list_compare(LeftList, RightList) ->
    LeftSet = ordsets:from_list(LeftList),
    RightSet = ordsets:from_list(RightList),
    Inter = ordsets:intersection(LeftSet, RightSet),
    Left = ordsets:subtract(LeftSet, Inter),
    Right = ordsets:subtract(RightSet, Inter),
    {Left, Inter, Right}.

get_key(List) ->
    [Key || {Key, _} <- List].

%% database operation

prepare_dbdata(Year) ->
    Query = <<"SELECT * from eco_editions where year = ?">>,
    Params = [Year],
    Dbdata = emysql:query(Query,Params).

insert_data(X) ->
  case emysql:insert(eco_editions,[
    {created_at, {datetime, calendar:local_time()}} | X
  ]) of
    {updated, {0, _}} ->
      io:format("cannot insert edition: ~p", [X]);
    {updated, {1, _}} ->
      ignore;
    {error, Reason} ->
      io:format("~p", [Reason]);
    Other ->
      io:format("insert edition failed!!:~p~n",[Other])
  end.

update_data(X) ->
    EditionId = proplists:get_value(edition_id,X),
    case emysql:update(eco_editions,
      [{updated_at, {datetime, calendar:local_time()}} | X ],
      {'and',[{edition_id,EditionId}]}) of
        {updated, {0, _}} ->
            io:format("cannot update edition data: ~p", [X]);
        {updated, {1, _}} ->
            ignore;
        {error, Reason} ->
            io:format("~p", [Reason]);
        Other ->
            io:format("update edition data failed!!:~p~n",[Other])
    end.

