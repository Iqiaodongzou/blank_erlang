-module(eco_query_article).
-import(extbif, [to_list/1,to_binary/1]).
-behavior(cowboy_handler).
-export([init/2,main/1]).

init(Req0, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req2),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        main(Req3),
        Req3),
    {ok, Req, State}.

main(Req0) ->
    QsVals = cowboy_req:parse_qs(Req0),
    Link = proplists:get_value(<<"article_link">>,QsVals),
    Prefix = <<"https://www.economist.com/">>,
    inets:start(),
    Url = binary_to_list(iolist_to_binary([Prefix,Link])),
    io:format("show me link url ~p~n",[Url]),
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get,
                      {Url, []},
                      [],
                      []),
    RawContent = Body,
    Index = jsx:encode(prepare_index(list_to_binary(RawContent))),
    Index.

prepare_index(Source) ->
    SourceList = binary:split(Source,<<"{\"props">>,[global]),
    PreIndex = lists:last([X || X <- SourceList, prepare_source_check(X,<<"pageProps">>)]),
    Index = prepare_format_index(PreIndex),
    Context = jsx:decode(Index,[{return_maps, false}]),
    Context.

prepare_format_index(Source) ->
    PreIndex = iolist_to_binary([<<"{\"props">>,Source]),
    MidIndex = lists:nth(1,binary:split(PreIndex,<<"<">>,[global])),
    OriUrl = <<"https://www.economist.com/">>,
    TarUrl = <<"http://54.190.52.30:8081/query/article?article_link=">>,
    Index = iolist_to_binary(re:replace(MidIndex,OriUrl,TarUrl,[global])),
    OriImg = <<"https://www.economist.com/media-assets/">>,
    TarImg = <<"http://54\\.190\\.52\\.30:8081/query/article\\?article_link=media-assets/">>,
    PostIndex = iolist_to_binary(re:replace(Index,TarImg,OriImg,[global])).

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
    Query = <<"SELECT * from eco_weekly_edition where edition_id = ?">>,
    Params = [Year],
    Dbdata = emysql:query(Query,Params).

insert_data(X) ->
  case emysql:insert(eco_weekly_edition,[
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
    Url = proplists:get_value(url,X),
    case emysql:update(eco_weekly_edition,
      [{updated_at, {datetime, calendar:local_time()}} | X ],
      {'and',[{url,Url}]}) of
        {updated, {0, _}} ->
            io:format("cannot update edition data: ~p", [X]);
        {updated, {1, _}} ->
            ignore;
        {error, Reason} ->
            io:format("~p", [Reason]);
        Other ->
            io:format("update edition data failed!!:~p~n",[Other])
    end.

