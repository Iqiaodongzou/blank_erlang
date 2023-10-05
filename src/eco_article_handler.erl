-module(eco_article_handler).
-import(extbif, [to_list/1,to_binary/1]).
-export([main/1]).

main(Edition) ->
    inets:start(),
    Url = binary_to_list(proplists:get_value(url,Edition)),
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get,
                      {Url, []},
                      [],
                      []),
    RawContent = Body,
    EditionId = proplists:get_value(edition_id,Edition),
    DbData = prepare_dbdata(EditionId),
    Editions = prepare_article(list_to_binary(RawContent),Edition),
    List=[{to_binary(prepare_rdn(R)), R} || R <- Editions],
    DbList=[{to_binary(prepare_rdn(R)), R} || R <- DbData],
    {AddList, UpdateList, DelList} = list_compare(get_key(List), get_key(DbList)),
    InsRes = [insert_data(proplists:get_value(Rdn,List)) || Rdn <- AddList],
    UpdRes = [update_data(proplists:get_value(Rdn,List)) || Rdn <- UpdateList],
    <<"ok">>.

prepare_rdn(R) ->
    Res = proplists:get_value(url,R,20230101).

prepare_article(Source,Agent) ->
    Editions = prepare_source_list(Source),
    Context = jsx:decode(Editions,[{return_maps, false}]),
    EcoArticle = [prepare_format_article(Context,Agent,Source)].

prepare_source_list(Source) ->
    MidList = binary:split(Source,<<"\n">>,[global]),
    MidArticle = lists:last([X || X <- MidList,prepare_source_check(X,<<"mainEntityOfPage">>)]),
    Res = MidArticle.

prepare_format_article(Edition,Agent,Source) ->
    Item = proplists:get_value(<<"url">>,Edition,null),
    case Item =:= null of
        false ->
            EditionId = proplists:get_value(edition_id,Agent),
            Url = proplists:get_value(<<"url">>,Edition),
            AudioUrl = prepare_audio(Source),
            Headline = proplists:get_value(<<"headline">>,Edition),
            ArticleBody = proplists:get_value(<<"articleBody">>,Edition),
            Image = proplists:get_value(<<"image">>,Edition),
            CopyrightYear = proplists:get_value(<<"copyrightYear">>,Edition),
            Res = [
                    {edition_id,EditionId},
                    {url,Url},
                    {audio_url,AudioUrl},
                    {headline,Headline},
                    {articleBody,ArticleBody},
                    {image,Image},
                    {copyrightYear,CopyrightYear}
                    ];
        true ->
            Res = []
    end.
            
prepare_audio(Source) ->
    Mp3 = prepare_source_match(Source,<<"https://www.economist.com/media-assets/[\\S]+mp3">>),
    Mp3.

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
    Query = <<"SELECT * from eco_articles where edition_id = ?">>,
    Params = [Year],
    Dbdata = emysql:query(Query,Params).

insert_data(X) ->
  case emysql:insert(eco_articles,[
    {created_at, {datetime, calendar:local_time()}} | X
  ]) of
    {updated, {0, _}} ->
      io:format("cannot insert article: ~p", [X]);
    {updated, {1, _}} ->
      ignore;
    {error, Reason} ->
      io:format("~p", [Reason]);
    Other ->
      io:format("insert article failed!!:~p~n",[Other])
  end.

update_data(X) ->
    Url = proplists:get_value(url,X),
    case emysql:update(eco_articles,
      [{updated_at, {datetime, calendar:local_time()}} | X ],
      {'and',[{url,Url}]}) of
        {updated, {0, _}} ->
            io:format("cannot update article data: ~p", [X]);
        {updated, {1, _}} ->
            ignore;
        {error, Reason} ->
            io:format("~p", [Reason]);
        Other ->
            io:format("update article data failed!!:~p~n",[Other])
    end.

