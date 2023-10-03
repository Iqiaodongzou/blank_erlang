-module(eco_article_handler).

-behavior(cowboy_handler).

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
    Editions = prepare_editions(list_to_binary(RawContent)),
    Articles = prepare_articles(lists:last(Editions)),
    R = jsx:encode(Articles).

prepare_articles(Edition) ->
    Url = binary_to_list(iolist_to_binary([<<"https://www.economist.com">>,Edition])),
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get,
                      {Url, []},
                      [], 
                      []),
    RawContent = list_to_binary(Body),
    Areas = prepare_areas(RawContent),
    Articles = [prepare_article(proplists:get_value(url,X)) || X <- Areas],
    Article = lists:last(Articles).

prepare_article(Url) ->
    Content = prepare_content(Url),
    Res = [
            {title,Url},
            {articles,Content}
            ].

prepare_content(PreUrl) ->
    Url = binary_to_list(iolist_to_binary([<<"https://www.economist.com">>,PreUrl])),
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get,
                      {Url, []},
                      [],
                      []),
    RawContent = list_to_binary(Body),
    Mp3 = prepare_source_match(RawContent,<<"https://www.economist.com/media-assets/[\\S]+mp3">>),
    Post = prepare_target_last(RawContent,1,<<",\"footer\"">>),
    PreContent = prepare_target_last(Post,0,<<"\"body\":">>),
    Flag = prepare_source_check(PreContent,<<"PARAGRAPH">>),
    case Flag of
        true ->
            MidContents = jsx:decode(PreContent,[{return_maps, false}]),
            Contents = iolist_to_binary([prepare_paragraph(X) 
                        || X <- MidContents,prepare_para_check(X)]),
            Res = [
                    {mp3,Mp3},
                    {content,Contents}
                    ];
        false ->
            Res = [ 
                    {mp3,null},
                    {content,null}
                    ]
    end.

prepare_paragraph(X) ->
    R = proplists:get_value(<<"text">>,X),
    R.
    
prepare_para_check(X) ->
    Type = proplists:get_value(<<"type">>,X,<<"unknown">>),
    R = (Type =:= <<"PARAGRAPH">>).

prepare_areas(RawContent) ->
    Lines = binary:split(RawContent,<<"\"">>,[global]),
    Ti = <<"/the-world-this-week/[0-9]+.*business|">>,
    T0 = <<"/the-world-this-week/[0-9]+.*politics|">>,
    T1 = <<"/leaders/[0-9]+.*|">>,
    T2 = <<"/briefing/[0-9]+.*|">>,
    T3 = <<"/china/[0-9]+.*|">>,
    T4 = <<"/united-states/[0-9]+.*|">>,
    T5 = <<"/middle-east-and-africa/[0-9]+.*|">>,
    T6 = <<"/the-americans/[0-9]+.*|">>,
    T7 = <<"/europe/[0-9]+.*|">>,
    T8 = <<"/britain/[0-9]+.*|">>,
    T9 = <<"/international/[0-9]+.*|">>,
    T10 = <<"/technology-quarterly/[0-9]+.*|">>,
    T11 = <<"/business/[0-9]+.*|">>,
    T12 = <<"/finance-economics/[0-9]+.*|">>,
    T13 = <<"/science-technology/[0-9]+.*|">>,
    T14 = <<"/culture/[0-9]+.*|">>,
    T15 = <<"/obituary/[0-9]+.*">>,
    Regx = iolist_to_binary([
             Ti,T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15
                                ]),
    PreLeaders = [prepare_source_match(X,Regx) || X <- Lines,prepare_source_check(X,Regx)],
    MidLeaders = lists:usort(PreLeaders),
    Leaders = [[{url,X}] || X <- MidLeaders],
    Leaders.

prepare_editions(Content) ->
    PreEditions = binary:split(Content,<<"edition-teaser">>,[global]),
    Blockditions = [X || X <- PreEditions, prepare_source_check(X,<<"printedition">>)],
    Editions = lists:usort([prepare_stdedition(X) || X <- Blockditions]),
    Editions.

prepare_stdedition(Block) ->
    PreLine = prepare_target_last(
                prepare_source_match(Block,<<"printedition[\\S]+">>),0,<<"\/">>), 
    Line = prepare_target_last(PreLine,1,<<"\"">>),
    R = iolist_to_binary([<<"/weeklyedition/">>,Line]).

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
