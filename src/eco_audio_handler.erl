-module(eco_audio_handler).
-import(extbif, [to_list/1,to_binary/1]).
-export([main/0]).

main() ->
    Edition = [
                {audio_url,<<"">>}
                ],
    Url = binary_to_list(proplists:get_value(audio_url,Edition)),
    Res = prepare_audio(Url),
    <<"ok">>.
    
prepare_audio(Url) ->
    inets:start(),
    {ok, {{_, 200, _}, Headers, Body}} =
        httpc:request(get,
                      {Url, []},
                      [],
                      []),
    %UpdRes = [update_data(proplists:get_value(Rdn,List)) || Rdn <- UpdateList],
    ok.

%% database operation

prepare_dbdata(Year) ->
    Query = <<"SELECT * from eco_articles where edition_id = ?">>,
    Params = [Year],
    Dbdata = emysql:query(Query,Params).

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

