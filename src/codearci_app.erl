%%%-------------------------------------------------------------------
%% @doc codearci public API
%% @end
%%%-------------------------------------------------------------------

-module(codearci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", eco_query_index, []},
				{"/query",eco_query_index,[]},
				{"/query/index",eco_query_index,[]},
				{"/query/editions",eco_query_editions,[]},
				{"/query/edition",eco_query_edition,[]},
				{"/query/article",eco_query_article,[]}
                ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),
    codearci_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
