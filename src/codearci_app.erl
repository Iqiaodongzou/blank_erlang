%%%-------------------------------------------------------------------
%% @doc codearci public API
%% @end
%%%-------------------------------------------------------------------

-module(codearci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", code_handler, []},
				{"/query",sql_handler,[]},
				%{"/weeklyedition/archive",eco_edition_handler,[]}
				{"/weeklyedition/archive",eco_edition_handler,[]}
                ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),
    % lager:error("cowboy is already started ~p~s", [_Args]),
    codearci_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
