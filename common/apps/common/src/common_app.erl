%%%-------------------------------------------------------------------
%% @doc common public API
%% @end
%%%-------------------------------------------------------------------

-module(common_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
-spec start(_, _) ->
    {ok, pid()}.

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[:id]", users_api, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    common_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(_) ->
    ok.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
