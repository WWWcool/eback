%%%-------------------------------------------------------------------
%% @doc json_proto public API
%% @end
%%%-------------------------------------------------------------------

-module(json_proto_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
-spec start(_, _) ->
    {ok, pid()}.

start(_StartType, _StartArgs) ->
    json_proto_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(_) ->
    ok.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================