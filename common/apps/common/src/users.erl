-module(users).

-export([add/1]).

-spec add(#{id := binary()}) ->
    {ok | error, binary()}.

add(#{id := ID}) ->
    {ok, ID}.
