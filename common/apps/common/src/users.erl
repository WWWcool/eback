-module(users).

-export([add/1]).
-export([read/1]).
-export([update/1]).
-export([delete/1]).

-type id()          :: binary().

-type user() :: #{
    id              := id(),
    lvl_ok          := binary(),
    all_ok          := binary(),
    hint_fstep      := binary(),
    hint_back       := binary(),
    live_count      := binary(),
    live_time       := binary(),
    price_time      := binary(),
    game_time       := binary(),
    game_points     := binary(),
    game_lvl_try    := binary(),
    sound           := binary(),
    music           := binary(),
    reserve_1       := binary(),
    reserve_2       := binary(),
    reserve_3       := binary(),
    reserve_4       := binary()
}.

-spec add(#{user := user()}) ->
    {ok, user()} | {error, binary()}.

add(#{user := User = #{<<"id">> := ID}}) ->
    unwrap_db_result(db:add_row(ID, User)).

-spec read(#{id := id()}) ->
    {ok, user()} | {error, binary()}.

read(#{id := ID}) ->
    unwrap_db_result(db:get_row(ID)).

-spec update(#{id := id(), user := user()}) ->
    {ok, user()} | {error, binary()}.

update(#{id := ID, user := User = #{<<"id">> := ID}}) ->
    unwrap_db_result(db:update_row(ID, User)).

-spec delete(#{id := id()}) ->
    ok | {error, binary()}.

delete(#{id := ID}) ->
    unwrap_db_result(db:delete_row(ID)).

unwrap_db_result(Common) ->
    case Common of
        {error, transaction_abort} ->
            {error, <<"transaction abort">>};
        {error, not_found} ->
            {error, <<"user not found">>};
        Result ->
            Result
    end.
