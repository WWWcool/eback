-module(users_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([add_ok/1]).
-export([read_ok/1]).
-export([update_ok/1]).
-export([delete_ok/1]).

%%

-type config()          :: [{atom(), term()}].
-type group_name()      :: atom().
-type test_case_name()  :: atom().
-type test_return()     :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        add_ok,
        read_ok,
        update_ok,
        delete_ok
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() -> [].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    application:set_env(mnesia, dir, "./mnesia"),
    application:start(mnesia),
    application:start(db),
    ID = unique(),
    ok = check_id(ID),
    User = make_user(ID),
    test_case_name(init, [{test_user, User}, {test_id, ID} | C]).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    ID = ?config(test_id, C),
    ok = users:delete(#{id => ID}),
    ok = check_id(ID),
    application:stop(db),
    application:stop(mnesia).

%%

-spec init_per_group(group_name(), config()) -> config().

init_per_group(_, C) ->
    C.

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_, _) ->
    ok.

%%

-spec init_per_testcase(test_case_name(), config()) -> config().

init_per_testcase(Name, C) ->
    test_case_name(Name, C).
    % C.

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok.

%%

-spec add_ok(config()) -> test_return().
add_ok(C) ->
    User = ?config(test_user, C),
    Data = #{user => User},
    {ok, User} = users:add(Data),
    ok.

-spec read_ok(config()) -> test_return().
read_ok(C) ->
    Data = #{id => ?config(test_id, C)},
    User = ?config(test_user, C),
    {ok, User} = users:read(Data),
    ok.

-spec update_ok(config()) -> test_return().
update_ok(C) ->
    ID = ?config(test_id, C),
    User = ?config(test_user, C),
    NewUser = User#{<<"lvl_ok">> := <<"OK">>},
    Data = #{id => ID, user => NewUser},
    {ok, NewUser} = users:update(Data),
    ok.

-spec delete_ok(config()) -> test_return().
delete_ok(C) ->
    ok = users:delete(#{id => ?config(test_id, C)}).

-spec unique() -> binary().

unique() ->
    <<I:160/integer>> = crypto:hash(sha, term_to_binary({make_ref(), os:timestamp()})),
    format_int_base(I, 61).

-spec format_int_base(Integer :: integer(), Base :: integer()) -> binary().

format_int_base(I, Base) when is_integer(I), is_integer(Base), Base >= 2, Base =< 62 ->
    R = list_to_binary(format_int_base(abs(I), Base, [])),
    if
        I  > 0 -> R;
        I == 0 -> <<$0>>;
        I  < 0 -> <<$-, R/binary>>
    end;

format_int_base(I, Base) ->
    error(badarg, [I, Base]).

format_int_base(0, _Base, R0) ->
    R0;

format_int_base(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if
        D >= 36 -> [D - 36 + $a | R0];
        D >= 10 -> [D - 10 + $A | R0];
        true    -> [D + $0 | R0]
    end,
    format_int_base(I1, Base, R1).

test_case_name(Name, C) ->
    Key = '$test_case_name',
    Value = Name,
    lists:keystore(Key, 1, C, {Key, Value}).

check_id(ID) ->
    case users:read(#{id => ID}) of
        {ok, _} ->
            {error, already_exist};
        {error, <<"transaction abort">>} ->
            {error, db_fail};
        {error, <<"user not found">>} ->
            ok
    end.

-define(STRING, <<"TEST">>).

make_user(ID) ->
    #{
        <<"id">>              => ID,
        <<"lvl_ok">>          => ?STRING,
        <<"all_ok">>          => ?STRING,
        <<"hint_fstep">>      => ?STRING,
        <<"hint_back">>       => ?STRING,
        <<"live_count">>      => ?STRING,
        <<"live_time">>       => ?STRING,
        <<"price_time">>      => ?STRING,
        <<"game_time">>       => ?STRING,
        <<"game_points">>     => ?STRING,
        <<"game_lvl_try">>    => ?STRING,
        <<"sound">>           => ?STRING,
        <<"music">>           => ?STRING,
        <<"reserve_1">>       => ?STRING,
        <<"reserve_2">>       => ?STRING,
        <<"reserve_3">>       => ?STRING,
        <<"reserve_4">>       => ?STRING
    }.
