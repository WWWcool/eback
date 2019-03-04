-module(db_SUITE).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([read_write_ok/1]).
-export([read_write_ok_1/1]).
-export([read_write_ok_2/1]).
-export([read_write_ok_3/1]).

%%

-type config()          :: [{atom(), term()}].
-type group_name()      :: atom().
-type test_case_name()  :: atom().
-type test_return()     :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        read_write_ok,
        read_write_ok_1,
        read_write_ok_2,
        read_write_ok_3
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() -> [].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    application:set_env(mnesia, dir, "./mnesia"),
    application:start(mnesia),
    db:init_mnesia([node()]),
    % application:start(mnesia),
    % application:start(db),
    test_case_name(init, C).
    % C.

-spec end_per_suite(config()) -> _.

end_per_suite(_C) ->
    % application:stop(db),
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

-spec read_write_ok(config()) -> test_return().
read_write_ok(_C) ->
    ID = unique(),
    Data = #{id => ID},
    ok = db:add_row(ID, Data),
    [{data_row, ID, Data}] = db:get_row(ID),
    ok.

-spec read_write_ok_1(config()) -> test_return().
read_write_ok_1(_C) ->
    ID = unique(),
    Data = #{id => ID},
    ok = db:add_row(ID, Data),
    [{data_row, ID, Data}] = db:get_row(ID),
    ok.

-spec read_write_ok_2(config()) -> test_return().
read_write_ok_2(_C) ->
    ID = unique(),
    Data = #{id => ID},
    ok = db:add_row(ID, Data),
    [{data_row, ID, Data}] = db:get_row(ID),
    ok.

-spec read_write_ok_3(config()) -> test_return().
read_write_ok_3(_C) ->
    ID = unique(),
    Data = #{id => ID},
    ok = db:add_row(ID, Data),
    [{data_row, ID, Data}] = db:get_row(ID),
    ok.

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
