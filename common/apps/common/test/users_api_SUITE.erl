-module(users_api_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([user_decode_ok/1]).
-export([user_encode_ok/1]).

%%

-define(STRING, <<"TEST">>).

-type config()          :: [{atom(), term()}].
-type group_name()      :: atom().
-type test_case_name()  :: atom().
-type test_return()     :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [
        user_decode_ok,
        user_encode_ok
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() -> [].

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    C.

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    C.

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

-spec end_per_testcase(test_case_name(), config()) -> _.

end_per_testcase(_Name, _C) ->
    ok.

%%

-spec user_decode_ok(config()) -> test_return().
user_decode_ok(_C) ->
    ID = ?STRING,
    User = make_json_data(ID),
    UserData = json_proto:decode(User),
    UserObject = maps:get(<<"user">>, UserData),
    ID = maps:get(<<"id">>, UserObject).

-spec user_encode_ok(config()) -> test_return().
user_encode_ok(_C) ->
    ID = ?STRING,
    User = make_map_user(ID),
    UserData = json_proto:encode(User),
    ok = check_user_encode_data(ID, UserData).

test_case_name(Name, C) ->
    Key = '$test_case_name',
    Value = Name,
    lists:keystore(Key, 1, C, {Key, Value}).

make_json_data(ID) ->
    io_lib:format(<<"{
        \"user\":~s
    }">>, [make_json_user(ID)]).

check_user_encode_data(ID, Data) ->
    case Data =:= make_json_resp_data(ID) of
        true ->
            ok;
        _ ->
            Data = make_swap_json_resp_data(ID),
            ok
    end.

make_json_resp_data(ID) ->
    <<"{\"id\":\"", ID/binary, "\",\"lvl_ok\":\"TEST\"}">>.
make_swap_json_resp_data(ID) ->
    <<"{\"lvl_ok\":\"TEST\",\"id\":\"", ID/binary, "\"}">>.

make_json_user(ID) ->
    io_lib:format(<<"{
            \"id\"              : \"~s\",
            \"lvl_ok\"          : \"TEST\"
        }">>, [ID]).

make_map_user(ID) ->
    #{
        <<"id">>              => ID,
        <<"lvl_ok">>          => ?STRING
    }.
