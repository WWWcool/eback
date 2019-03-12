-module(users_api).

-export([init/2]).
-export([process_req/2]).

-spec init(cowboy_req:req(), _) ->
    {ok, cowboy_req:req(), _}.

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Req = process_req(Method, Req0),
    {ok, Req, Opts}.

-spec process_req(binary(), cowboy_req:req()) ->
    cowboy_req:req().

process_req(<<"POST">>, Req) ->
    process_if_ok(
        [
            {user, fun try_get_user/3}
        ],
        fun users:add/1,
        #{},
        Req
    );
process_req(<<"GET">>, Req) ->
    process_if_ok(
        [
            {id, fun try_get_id/3}
        ],
        fun users:read/1,
        #{},
        Req
    );
process_req(<<"PUT">>, Req) ->
    process_if_ok(
        [
            {id, fun try_get_id/3},
            {user, fun try_get_user/3}
        ],
        fun users:update/1,
        #{},
        Req
    );
process_req(<<"DELETE">>, Req) ->
    process_if_ok(
        [
            {id, fun try_get_id/3}
        ],
        fun users:delete/1,
        #{},
        Req
    );
process_req(_Method, Req) ->
    cowboy_req:reply(400, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Invalid request :( ">>, Req).

%%

try_get_id(Key, Params, Req) ->
    case cowboy_req:binding(id, Req, undefined) of
        undefined ->
            {error, <<"Missing id.">>};
        ID ->
            {ok, Params#{Key => ID}}
    end.

try_get_user(Key, Params, Req) ->
    case cowboy_req:has_body(Req) of
        false ->
            {error, <<"Missing body.">>};
        _ ->
            {ok, BodyVals, _Req} = cowboy_req:read_urlencoded_body(Req),
            try_get_user_(Key, Params, BodyVals)
    end.

try_get_user_(Key, Params, ReqVals) ->
    case proplists:get_value(<<"user">>, ReqVals, undefined) of
        undefined ->
           {error, <<"Missing user data in body.">>};
        User ->
            Data = json_proto:decode(User),
            {ok, Params#{Key => Data}}
    end.

process_if_ok([], Fun, Params, Req) ->
    handle_process_result(Fun(Params), Req);
process_if_ok([{Key, CheckFun} | Rest], Fun, Params, Req) ->
    case CheckFun(Key, Params, Req) of
        {ok, NewParams} ->
            process_if_ok(Rest, Fun, NewParams, Req);
        {error, Reason} ->
            bad_request(Reason, Req)
    end.

handle_process_result(ok, Req) ->
    cowboy_req:reply(200, #{}, <<"">>, Req);
handle_process_result({ok, Result}, Req) ->
    cowboy_req:reply(200, #{}, json_proto:encode(Result), Req);
handle_process_result({error, Reason}, Req) ->
    cowboy_req:reply(400, #{}, Reason, Req).

bad_request(Reason, Req) ->
    cowboy_req:reply(400, #{}, Reason, Req).
