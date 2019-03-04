-module(db).

-export([init_mnesia/1]).
-export([add_row/2]).
-export([get_row/1]).

-record(data_row, {
    id              :: id(),
    data            :: data()
}).

-type id()          :: binary().
-type data()        :: #{
    id := id()
}.
-type row()         :: #data_row{}.

-export_type([data/0]).
-export_type([row/0]).

-spec init_mnesia(list(atom())) ->
    ok.

init_mnesia(Nodes) ->
    io:format("db | stop mnesia on nodes...~p~n", [Nodes]),
    {[ok], []} = rpc:multicall(Nodes, application, stop, [mnesia]),
    % io:format("db | deleting schemas on nodes...~p~n", [Nodes]),
    % ok = mnesia:delete_schema(Nodes),
    Result = mnesia:create_schema(Nodes),
    io:format("db | creating mnesia schema in nodes - ~p with res - ~p~n", [Nodes, Result]),
    io:format("db | mnesia schema in dir (~p:~p) ~n", [mnesia:system_info(directory), application:get_env(mnesia, dir)]),
    application:start(mnesia),
    CreateTableResult = mnesia:create_table(data_row,
        [{attributes, record_info(fields, data_row)},
        {disc_copies, Nodes}]),
    io:format("db | created table with - ~p~n", [CreateTableResult]),
    io:format("db | wait for table...~n", []),
    WaitResult = case mnesia:wait_for_tables([data_row], 5000) of
        {timeout, _} ->
            {force_load, mnesia:force_load_table(data_row)};
        R ->
            R
    end,
    io:format("db | wait for table end with - ~p~n", [WaitResult]),
    io:format("db | storage type schema ~p~n", [mnesia:table_info(schema, storage_type)]),
    io:format("db | storage type data_row ~p~n", [mnesia:table_info(data_row, storage_type)]),
    ChangeResultSchema = mnesia:change_table_copy_type(schema, node(), disc_copies),
    io:format("db | change storage type of schema ~p with - ~p~n", [mnesia:table_info(schema, storage_type), ChangeResultSchema]),
    ChangeResult = mnesia:change_table_copy_type(data_row, node(), disc_copies),
    io:format("db | change storage type of data_row ~p with - ~p~n", [mnesia:table_info(data_row, storage_type), ChangeResult]),

    mnesia:info().
    % application:stop(mnesia),
    % io:format("db | test end...~n").

-spec add_row(id(), map()) ->
    transaction_abort | ok.
add_row(ID, Data) ->
    F = fun() ->
        mnesia:write(#data_row{id = ID, data = Data})
    end,
    mnesia:activity(transaction, F).

-spec get_row(map()) ->
    transaction_abort | list(row()).
get_row(ID) ->
    F = fun() ->
        mnesia:read({data_row, ID})
    end,
    mnesia:activity(transaction, F).
