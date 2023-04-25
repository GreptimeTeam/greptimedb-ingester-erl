-module(greptimedb_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [t_send, t_collect_columns].

init_per_suite(Config) ->
    application:ensure_all_started(greptimedb),
    Config.

end_per_suite(_Config) ->
    application:stop(greptimedb).

t_collect_columns(_) ->
    Points =
        [#{fields => #{<<"temperature">> => 1},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverA">>,
                 <<"qos">> => "0",
                 <<"region">> => <<"hangzhou">>},
           timestamp => 1619775142098},
         #{fields => #{<<"temperature">> => 2},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverB">>,
                 <<"qos">> => "1",
                 <<"region">> => <<"ningbo">>,
                 <<"to">> => <<"kafka">>},
           timestamp => 1619775143098}],
    Metric = "Test",
    Columns = greptimedb_encoder:insert_request(Metric, Points),
    ct:print("~w~n", [Columns]),
    ok.

t_send(_) ->
    Metric = <<"temperatures">>,
    Points =
        [#{fields => #{<<"temperature">> => 1},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverA">>,
                 <<"qos">> => greptimedb_values:int64_value(0),
                 <<"region">> => <<"hangzhou">>},
           timestamp => 1619775142098},
         #{fields => #{<<"temperature">> => 2},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverB">>,
                 <<"qos">> => greptimedb_values:int64_value(1),
                 <<"region">> => <<"ningbo">>,
                 <<"to">> => <<"kafka">>},
           timestamp => 1619775143098}],
    Options =
        [{endpoints, [{http, "localhost", 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 8},
         {pool_type, random}],

    {ok, Client} = greptimedb:start_client(Options),
    {ok, #{response := {affected_rows, #{value := 2}}}} =
        greptimedb:write(Client, Metric, Points),
    ok.
