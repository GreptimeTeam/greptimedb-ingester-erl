-module(greptimedb_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [t_write, t_write_stream, t_collect_columns].

init_per_suite(Config) ->
    application:ensure_all_started(greptimedb),
    Config.

end_per_suite(_Config) ->
    application:stop(greptimedb).

points(N) ->
    lists:map(fun(Num) ->
                 #{fields => #{<<"temperature">> => 2},
                   tags =>
                       #{<<"from">> => <<"mqttx_4b963a8e">>,
                         <<"host">> => <<"serverB">>,
                         <<"qos">> => "1",
                         <<"region">> => <<"ningbo">>,
                         <<"to">> => <<"kafka">>},
                   timestamp => 1619775143098 + Num}
              end,
              lists:seq(1, N)).

t_collect_columns(_) ->
    Points =
        [#{fields => #{<<"temperature">> => 1},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverA">>,
                 <<"qos">> => "0",
                 <<"device">> => <<"NO.1">>,
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
    AuthInfo = {basic, #{username => "test", password => "test"}},
    Client = #{cli_opts => [{auth, AuthInfo}]},
    Request = greptimedb_encoder:insert_requests(Client, [{Metric, Points}]),
    case Request of
        #{header :=
              #{dbname := DbName,
                authorization := Auth},
          request := {inserts, #{inserts := [#{columns := Columns}]}}} ->
            ?assertEqual(DbName, "greptime-public"),
            ?assertEqual(8, length(Columns)),
            ?assertEqual(Auth, #{auth_scheme => AuthInfo}),

            {value, TemperatureColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"temperature">> end, Columns),
            ?assertEqual([1, 2], maps:get(f64_values, maps:get(values, TemperatureColumn))),

            {value, QosColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"qos">> end, Columns),
            ?assertEqual(["0", "1"], maps:get(string_values, maps:get(values, QosColumn))),

            {value, ToColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"to">> end, Columns),
            ?assertEqual([<<"kafka">>], maps:get(string_values, maps:get(values, ToColumn))),
            ?assertEqual(<<0:7/integer, 1:1/integer>>, maps:get(null_mask, ToColumn)),

            {value, DeviceColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"device">> end, Columns),
            ?assertEqual([<<"NO.1">>], maps:get(string_values, maps:get(values, DeviceColumn))),
            ?assertEqual(<<0:6/integer, 1:1/integer, 0:1/integer>>,
                         maps:get(null_mask, DeviceColumn)),

            {value, TimestampColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"greptime_timestamp">> end,
                             Columns),
            ?assertEqual([1619775142098, 1619775143098],
                         maps:get(ts_millisecond_values, maps:get(values, TimestampColumn)));
        _ ->
            ?assert(false)
    end,
    ok.

t_write(_) ->
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
         {pool, greptimedb_client_pool_1},
         {pool_size, 5},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),
    {ok, #{response := {affected_rows, #{value := 2}}}} =
        greptimedb:write(Client, Metric, Points),
    ok.

t_write_stream(_) ->
    Options =
        [{endpoints, [{http, "localhost", 4001}]},
         {pool, greptimedb_client_pool_3},
         {pool_size, 8},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),
    {ok, Stream} = greptimedb:write_stream(Client),

    Metric = <<"temperatures_stream">>,
    lists:foreach(fun(N) ->
                     Points = points(N),
                     ok = greptimedb_stream:write(Stream, Metric, Points)
                  end,
                  lists:seq(1, 10)),

    {ok, #{response := {affected_rows, #{value := 55}}}} = greptimedb_stream:finish(Stream),
    ok.
