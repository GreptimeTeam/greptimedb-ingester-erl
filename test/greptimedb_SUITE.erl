-module(greptimedb_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [t_write,
     t_write_stream,
     t_write_failure,
     t_write_batch,
     t_bench_perf,
     t_write_stream,
     t_async_write_batch,
     t_insert_greptime_cloud,
     t_auth_error,
     t_insert_requests,
     t_insert_requests_with_timeunit,
     t_insert_requests_empty_points,
     t_insert_requests_single_point,
     t_insert_requests_all_sparse,
     t_insert_requests_all_data_types,
     t_insert_requests_all_time_units,
     t_insert_requests_metric_formats].

%%[t_bench_perf].
%%[t_insert_requests, t_bench_perf].

init_per_suite(Config) ->
    application:ensure_all_started(greptimedb),
    application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    application:stop(greptimedb).

init_per_testcase(t_insert_greptime_cloud, Config) ->
    Host = os:getenv("GT_TEST_HOST"),
    DbName = os:getenv("GT_TEST_DB"),
    UserName = os:getenv("GT_TEST_USER"),
    PassWd = os:getenv("GT_TEST_PASSWD"),
    UndefinedVars =
        lists:filter(fun(X) -> not is_list(X) orelse X == "" end,
                     [Host, DbName, UserName, PassWd]),
    case UndefinedVars of
        [] ->
            Config;
        _ ->
            {skip, cloud_env_vars_undefined}
    end;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

greptime_host() ->
    os:getenv("GT_HOST", "localhost").

points(N) ->
    lists:map(fun(Num) ->
                 #{fields => #{<<"temperature">> => Num},
                   tags =>
                       #{<<"from">> => <<"mqttx_4b963a8e">>,
                         <<"host">> => <<"serverB">>,
                         <<"qos">> => "1",
                         <<"region">> => <<"ningbo">>,
                         <<"to">> => <<"kafka">>},
                   timestamp => 1619775143098 + Num}
              end,
              lists:seq(1, N)).

t_insert_requests(_) ->
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
           timestamp => 1619775143098},
         #{fields => #{<<"temperature">> => 3},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverB">>,
                 <<"qos">> => "2",
                 <<"region">> => <<"xiamen">>,
                 <<"to">> => <<"kafka">>},
           timestamp => 1619775144098}],

    Metric = "Test",
    AuthInfo = {basic, #{username => "test", password => "test"}},
    Client = #{cli_opts => [{auth, AuthInfo}, {timeunit, second}]},
    Request = greptimedb_encoder:insert_requests(Client, [{Metric, Points}]),
    case Request of
        #{header := #{dbname := DbName, authorization := Auth},
          request := {inserts, #{inserts := [#{columns := Columns}]}}} ->
            ?assertEqual(DbName, "greptime-public"),
            ?assertEqual(8, length(Columns)),
            ?assertEqual(Auth, #{auth_scheme => AuthInfo}),

            {value, TemperatureColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"temperature">> end, Columns),
            ?assertEqual([1, 2, 3], maps:get(f64_values, maps:get(values, TemperatureColumn))),

            {value, QosColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"qos">> end, Columns),
            ?assertEqual(["0", "1", "2"], maps:get(string_values, maps:get(values, QosColumn))),

            {value, ToColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"to">> end, Columns),
            ?assertEqual([<<"kafka">>, <<"kafka">>],
                         maps:get(string_values, maps:get(values, ToColumn))),
            ?assertEqual(<<0:6/integer, 1:1/integer, 1:1/integer>>, maps:get(null_mask, ToColumn)),

            {value, DeviceColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"device">> end, Columns),
            ?assertEqual([<<"NO.1">>], maps:get(string_values, maps:get(values, DeviceColumn))),
            ?assertEqual(<<0:5/integer, 1:1/integer, 0:1/integer, 0:1/integer>>,
                         maps:get(null_mask, DeviceColumn)),

            {value, TimestampColumn} =
                lists:search(fun(C) -> maps:get(column_name, C) == <<"greptime_timestamp">> end,
                             Columns),
            ?assertEqual([1619775142098, 1619775143098, 1619775144098],
                         maps:get(timestamp_second_values, maps:get(values, TimestampColumn)));
        _ ->
            ?assert(false)
    end,
    ok.

t_insert_requests_with_timeunit(_) ->
    TsNano = 1705946037724448346,
    Points =
        [#{fields => #{<<"temperature">> => 1},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverA">>,
                 <<"qos">> => "0",
                 <<"device">> => <<"NO.1">>,
                 <<"region">> => <<"hangzhou">>},
           timestamp => TsNano}],
    AuthInfo = {basic, #{username => "test", password => "test"}},
    Client = #{cli_opts => [{auth, AuthInfo}, {timeunit, second}]},
    Metric = #{table => "Test", timeunit => nanosecond},
    Request = greptimedb_encoder:insert_requests(Client, [{Metric, Points}]),
    #{header := #{dbname := _DbName, authorization := _Auth},
      request := {inserts, #{inserts := [#{columns := Columns}]}}} =
        Request,
    {value, TimestampColumn} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"greptime_timestamp">> end, Columns),
    ?assertEqual([TsNano],
                 maps:get(timestamp_nanosecond_values, maps:get(values, TimestampColumn))).

t_write_failure(_) ->
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
        %% the port 5001 is invalid
        [{endpoints, [{http, greptime_host(), 5001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 5},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    false = greptimedb:is_alive(Client),
    {error, _} = greptimedb:write(Client, Metric, Points),

    %% async write
    Ref = make_ref(),
    TestPid = self(),
    ResultCallback = {fun(Reply) -> TestPid ! {{Ref, reply}, Reply} end, []},

    ok = greptimedb:async_write(Client, Metric, Points, ResultCallback),
    receive
        {{Ref, reply}, {error, Error}} ->
            ct:print("write failure: ~p", [Error]);
        {{Ref, reply}, _Other} ->
            ?assert(false)
    end,

    greptimedb:stop_client(Client),
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
    Host = greptime_host(),
    Options =
        [{endpoints, [{http, Host, 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 5},
         %% enable append mode and ttl
         {grpc_hints, #{<<"append_mode">> => <<"true">>, <<"ttl">> => <<"7 days">>}},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),
    {ok, #{response := {affected_rows, #{value := 2}}}} =
        greptimedb:write(Client, Metric, Points),

    %% assert table options
    Sql = "show create table temperatures",
    EncodedSql = uri_string:quote(Sql),
    URL = "http://" ++ Host ++ ":4000/v1/sql?sql=" ++ EncodedSql,
    User = <<"greptime_user">>,
    Pass = <<"greptime_pwd">>,
    AuthBin = base64:encode(<<User/binary, ":", Pass/binary>>),
    AuthHeader = {"authorization", <<"Basic ", AuthBin/binary>>},

    {ok, {{_, 200, _}, _, RespBody}} = httpc:request(get, {URL, [AuthHeader]}, [], []),

    ?assert(string:find(RespBody, "ttl = '7days'") =/= nomatch),
    ?assert(string:find(RespBody, "append_mode = 'true'") =/= nomatch),

    greptimedb:stop_client(Client),
    ok.

t_auth_error(_) ->
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
        [{endpoints, [{http, greptime_host(), 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 5},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"wrong_pwd">>}}}],
    {ok, Client} = greptimedb:start_client(Options),

    %% sync write
    {error, {unauth, _, _}} = greptimedb:write(Client, Metric, Points),
    %% async write
    Ref = make_ref(),
    TestPid = self(),
    ResultCallback = {fun(Reply) -> TestPid ! {{Ref, reply}, Reply} end, []},

    ok = greptimedb:async_write(Client, Metric, Points, ResultCallback),
    receive
        {{Ref, reply}, {error, {unauth, _, _}}} ->
            ok;
        {{Ref, reply}, _Other} ->
            ?assert(false)
    end,

    greptimedb:stop_client(Client),
    ok.

t_write_stream(_) ->
    Options =
        [{endpoints, [{http, greptime_host(), 4001}]},
         {pool, greptimedb_client_pool},
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
    greptimedb:stop_client(Client),
    ok.

t_write_batch(_) ->
    Options =
        [{endpoints, [{http, greptime_host(), 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 8},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),

    Metric = <<"temperatures_">>,
    MetricAndPoints =
        lists:map(fun(N) ->
                     Points = points(N),
                     {erlang:iolist_to_binary([Metric, integer_to_binary(N)]), Points}
                  end,
                  lists:seq(1, 10)),

    {ok, #{response := {affected_rows, #{value := 55}}}} =
        greptimedb:write_batch(Client, MetricAndPoints),
    greptimedb:stop_client(Client),
    ok.

rand_string(Bytes) ->
    base64:encode(
        crypto:strong_rand_bytes(Bytes)).

bench_points(StartTs, N, Tags) ->
    lists:map(fun(Num) ->
                 #{fields =>
                       #{<<"f0">> => Num,
                         <<"f1">> => Num,
                         <<"f2">> => Num,
                         <<"f3">> => Num,
                         <<"f4">> => Num,
                         <<"f5">> => Num,
                         <<"f6">> => Num,
                         <<"f7">> => Num,
                         <<"f8">> => Num,
                         <<"f9">> => rand:uniform(Num)},
                   tags =>
                       #{<<"tag0">> => <<"tagv0">>,
                         <<"tag1">> => <<"tagv1">>,
                         <<"tag2">> => <<"tagv2">>,
                         <<"tag3">> => <<"tagv3">>,
                         <<"tag4">> => <<"tagv4">>,
                         <<"tag5">> => <<"tagv5">>,
                         <<"tag6">> => <<"tagv6">>,
                         <<"tag7">> => <<"tagv7">>,
                         <<"tag8">> => <<"tagv8">>,
                         <<"tag9">> => element(rand:uniform(tuple_size(Tags)), Tags)},
                   timestamp => StartTs + Num}
              end,
              lists:seq(1, N)).

bench_write(N, StartMs, BatchSize, Client, BenchmarkEncoding, Tags) ->
    bench_write(N, StartMs, BatchSize, Client, BenchmarkEncoding, Tags, 0).

bench_write(0, _StartMs, _BatchSize, _Client, _BenchmarkEncoding, _Tags, Written) ->
    Written;
bench_write(N, StartMs, BatchSize, Client, BenchmarkEncoding, Tags, Written) ->
    Rows =
        case BenchmarkEncoding of
            true ->
                Metric = <<"bench_metrics">>,
                Points = bench_points(StartMs + N, BatchSize, Tags),
                _Request = greptimedb_encoder:insert_requests(Client, [{Metric, Points}]),
                length(Points);
            false ->
                {ok, #{response := {affected_rows, #{value := AffectedRows}}}} =
                    greptimedb:write(Client,
                                     <<"bench_metrics">>,
                                     bench_points(StartMs + N, BatchSize, Tags)),
                AffectedRows
        end,

    NewWritten = Written + Rows,
    bench_write(N - 1, StartMs, BatchSize, Client, BenchmarkEncoding, Tags, NewWritten).

join([P | Ps]) ->
    receive
        {P, Result} ->
            [Result | join(Ps)]
    end;
join([]) ->
    [].

t_bench_perf(_) ->
    Options =
        [{endpoints, [{http, greptime_host(), 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 8},
         {pool_type, random},
         {timeunit, ms},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),
    BatchSize = 100,
    Num = 1000,
    Series = 1000,
    Profile = false,
    BenchmarkEncoding = false,
    Concurrency = 3,
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    StartMs0 = (MegaSecs * 1000000 + Secs) * 1000,

    Tags = list_to_tuple(lists:map(fun(_N) -> rand_string(8) end, lists:seq(1, Series))),
    %% warmup
    bench_write(1000, StartMs0, BatchSize, Client, BenchmarkEncoding, Tags),
    ct:print("Warmed up, start to benchmark writing..."),
    %% benchmark
    T1 = erlang:monotonic_time(),
    Rows =
        case Profile of
            true ->
                ct:print("Enable eprof..."),
                eprof:start(),
                eprof:log("/tmp/eprof.result"),
                {ok, Ret} =
                    eprof:profile(fun() ->
                                     bench_write(Num,
                                                 StartMs0,
                                                 BatchSize,
                                                 Client,
                                                 BenchmarkEncoding,
                                                 Tags)
                                  end),
                eprof:analyze(),
                eprof:stop(),
                Ret;
            false ->
                Parent = self(),
                Pids =
                    lists:map(fun(C) ->
                                 spawn(fun() ->
                                          StartMs1 = StartMs0 - C * Num,
                                          Written =
                                              bench_write(Num,
                                                          StartMs1,
                                                          BatchSize,
                                                          Client,
                                                          BenchmarkEncoding,
                                                          Tags),
                                          Parent ! {self(), Written}
                                       end)
                              end,
                              lists:seq(1, Concurrency)),
                lists:sum(join(Pids))
        end,

    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, seconds),
    TPS = Rows / Time,
    %% print the result
    ct:print("Finish benchmark, series: ~p, concurrency: ~p, cost: ~p seconds, rows: ~p, TPS: ~p~n",
             [Series, Concurrency, Time, Rows, TPS]),
    greptimedb:stop_client(Client),
    ok.

async_write(Client, StartMs) ->
    Ref = make_ref(),
    TestPid = self(),
    ResultCallback = {fun(Reply) -> TestPid ! {{Ref, reply}, Reply} end, []},

    Tags = list_to_tuple(lists:map(fun(_N) -> rand_string(8) end, lists:seq(1, 100))),
    Metric = <<"async_metrics">>,
    Points = bench_points(StartMs, 10, Tags),

    ok = greptimedb:async_write_batch(Client, [{Metric, Points}], ResultCallback),

    Ref.

recv(Ref) ->
    receive
        {{Ref, reply}, Reply} ->
            ct:print("Reply ~w~n", [Reply])
    end.

t_async_write_batch(_) ->
    Options =
        [{endpoints, [{http, greptime_host(), 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 8},
         {pool_type, random},
         {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),

    StartMs = 1690874475279,
    %% Write once
    Ref = async_write(Client, StartMs + 100000),
    recv(Ref),

    %% Write batches
    N = 100,
    Refs =
        lists:map(fun(Num) -> async_write(Client, StartMs + Num * 10) end, lists:seq(1, N)),
    lists:foreach(fun(Ref0) -> recv(Ref0) end, Refs),

    greptimedb:stop_client(Client),
    ok.

t_insert_greptime_cloud(_) ->
    Host = os:getenv("GT_TEST_HOST"),
    DbName = os:getenv("GT_TEST_DB"),
    UserName = os:getenv("GT_TEST_USER"),
    PassWd = os:getenv("GT_TEST_PASSWD"),

    if (Host == false) or (DbName == false) or (UserName == false) or (PassWd == false) ->
           ct:print("Ignored t_insert_greptime_cloud..."),
           ok;
       true ->
           ct:print("Running t_insert_greptime_cloud..."),
           %% the endpoint scheme must be `https`.
           Options =
               [{endpoints, [{https, Host, 5001}]},
                {pool, greptimedb_client_pool},
                {pool_size, 5},
                {pool_type, random},
                {timeunit, ms},
                {dbname, DbName},
                {auth, {basic, #{username => UserName, password => PassWd}}}],
           {ok, Client} = greptimedb:start_client(Options),
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
           {ok, #{response := {affected_rows, #{value := 2}}}} =
               greptimedb:write(Client, Metric, Points),
           greptimedb:stop_client(Client),
           ok
    end.

%% ======================================================================
%% New encoder-specific tests
%% ======================================================================

t_insert_requests_empty_points(_) ->
    Client = #{cli_opts => [{timeunit, ms}]},
    Request = greptimedb_encoder:insert_requests(Client, []),
    #{header := #{dbname := unknown}, request := {inserts, #{inserts := []}}} = Request.

t_insert_requests_single_point(_) ->
    Point =
        #{fields => #{<<"temp">> => 25.5},
          tags => #{<<"sensor">> => <<"room1">>},
          timestamp => 1619775142098},
    Client = #{cli_opts => [{timeunit, ms}]},
    Request = greptimedb_encoder:insert_requests(Client, [{"sensors", [Point]}]),

    #{header := #{dbname := "greptime-public"},
      request :=
          {inserts,
           #{inserts :=
                 [#{table_name := "sensors",
                    columns := Columns,
                    row_count := 1}]}}} =
        Request,

    ?assertEqual(3, length(Columns)), % temp, sensor, timestamp

    {value, TempColumn} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"temp">> end, Columns),
    ?assertEqual([25.5], maps:get(f64_values, maps:get(values, TempColumn))),
    ?assertNot(maps:is_key(null_mask, TempColumn)).

t_insert_requests_all_sparse(_) ->
    Points =
        [% Point 1: only field1 and tag1
         #{fields => #{<<"field1">> => 1.0},
           tags => #{<<"tag1">> => <<"value1">>},
           timestamp => 1000000000},
         % Point 2: only field2 and tag2
         #{fields => #{<<"field2">> => 2.0},
           tags => #{<<"tag2">> => <<"value2">>},
           timestamp => 1000000001},
         % Point 3: only field3 and tag3
         #{fields => #{<<"field3">> => 3.0},
           tags => #{<<"tag3">> => <<"value3">>},
           timestamp => 1000000002}],

    Client = #{cli_opts => [{timeunit, s}]},
    Request = greptimedb_encoder:insert_requests(Client, [{"sparse_test", Points}]),

    #{request := {inserts, #{inserts := [#{columns := Columns, row_count := 3}]}}} = Request,

    ?assertEqual(7, length(Columns)), % 3 fields + 3 tags + timestamp

    % Check field1: only present in row 1
    {value, Field1} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"field1">> end, Columns),
    ?assertEqual([1.0], maps:get(f64_values, maps:get(values, Field1))),
    ?assertEqual(<<0:5, 1:1, 0:1, 0:1>>, maps:get(null_mask, Field1)),

    % Check field2: only present in row 2
    {value, Field2} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"field2">> end, Columns),
    ?assertEqual([2.0], maps:get(f64_values, maps:get(values, Field2))),
    ?assertEqual(<<0:5, 0:1, 1:1, 0:1>>, maps:get(null_mask, Field2)),

    % Check field3: only present in row 3
    {value, Field3} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"field3">> end, Columns),
    ?assertEqual([3.0], maps:get(f64_values, maps:get(values, Field3))),
    ?assertEqual(<<0:5, 0:1, 0:1, 1:1>>, maps:get(null_mask, Field3)).

t_insert_requests_all_data_types(_) ->
    Point =
        #{fields =>
              #{<<"int32_field">> => greptimedb_values:int32_value(42),
                <<"int64_field">> => greptimedb_values:int64_value(9223372036854775807),
                <<"uint32_field">> => greptimedb_values:uint32_value(4294967295),
                <<"uint64_field">> => greptimedb_values:uint64_value(18446744073709551615),
                <<"float64_field">> => greptimedb_values:float64_value(3.14159),
                <<"bool_field">> => greptimedb_values:boolean_value(true),
                <<"binary_field">> => greptimedb_values:binary_value(<<1, 2, 3>>),
                <<"default_field">> => 123.45}, % Should default to float64
          tags =>
              #{<<"string_tag">> => greptimedb_values:string_value("test_string"),
                <<"default_tag">> => "default_string"}, % Should default to string
          timestamp => greptimedb_values:timestamp_nanosecond_value(1619775142098000000)},

    Client = #{cli_opts => [{timeunit, ms}]},
    Request = greptimedb_encoder:insert_requests(Client, [{"types_test", [Point]}]),

    #{request := {inserts, #{inserts := [#{columns := Columns}]}}} = Request,

    % Verify int32 field
    {value, Int32Col} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"int32_field">> end, Columns),
    ?assertEqual('INT32', maps:get(datatype, Int32Col)),
    ?assertEqual([42], maps:get(i32_values, maps:get(values, Int32Col))),

    % Verify int64 field
    {value, Int64Col} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"int64_field">> end, Columns),
    ?assertEqual('INT64', maps:get(datatype, Int64Col)),
    ?assertEqual([9223372036854775807], maps:get(i64_values, maps:get(values, Int64Col))),

    % Verify uint32 field
    {value, UInt32Col} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"uint32_field">> end, Columns),
    ?assertEqual('UINT32', maps:get(datatype, UInt32Col)),
    ?assertEqual([4294967295], maps:get(u32_values, maps:get(values, UInt32Col))),

    % Verify boolean field
    {value, BoolCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"bool_field">> end, Columns),
    ?assertEqual('BOOLEAN', maps:get(datatype, BoolCol)),
    ?assertEqual([true], maps:get(bool_values, maps:get(values, BoolCol))),

    % Verify binary field
    {value, BinaryCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"binary_field">> end, Columns),
    ?assertEqual('BINARY', maps:get(datatype, BinaryCol)),
    ?assertEqual([<<1, 2, 3>>], maps:get(binary_values, maps:get(values, BinaryCol))),

    % Verify default field type (should be float64)
    {value, DefaultFieldCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"default_field">> end, Columns),
    ?assertEqual('FLOAT64', maps:get(datatype, DefaultFieldCol)),
    ?assertEqual([123.45], maps:get(f64_values, maps:get(values, DefaultFieldCol))),

    % Verify string tag
    {value, StringTagCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"string_tag">> end, Columns),
    ?assertEqual('STRING', maps:get(datatype, StringTagCol)),
    ?assertEqual(["test_string"], maps:get(string_values, maps:get(values, StringTagCol))),

    % Verify default tag type (should be string)
    {value, DefaultTagCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"default_tag">> end, Columns),
    ?assertEqual('STRING', maps:get(datatype, DefaultTagCol)),
    ?assertEqual(["default_string"],
                 maps:get(string_values, maps:get(values, DefaultTagCol))),

    % Verify timestamp with nanosecond precision
    {value, TsCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"greptime_timestamp">> end, Columns),
    ?assertEqual('TIMESTAMP_NANOSECOND', maps:get(datatype, TsCol)),
    ?assertEqual([1619775142098000000],
                 maps:get(timestamp_nanosecond_values, maps:get(values, TsCol))).

t_insert_requests_all_time_units(_) ->
    BaseTimestamp = 1619775142,
    TestCases =
        [{ns, BaseTimestamp * 1000000000, timestamp_nanosecond_values, 'TIMESTAMP_NANOSECOND'},
         {nanosecond,
          BaseTimestamp * 1000000000,
          timestamp_nanosecond_values,
          'TIMESTAMP_NANOSECOND'},
         {us, BaseTimestamp * 1000000, timestamp_microsecond_values, 'TIMESTAMP_MICROSECOND'},
         {microsecond,
          BaseTimestamp * 1000000,
          timestamp_microsecond_values,
          'TIMESTAMP_MICROSECOND'},
         {ms, BaseTimestamp * 1000, timestamp_millisecond_values, 'TIMESTAMP_MILLISECOND'},
         {millisecond,
          BaseTimestamp * 1000,
          timestamp_millisecond_values,
          'TIMESTAMP_MILLISECOND'},
         {s, BaseTimestamp, timestamp_second_values, 'TIMESTAMP_SECOND'},
         {second, BaseTimestamp, timestamp_second_values, 'TIMESTAMP_SECOND'}],

    lists:foreach(fun({TimeUnit, Timestamp, ValuesKey, DataType}) ->
                     Point =
                         #{fields => #{<<"temp">> => 25.0},
                           tags => #{<<"sensor">> => <<"test">>},
                           timestamp => Timestamp},

                     Client = #{cli_opts => [{timeunit, TimeUnit}]},
                     Request = greptimedb_encoder:insert_requests(Client, [{"time_test", [Point]}]),

                     #{request := {inserts, #{inserts := [#{columns := Columns}]}}} = Request,

                     {value, TsCol} =
                         lists:search(fun(C) -> maps:get(column_name, C) == <<"greptime_timestamp">>
                                      end,
                                      Columns),

                     ?assertEqual(DataType, maps:get(datatype, TsCol)),
                     ?assertEqual([Timestamp], maps:get(ValuesKey, maps:get(values, TsCol)))
                  end,
                  TestCases).

t_insert_requests_metric_formats(_) ->
    Point =
        #{fields => #{<<"temp">> => 25.0},
          tags => #{<<"sensor">> => <<"test">>},
          timestamp => 1619775142098},

    % Test 1: String table name
    Client1 = #{cli_opts => [{dbname, "custom_db"}, {timeunit, ms}]},
    Request1 = greptimedb_encoder:insert_requests(Client1, [{"string_table", [Point]}]),
    #{header := #{dbname := "custom_db"},
      request := {inserts, #{inserts := [#{table_name := "string_table"}]}}} =
        Request1,

    % Test 2: Binary table name
    Request2 = greptimedb_encoder:insert_requests(Client1, [{<<"binary_table">>, [Point]}]),
    #{request := {inserts, #{inserts := [#{table_name := <<"binary_table">>}]}}} = Request2,

    % Test 3: Atom table name
    Request3 = greptimedb_encoder:insert_requests(Client1, [{atom_table, [Point]}]),
    #{request := {inserts, #{inserts := [#{table_name := atom_table}]}}} = Request3,

    % Test 4: {DbName, Table} tuple format
    Request4 =
        greptimedb_encoder:insert_requests(Client1, [{{"tuple_db", "tuple_table"}, [Point]}]),
    #{header := #{dbname := "tuple_db"},
      request := {inserts, #{inserts := [#{table_name := "tuple_table"}]}}} =
        Request4,

    % Test 5: Map format with table override
    MetricMap =
        #{table => "map_table",
          dbname => "map_db",
          timeunit => second},
    Request5 = greptimedb_encoder:insert_requests(Client1, [{MetricMap, [Point]}]),
    #{header := #{dbname := "map_db"}} = Request5,
    #{request :=
          {inserts, #{inserts := [#{table_name := "map_table", columns := Columns}]}}} =
        Request5,

    % Verify timeunit override (should be seconds, not ms from client)
    {value, TsCol} =
        lists:search(fun(C) -> maps:get(column_name, C) == <<"greptime_timestamp">> end, Columns),
    ?assertEqual('TIMESTAMP_SECOND', maps:get(datatype, TsCol)).
