-module(greptimedb_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

%% Test credential macros
-define(TEST_USERNAME, "test").
-define(TEST_PASSWORD, "test").
-define(GREPTIME_USERNAME, <<"greptime_user">>).
-define(GREPTIME_PASSWORD, <<"greptime_pwd">>).
-define(WRONG_PASSWORD, <<"wrong_pwd">>).

all() ->
    [t_write,
     t_write_stream,
     t_write_failure,
     t_write_batch,
     t_bench_perf,
     t_write_stream,
     t_async_write_batch,
     t_write_greptime_cloud,
     t_auth_error,
     t_insert_requests,
     t_insert_requests_with_timeunit,
     t_insert_requests_empty_points,
     t_insert_requests_single_point,
     t_insert_requests_all_sparse,
     t_insert_requests_all_data_types,
     t_insert_requests_all_time_units,
     t_insert_requests_metric_formats,
     t_write_sparse_and_non_sparse,
     t_write_custom_ts_column].

%%[t_bench_perf].
%%[t_insert_requests, t_bench_perf].

init_per_suite(Config) ->
    application:ensure_all_started(greptimedb),
    application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    application:stop(greptimedb).

init_per_testcase(t_write_greptime_cloud, Config) ->
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
    AuthInfo = {basic, #{username => ?TEST_USERNAME, password => ?TEST_PASSWORD}},
    Client = #{cli_opts => [{auth, AuthInfo}, {timeunit, second}]},
    Request = greptimedb_encoder:insert_requests(Client, [{Metric, Points}]),
    case Request of
        #{header := #{dbname := DbName, authorization := Auth},
          request := {row_inserts, #{inserts := [#{rows := #{schema := Schema, rows := Rows}}]}}} ->
            ?assertEqual(DbName, "greptime-public"),
            ?assertEqual(8, length(Schema)),
            ?assertEqual(3, length(Rows)),
            ?assertEqual(Auth, #{auth_scheme => AuthInfo}),

            ColumnNames = [maps:get(column_name, S) || S <- Schema],
            ?assert(lists:member(<<"greptime_timestamp">>, ColumnNames)),
            ?assert(lists:member(<<"temperature">>, ColumnNames)),
            ?assert(lists:member(<<"qos">>, ColumnNames)),
            ?assert(lists:member(<<"to">>, ColumnNames)),
            ?assert(lists:member(<<"device">>, ColumnNames)),

            GetColumnIndex =
                fun(ColName) ->
                   Zipped =
                       lists:zip(
                           lists:seq(1, length(ColumnNames)), ColumnNames),
                   case lists:keyfind(ColName, 2, Zipped) of
                       {Index, _Name} ->
                           Index;
                       false ->
                           not_found
                   end
                end,

            TsIdx = GetColumnIndex(<<"greptime_timestamp">>),
            TempIdx = GetColumnIndex(<<"temperature">>),
            HostIdx = GetColumnIndex(<<"host">>),
            RegionIdx = GetColumnIndex(<<"region">>),
            QosIdx = GetColumnIndex(<<"qos">>),
            ToIdx = GetColumnIndex(<<"to">>),
            DeviceIdx = GetColumnIndex(<<"device">>),
            FromIdx = GetColumnIndex(<<"from">>),

            [Row1, Row2, Row3] = Rows,
            #{values := Values1} = Row1,
            #{values := Values2} = Row2,
            #{values := Values3} = Row3,

            ?assertEqual(8, length(Values1)),
            ?assertEqual(8, length(Values2)),
            ?assertEqual(8, length(Values3)),

            % Verify row 1 content
            ?assertEqual(#{value_data => {timestamp_second_value, 1619775142098}},
                         lists:nth(TsIdx, Values1)),
            ?assertEqual(#{value_data => {f64_value, 1}}, lists:nth(TempIdx, Values1)),
            ?assertEqual(#{value_data => {string_value, <<"serverA">>}},
                         lists:nth(HostIdx, Values1)),
            ?assertEqual(#{value_data => {string_value, <<"hangzhou">>}},
                         lists:nth(RegionIdx, Values1)),
            ?assertEqual(#{value_data => {string_value, "0"}}, lists:nth(QosIdx, Values1)),
            ?assertEqual(#{}, lists:nth(ToIdx, Values1)),  % Row 1 doesn't have "to" field (sparse)
            ?assertEqual(#{value_data => {string_value, <<"NO.1">>}},
                         lists:nth(DeviceIdx, Values1)),
            ?assertEqual(#{value_data => {string_value, <<"mqttx_4b963a8e">>}},
                         lists:nth(FromIdx, Values1)),

            % Verify row 2 content
            ?assertEqual(#{value_data => {timestamp_second_value, 1619775143098}},
                         lists:nth(TsIdx, Values2)),
            ?assertEqual(#{value_data => {f64_value, 2}}, lists:nth(TempIdx, Values2)),
            ?assertEqual(#{value_data => {string_value, <<"serverB">>}},
                         lists:nth(HostIdx, Values2)),
            ?assertEqual(#{value_data => {string_value, <<"ningbo">>}},
                         lists:nth(RegionIdx, Values2)),
            ?assertEqual(#{value_data => {string_value, "1"}}, lists:nth(QosIdx, Values2)),
            ?assertEqual(#{value_data => {string_value, <<"kafka">>}}, lists:nth(ToIdx, Values2)),
            ?assertEqual(#{},
                         lists:nth(DeviceIdx,
                                   Values2)),  % Row 2 doesn't have "device" field (sparse)
            ?assertEqual(#{value_data => {string_value, <<"mqttx_4b963a8e">>}},
                         lists:nth(FromIdx, Values2)),

            % Verify row 3 content
            ?assertEqual(#{value_data => {timestamp_second_value, 1619775144098}},
                         lists:nth(TsIdx, Values3)),
            ?assertEqual(#{value_data => {f64_value, 3}}, lists:nth(TempIdx, Values3)),
            ?assertEqual(#{value_data => {string_value, <<"serverB">>}},
                         lists:nth(HostIdx, Values3)),
            ?assertEqual(#{value_data => {string_value, <<"xiamen">>}},
                         lists:nth(RegionIdx, Values3)),
            ?assertEqual(#{value_data => {string_value, "2"}}, lists:nth(QosIdx, Values3)),
            ?assertEqual(#{value_data => {string_value, <<"kafka">>}}, lists:nth(ToIdx, Values3)),
            ?assertEqual(#{},
                         lists:nth(DeviceIdx,
                                   Values3)),  % Row 3 doesn't have "device" field (sparse)
            ?assertEqual(#{value_data => {string_value, <<"mqttx_4b963a8e">>}},
                         lists:nth(FromIdx, Values3));
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
    AuthInfo = {basic, #{username => ?TEST_USERNAME, password => ?TEST_PASSWORD}},
    Client = #{cli_opts => [{auth, AuthInfo}, {timeunit, second}]},
    Metric = #{table => "Test", timeunit => nanosecond},
    Request = greptimedb_encoder:insert_requests(Client, [{Metric, Points}]),
    #{header := #{dbname := _DbName, authorization := _Auth},
      request := {row_inserts, #{inserts := [#{rows := #{schema := Schema, rows := Rows}}]}}} =
        Request,
    ?assertEqual(1, length(Rows)),
    % Find timestamp column in schema
    {value, TsSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"greptime_timestamp">> end, Schema),
    ?assertEqual('TIMESTAMP_NANOSECOND', maps:get(datatype, TsSchema)).

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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

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
    drop_table(Metric),
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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),
    {ok, #{response := {affected_rows, #{value := 2}}}} =
        greptimedb:write(Client, Metric, Points),

    %% assert table options
    ShowCreate = execute_sql_query("show create table temperatures"),

    ?assert(string:find(ShowCreate, "ttl = '7days'") =/= nomatch),
    ?assert(string:find(ShowCreate, "append_mode = 'true'") =/= nomatch),

    QueryResult =
        execute_sql_query("SELECT greptime_timestamp,host,to,temperature,qos,`from`,region FROM temperatures ORDER BY greptime_timestamp DESC",
                          <<"output">>),
    ?assertEqual(<<"[
  {
    \"records\": {
      \"rows\": [
        [
          1619775143098,
          \"serverB\",
          \"kafka\",
          2.0,
          1,
          \"mqttx_4b963a8e\",
          \"ningbo\"
        ],
        [
          1619775142098,
          \"serverA\",
          null,
          1.0,
          0,
          \"mqttx_4b963a8e\",
          \"hangzhou\"
        ]
      ],
      \"schema\": {
        \"column_schemas\": [
          {
            \"data_type\": \"TimestampMillisecond\",
            \"name\": \"greptime_timestamp\"
          },
          {
            \"data_type\": \"String\",
            \"name\": \"host\"
          },
          {
            \"data_type\": \"String\",
            \"name\": \"to\"
          },
          {
            \"data_type\": \"Float64\",
            \"name\": \"temperature\"
          },
          {
            \"data_type\": \"Int64\",
            \"name\": \"qos\"
          },
          {
            \"data_type\": \"String\",
            \"name\": \"from\"
          },
          {
            \"data_type\": \"String\",
            \"name\": \"region\"
          }
        ]
      },
      \"total_rows\": 2
    }
  }
]">>,
                 QueryResult),

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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?WRONG_PASSWORD}}}],
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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),

    Metric = <<"temperatures_batch">>,
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

t_write_f32(_) ->
    Metric = <<"table_f32">>,
    drop_table(Metric),
    Points =
        [#{fields => #{<<"field_value">> => greptimedb_values:float32_value(3.0)},
           tags =>
               #{<<"tag_value">> => greptimedb_values:float32_value(14.0)},
           timestamp => 1619775142098}],
    Host = greptime_host(),
    Options =
        [{endpoints, [{http, Host, 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 5},
         %% enable append mode and ttl
         {grpc_hints, #{<<"append_mode">> => <<"true">>, <<"ttl">> => <<"7 days">>}},
         {pool_type, random},
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),
    {ok, #{response := {affected_rows, #{value := 1}}}} =
        greptimedb:write(Client, Metric, Points),
    SQL = iolist_to_binary(["SELECT * FROM ", Metric]),
    ?assertMatch(
       [#{ <<"records">> :=
               #{ <<"rows">> := [[_, 14.0, 3.0]]
                , <<"schema">> := #{
                    <<"column_schemas">> := [_,
                                             #{<<"data_type">> := <<"Float32">>},
                                             #{<<"data_type">> := <<"Float32">>}]
                  }
                }
         }],
       jsx:decode(execute_sql_query(SQL, <<"output">>), [return_maps])),
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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

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
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

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

t_write_greptime_cloud(_) ->
    Host = os:getenv("GT_TEST_HOST"),
    DbName = os:getenv("GT_TEST_DB"),
    UserName = os:getenv("GT_TEST_USER"),
    PassWd = os:getenv("GT_TEST_PASSWD"),

    if (Host == false) or (DbName == false) or (UserName == false) or (PassWd == false) ->
           ct:print("Ignored t_write_greptime_cloud..."),
           ok;
       true ->
           ct:print("Running t_write_greptime_cloud..."),
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

t_insert_requests_empty_points(_) ->
    Client = #{cli_opts => [{timeunit, ms}]},
    Request = greptimedb_encoder:insert_requests(Client, []),
    #{header := #{dbname := unknown}, request := {row_inserts, #{inserts := []}}} = Request.

t_insert_requests_single_point(_) ->
    Point =
        #{fields => #{<<"temp">> => 25.5},
          tags => #{<<"sensor">> => <<"room1">>},
          timestamp => 1619775142098},
    Client = #{cli_opts => [{timeunit, ms}]},
    Request = greptimedb_encoder:insert_requests(Client, [{"sensors", [Point]}]),

    #{header := #{dbname := "greptime-public"},
      request :=
          {row_inserts,
           #{inserts := [#{table_name := "sensors", rows := #{schema := Schema, rows := Rows}}]}}} =
        Request,

    ?assertEqual(3, length(Schema)), % temp, sensor, timestamp
    ?assertEqual(1, length(Rows)),

    ColumnNames = [maps:get(column_name, S) || S <- Schema],
    ?assert(lists:member(<<"greptime_timestamp">>, ColumnNames)),
    ?assert(lists:member(<<"temp">>, ColumnNames)),
    ?assert(lists:member(<<"sensor">>, ColumnNames)).

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

    #{request :=
          {row_inserts, #{inserts := [#{rows := #{schema := Schema, rows := Rows}}]}}} =
        Request,

    ?assertEqual(7, length(Schema)), % 3 fields + 3 tags + timestamp
    ?assertEqual(3, length(Rows)),

    ColumnNames = [maps:get(column_name, S) || S <- Schema],
    ?assert(lists:member(<<"field1">>, ColumnNames)),
    ?assert(lists:member(<<"field2">>, ColumnNames)),
    ?assert(lists:member(<<"field3">>, ColumnNames)),
    ?assert(lists:member(<<"tag1">>, ColumnNames)),
    ?assert(lists:member(<<"tag2">>, ColumnNames)),
    ?assert(lists:member(<<"tag3">>, ColumnNames)),
    ?assert(lists:member(<<"greptime_timestamp">>, ColumnNames)),

    GetColumnIndex =
        fun(ColName) ->
           Zipped =
               lists:zip(
                   lists:seq(1, length(ColumnNames)), ColumnNames),
           case lists:keyfind(ColName, 2, Zipped) of
               {Index, _Name} ->
                   Index;
               false ->
                   not_found
           end
        end,

    % Get column indices
    TsIdx = GetColumnIndex(<<"greptime_timestamp">>),
    Field1Idx = GetColumnIndex(<<"field1">>),
    Field2Idx = GetColumnIndex(<<"field2">>),
    Field3Idx = GetColumnIndex(<<"field3">>),
    Tag1Idx = GetColumnIndex(<<"tag1">>),
    Tag2Idx = GetColumnIndex(<<"tag2">>),
    Tag3Idx = GetColumnIndex(<<"tag3">>),

    [Row1, Row2, Row3] = Rows,
    #{values := Values1} = Row1,
    #{values := Values2} = Row2,
    #{values := Values3} = Row3,

    ?assertEqual(7, length(Values1)),
    ?assertEqual(7, length(Values2)),
    ?assertEqual(7, length(Values3)),

    % Verify row 1 content (only field1 and tag1 should have values)
    ?assertEqual(#{value_data => {timestamp_second_value, 1000000000}},
                 lists:nth(TsIdx, Values1)),
    ?assertEqual(#{value_data => {f64_value, 1.0}}, lists:nth(Field1Idx, Values1)),
    ?assertEqual(#{}, lists:nth(Field2Idx, Values1)),  % sparse: not present
    ?assertEqual(#{}, lists:nth(Field3Idx, Values1)),  % sparse: not present
    ?assertEqual(#{value_data => {string_value, <<"value1">>}}, lists:nth(Tag1Idx, Values1)),
    ?assertEqual(#{}, lists:nth(Tag2Idx, Values1)),    % sparse: not present
    ?assertEqual(#{}, lists:nth(Tag3Idx, Values1)),    % sparse: not present

    % Verify row 2 content (only field2 and tag2 should have values)
    ?assertEqual(#{value_data => {timestamp_second_value, 1000000001}},
                 lists:nth(TsIdx, Values2)),
    ?assertEqual(#{}, lists:nth(Field1Idx, Values2)),  % sparse: not present
    ?assertEqual(#{value_data => {f64_value, 2.0}}, lists:nth(Field2Idx, Values2)),
    ?assertEqual(#{}, lists:nth(Field3Idx, Values2)),  % sparse: not present
    ?assertEqual(#{}, lists:nth(Tag1Idx, Values2)),    % sparse: not present
    ?assertEqual(#{value_data => {string_value, <<"value2">>}}, lists:nth(Tag2Idx, Values2)),
    ?assertEqual(#{}, lists:nth(Tag3Idx, Values2)),    % sparse: not present

    % Verify row 3 content (only field3 and tag3 should have values)
    ?assertEqual(#{value_data => {timestamp_second_value, 1000000002}},
                 lists:nth(TsIdx, Values3)),
    ?assertEqual(#{}, lists:nth(Field1Idx, Values3)),  % sparse: not present
    ?assertEqual(#{}, lists:nth(Field2Idx, Values3)),  % sparse: not present
    ?assertEqual(#{value_data => {f64_value, 3.0}}, lists:nth(Field3Idx, Values3)),
    ?assertEqual(#{}, lists:nth(Tag1Idx, Values3)),    % sparse: not present
    ?assertEqual(#{}, lists:nth(Tag2Idx, Values3)),    % sparse: not present
    ?assertEqual(#{value_data => {string_value, <<"value3">>}}, lists:nth(Tag3Idx, Values3)).

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

    #{request :=
          {row_inserts, #{inserts := [#{rows := #{schema := Schema, rows := Rows}}]}}} =
        Request,
    ?assertEqual(1, length(Rows)),

    % Verify schema datatypes
    {value, Int32Schema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"int32_field">> end, Schema),
    ?assertEqual('INT32', maps:get(datatype, Int32Schema)),

    {value, Int64Schema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"int64_field">> end, Schema),
    ?assertEqual('INT64', maps:get(datatype, Int64Schema)),

    {value, UInt32Schema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"uint32_field">> end, Schema),
    ?assertEqual('UINT32', maps:get(datatype, UInt32Schema)),

    {value, BoolSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"bool_field">> end, Schema),
    ?assertEqual('BOOLEAN', maps:get(datatype, BoolSchema)),

    {value, BinarySchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"binary_field">> end, Schema),
    ?assertEqual('BINARY', maps:get(datatype, BinarySchema)),

    {value, DefaultFieldSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"default_field">> end, Schema),
    ?assertEqual('FLOAT64', maps:get(datatype, DefaultFieldSchema)),

    {value, StringTagSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"string_tag">> end, Schema),
    ?assertEqual('STRING', maps:get(datatype, StringTagSchema)),

    {value, DefaultTagSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"default_tag">> end, Schema),
    ?assertEqual('STRING', maps:get(datatype, DefaultTagSchema)),

    {value, TsSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"greptime_timestamp">> end, Schema),
    ?assertEqual('TIMESTAMP_NANOSECOND', maps:get(datatype, TsSchema)),

    [Row] = Rows,
    #{values := Values} = Row,
    ?assertEqual(11, length(Values)).

t_insert_requests_all_time_units(_) ->
    BaseTimestamp = 1619775142,
    TestCases =
        [{ns, BaseTimestamp * 1000000000, 'TIMESTAMP_NANOSECOND'},
         {nanosecond, BaseTimestamp * 1000000000, 'TIMESTAMP_NANOSECOND'},
         {us, BaseTimestamp * 1000000, 'TIMESTAMP_MICROSECOND'},
         {microsecond, BaseTimestamp * 1000000, 'TIMESTAMP_MICROSECOND'},
         {ms, BaseTimestamp * 1000, 'TIMESTAMP_MILLISECOND'},
         {millisecond, BaseTimestamp * 1000, 'TIMESTAMP_MILLISECOND'},
         {s, BaseTimestamp, 'TIMESTAMP_SECOND'},
         {second, BaseTimestamp, 'TIMESTAMP_SECOND'}],

    lists:foreach(fun({TimeUnit, Timestamp, DataType}) ->
                     Point =
                         #{fields => #{<<"temp">> => 25.0},
                           tags => #{<<"sensor">> => <<"test">>},
                           timestamp => Timestamp},

                     Client = #{cli_opts => [{timeunit, TimeUnit}]},
                     Request = greptimedb_encoder:insert_requests(Client, [{"time_test", [Point]}]),

                     #{request :=
                           {row_inserts,
                            #{inserts := [#{rows := #{schema := Schema, rows := Rows}}]}}} =
                         Request,
                     ?assertEqual(1, length(Rows)),

                     {value, TsSchema} =
                         lists:search(fun(S) -> maps:get(column_name, S) == <<"greptime_timestamp">>
                                      end,
                                      Schema),

                     ?assertEqual(DataType, maps:get(datatype, TsSchema))
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
      request := {row_inserts, #{inserts := [#{table_name := "string_table"}]}}} =
        Request1,

    % Test 2: Binary table name
    Request2 = greptimedb_encoder:insert_requests(Client1, [{<<"binary_table">>, [Point]}]),
    #{request := {row_inserts, #{inserts := [#{table_name := <<"binary_table">>}]}}} =
        Request2,

    % Test 3: Atom table name
    Request3 = greptimedb_encoder:insert_requests(Client1, [{atom_table, [Point]}]),
    #{request := {row_inserts, #{inserts := [#{table_name := atom_table}]}}} = Request3,

    % Test 4: {DbName, Table} tuple format
    Request4 =
        greptimedb_encoder:insert_requests(Client1, [{{"tuple_db", "tuple_table"}, [Point]}]),
    #{header := #{dbname := "tuple_db"},
      request := {row_inserts, #{inserts := [#{table_name := "tuple_table"}]}}} =
        Request4,

    % Test 5: Map format with table override
    MetricMap =
        #{table => "map_table",
          dbname => "map_db",
          timeunit => second},
    Request5 = greptimedb_encoder:insert_requests(Client1, [{MetricMap, [Point]}]),
    #{header := #{dbname := "map_db"}} = Request5,
    #{request :=
          {row_inserts,
           #{inserts :=
                 [#{table_name := "map_table", rows := #{schema := Schema, rows := _Rows}}]}}} =
        Request5,

    % Verify timeunit override (should be seconds, not ms from client)
    {value, TsSchema} =
        lists:search(fun(S) -> maps:get(column_name, S) == <<"greptime_timestamp">> end, Schema),
    ?assertEqual('TIMESTAMP_SECOND', maps:get(datatype, TsSchema)).

%% Helper function to execute SQL query and return pretty-printed JSON
execute_sql_query(Sql) ->
    execute_sql_query(Sql, undefined).

execute_sql_query(Sql, Key) ->
    User = ?GREPTIME_USERNAME,
    Pass = ?GREPTIME_PASSWORD,
    EncodedSql = uri_string:quote(Sql),
    URL = "http://" ++ greptime_host() ++ ":4000/v1/sql?sql=" ++ EncodedSql,
    AuthBin = base64:encode(<<User/binary, ":", Pass/binary>>),
    AuthHeader = {"authorization", <<"Basic ", AuthBin/binary>>},

    {ok, {{_, 200, _}, _, RespBody}} = httpc:request(get, {URL, [AuthHeader]}, [], []),
    % Parse JSON and pretty print
    JsonTerm = jsx:decode(list_to_binary(RespBody), [return_maps]),
    JsonTerm1 =
        case Key of
            undefined ->
                JsonTerm;
            _ ->
                maps:get(Key, JsonTerm)
        end,
    jsx:prettify(
        jsx:encode(JsonTerm1)).

drop_table(Table) ->
    Rows = execute_sql_query(<<"DROP TABLE IF EXISTS ", Table/binary>>, <<"output">>),
    ?assertEqual(<<"[
  {
    \"affectedrows\": 0
  }
]">>, Rows).

t_write_sparse_and_non_sparse(_) ->
    Metric = <<"t_write_sparse_and_non_sparse">>,
    drop_table(Metric),

    %% Create mixed sparse and non-sparse points
    Points =
        [%% Point 1: Complete data with all common fields
         #{fields => #{<<"temperature">> => 25.5, <<"humidity">> => 60.0},
           tags =>
               #{<<"sensor_id">> => <<"sensor_001">>,
                 <<"location">> => <<"room_a">>,
                 <<"building">> => <<"main">>},
           timestamp => 1619775142000},
         %% Point 2: Missing optional building tag
         #{fields => #{<<"temperature">> => 26.0, <<"humidity">> => 65.0},
           tags => #{<<"sensor_id">> => <<"sensor_002">>, <<"location">> => <<"room_b">>},
           timestamp => 1619775143000},
         %% Point 3: Missing humidity field and building tag
         #{fields => #{<<"temperature">> => 24.0},
           tags => #{<<"sensor_id">> => <<"sensor_003">>, <<"location">> => <<"room_c">>},
           timestamp => 1619775144000},
         %% Point 4: Has extra optional pressure field and status tag
         #{fields =>
               #{<<"temperature">> => 22.0,
                 <<"humidity">> => 55.0,
                 <<"pressure">> => 1013.0},
           tags =>
               #{<<"sensor_id">> => <<"sensor_004">>,
                 <<"location">> => <<"room_d">>,
                 <<"status">> => <<"active">>},
           timestamp => 1619775145000},
         %% Point 5: Similar to point 1 but with extra status tag
         #{fields => #{<<"temperature">> => 27.0, <<"humidity">> => 68.0},
           tags =>
               #{<<"sensor_id">> => <<"sensor_005">>,
                 <<"location">> => <<"room_e">>,
                 <<"building">> => <<"annex">>,
                 <<"status">> => <<"maintenance">>},
           timestamp => 1619775146000}],

    Host = greptime_host(),
    Options =
        [{endpoints, [{http, Host, 4001}]},
         {pool, greptimedb_client_pool},
         {pool_size, 5},
         {pool_type, random},
         {timeunit, ms},
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),

    %% Write the mixed sparse/non-sparse data
    {ok, #{response := {affected_rows, #{value := 5}}}} =
        greptimedb:write(Client, Metric, Points),

    QueryResult =
        execute_sql_query("SELECT * FROM t_write_sparse_and_non_sparse ORDER BY greptime_timestamp ASC",
                          <<"output">>),

    JsonTerm = jsx:decode(QueryResult, [return_maps]),
    [#{<<"records">> := #{<<"rows">> := Rows, <<"schema">> := Schema}}] = JsonTerm,

    ?assertEqual(5, length(Rows)),

    #{<<"column_schemas">> := ColumnSchemas} = Schema,
    ColumnNames = [maps:get(<<"name">>, Col) || Col <- ColumnSchemas],

    GetColIndex =
        fun(ColName) ->
           Zipped =
               lists:zip(
                   lists:seq(1, length(ColumnNames)), ColumnNames),
           case lists:keyfind(ColName, 2, Zipped) of
               {Index, _Name} ->
                   Index;
               false ->
                   not_found
           end
        end,

    ExpectedColumns =
        [<<"greptime_timestamp">>,
         <<"temperature">>,
         <<"humidity">>,
         <<"pressure">>,
         <<"sensor_id">>,
         <<"location">>,
         <<"building">>,
         <<"status">>],

    lists:foreach(fun(ExpectedCol) -> ?assert(lists:member(ExpectedCol, ColumnNames)) end,
                  ExpectedColumns),

    [Row1, Row2, Row3, Row4, Row5] = Rows,

    GetValue =
        fun(Row, ColName) ->
           case GetColIndex(ColName) of
               not_found ->
                   not_found;
               Index ->
                   lists:nth(Index, Row)
           end
        end,

    %% Verify Point 1 (complete baseline data)
    ?assertEqual(1619775142000, GetValue(Row1, <<"greptime_timestamp">>)),
    ?assertEqual(25.5, GetValue(Row1, <<"temperature">>)),
    ?assertEqual(60.0, GetValue(Row1, <<"humidity">>)),
    ?assertEqual(null, GetValue(Row1, <<"pressure">>)), % Not present in this point
    ?assertEqual(<<"sensor_001">>, GetValue(Row1, <<"sensor_id">>)),
    ?assertEqual(<<"room_a">>, GetValue(Row1, <<"location">>)),
    ?assertEqual(<<"main">>, GetValue(Row1, <<"building">>)),
    ?assertEqual(null, GetValue(Row1, <<"status">>)), % Not present in this point

    %% Verify Point 2 (missing building tag)
    ?assertEqual(1619775143000, GetValue(Row2, <<"greptime_timestamp">>)),
    ?assertEqual(26.0, GetValue(Row2, <<"temperature">>)),
    ?assertEqual(65.0, GetValue(Row2, <<"humidity">>)),
    ?assertEqual(null, GetValue(Row2, <<"pressure">>)), % Not present
    ?assertEqual(<<"sensor_002">>, GetValue(Row2, <<"sensor_id">>)),
    ?assertEqual(<<"room_b">>, GetValue(Row2, <<"location">>)),
    ?assertEqual(null, GetValue(Row2, <<"building">>)), % Sparse - not present
    ?assertEqual(null, GetValue(Row2, <<"status">>)), % Not present

    %% Verify Point 3 (missing humidity field and building tag)
    ?assertEqual(1619775144000, GetValue(Row3, <<"greptime_timestamp">>)),
    ?assertEqual(24.0, GetValue(Row3, <<"temperature">>)),
    ?assertEqual(null, GetValue(Row3, <<"humidity">>)), % Sparse - not present
    ?assertEqual(null, GetValue(Row3, <<"pressure">>)), % Not present
    ?assertEqual(<<"sensor_003">>, GetValue(Row3, <<"sensor_id">>)),
    ?assertEqual(<<"room_c">>, GetValue(Row3, <<"location">>)),
    ?assertEqual(null, GetValue(Row3, <<"building">>)), % Sparse - not present
    ?assertEqual(null, GetValue(Row3, <<"status">>)), % Not present

    %% Verify Point 4 (has extra pressure field and status tag)
    ?assertEqual(1619775145000, GetValue(Row4, <<"greptime_timestamp">>)),
    ?assertEqual(22.0, GetValue(Row4, <<"temperature">>)),
    ?assertEqual(55.0, GetValue(Row4, <<"humidity">>)),
    ?assertEqual(1013.0, GetValue(Row4, <<"pressure">>)), % Extra field present
    ?assertEqual(<<"sensor_004">>, GetValue(Row4, <<"sensor_id">>)),
    ?assertEqual(<<"room_d">>, GetValue(Row4, <<"location">>)),
    ?assertEqual(null, GetValue(Row4, <<"building">>)), % Not present
    ?assertEqual(<<"active">>, GetValue(Row4, <<"status">>)), % Extra tag present

    %% Verify Point 5 (complete with all optional fields)
    ?assertEqual(1619775146000, GetValue(Row5, <<"greptime_timestamp">>)),
    ?assertEqual(27.0, GetValue(Row5, <<"temperature">>)),
    ?assertEqual(68.0, GetValue(Row5, <<"humidity">>)),
    ?assertEqual(null, GetValue(Row5, <<"pressure">>)), % Not present in this point
    ?assertEqual(<<"sensor_005">>, GetValue(Row5, <<"sensor_id">>)),
    ?assertEqual(<<"room_e">>, GetValue(Row5, <<"location">>)),
    ?assertEqual(<<"annex">>, GetValue(Row5, <<"building">>)),
    ?assertEqual(<<"maintenance">>, GetValue(Row5, <<"status">>)), % Present

    greptimedb:stop_client(Client),
    ok.

t_write_custom_ts_column(_) ->
    %% Test writing with custom timestamp column name
    Metric = <<"custom_ts_test">>,
    drop_table(Metric),

    %% Define custom timestamp column name
    CustomTsColumn = <<"event_time">>,

    Points =
        [#{fields => #{<<"temperature">> => 25.5, <<"humidity">> => 60.0},
           tags => #{<<"sensor_id">> => <<"sensor_001">>, <<"location">> => <<"room_a">>},
           timestamp => 1619775142000},
         #{fields => #{<<"temperature">> => 26.0, <<"humidity">> => 65.0},
           tags => #{<<"sensor_id">> => <<"sensor_002">>, <<"location">> => <<"room_b">>},
           timestamp => 1619775143000}],

    %% Create client with custom ts_column option
    Host = greptime_host(),
    Options =
        [{endpoints, [{http, Host, 4001}]},
         {pool, greptimedb_client_pool_custom_ts},
         {pool_size, 3},
         {pool_type, random},
         {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}},
         {ts_column, CustomTsColumn}],

    {ok, Client} = greptimedb:start_client(Options),
    true = greptimedb:is_alive(Client),

    %% Write with custom timestamp column
    {ok, #{response := {affected_rows, #{value := 2}}}} =
        greptimedb:write(Client, Metric, Points),

    QueryResult =
        execute_sql_query(io_lib:format("SELECT event_time, sensor_id, location, temperature, humidity FROM ~s ORDER BY event_time ASC",
                                        [Metric]),
                          <<"output">>),

    JsonTerm = jsx:decode(QueryResult, [return_maps]),
    [#{<<"records">> := #{<<"rows">> := Rows}}] = JsonTerm,

    ?assertEqual(2, length(Rows)),
    [Row1, Row2] = Rows,

    [Ts1, SensorId1, Location1, Temp1, Humidity1] = Row1,
    ?assertEqual(1619775142000, Ts1),
    ?assertEqual(<<"sensor_001">>, SensorId1),
    ?assertEqual(<<"room_a">>, Location1),
    ?assertEqual(25.5, Temp1),
    ?assertEqual(60.0, Humidity1),

    [Ts2, SensorId2, Location2, Temp2, Humidity2] = Row2,
    ?assertEqual(1619775143000, Ts2),
    ?assertEqual(<<"sensor_002">>, SensorId2),
    ?assertEqual(<<"room_b">>, Location2),
    ?assertEqual(26.0, Temp2),
    ?assertEqual(65.0, Humidity2),

    greptimedb:stop_client(Client),
    ok.
