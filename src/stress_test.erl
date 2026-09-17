-module(stress_test).

-export([run/0, run/1, test_single_write/0]).

-define(GREPTIME_USERNAME, <<"greptime_user">>).
-define(GREPTIME_PASSWORD, <<"greptime_pwd">>).
-define(TEST_DURATION_MINUTES, 30).
-define(ROWS_PER_SECOND, 10000).
-define(BATCH_SIZE, 100).
-define(BATCH_INTERVAL_MS, (?BATCH_SIZE * 1000) div ?ROWS_PER_SECOND).

greptime_host() ->
    os:getenv("GT_HOST", "localhost").

get_current_timestamp_ms() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

pick_random_value(List) ->
    lists:nth(rand:uniform(length(List)), List).

generate_mixed_points(BaseTimestamp, Count) ->
    lists:map(fun(N) ->
                 case rand:uniform(3) of
                     1 -> generate_simple_complete_point(BaseTimestamp, N);
                     _ -> generate_simple_sparse_point(BaseTimestamp, N)
                 end
              end, lists:seq(1, Count)).

generate_simple_complete_point(BaseTimestamp, SequenceId) ->
    #{fields => #{<<"temperature">> => rand:uniform() * 50.0,
                  <<"humidity">> => rand:uniform() * 100.0,
                  <<"pressure">> => 1000.0 + rand:uniform() * 50.0},
      tags => #{<<"device_id">> => <<"dev_", (integer_to_binary(rand:uniform(5)))/binary>>,
                <<"location">> => pick_random_value([<<"room_a">>, <<"room_b">>, <<"room_c">>]),
                <<"status">> => pick_random_value([<<"active">>, <<"standby">>])},
      timestamp => BaseTimestamp + SequenceId}.

generate_simple_sparse_point(BaseTimestamp, SequenceId) ->
    Fields = case rand:uniform(3) of
                 1 -> #{<<"temperature">> => rand:uniform() * 50.0};
                 2 -> #{<<"humidity">> => rand:uniform() * 100.0};
                 3 -> #{<<"temperature">> => rand:uniform() * 50.0,
                        <<"pressure">> => 1000.0 + rand:uniform() * 50.0}
             end,

    Tags = case rand:uniform(2) of
               1 -> #{<<"device_id">> => <<"dev_", (integer_to_binary(rand:uniform(5)))/binary>>};
               2 -> #{<<"device_id">> => <<"dev_", (integer_to_binary(rand:uniform(5)))/binary>>,
                      <<"location">> => pick_random_value([<<"room_a">>, <<"room_b">>, <<"room_c">>])}
           end,

    #{fields => Fields,
      tags => Tags,
      timestamp => BaseTimestamp + SequenceId}.

write_batch(Client, Metric, BaseTimestamp, BatchSize, Stats) ->
    Points = generate_mixed_points(BaseTimestamp, BatchSize),

    StartTime = erlang:monotonic_time(millisecond),
    Result = greptimedb:write(Client, Metric, Points),
    EndTime = erlang:monotonic_time(millisecond),

    WriteTime = EndTime - StartTime,

    case Result of
        {ok, #{response := {affected_rows, #{value := AffectedRows}}}} ->
            NewStats = Stats#{
                total_points => maps:get(total_points, Stats, 0) + AffectedRows,
                total_batches => maps:get(total_batches, Stats, 0) + 1,
                total_write_time => maps:get(total_write_time, Stats, 0) + WriteTime,
                last_write_time => WriteTime,
                last_affected_rows => AffectedRows
            },
            {ok, NewStats};
        {error, Error} ->
            TotalErrors = maps:get(total_errors, Stats, 0) + 1,
            if TotalErrors =< 5 ->
                   io:format("Write error #~p: ~p~n", [TotalErrors, Error]);
               TotalErrors =:= 10 ->
                   io:format("Too many errors, suppressing further error messages~n");
               true ->
                   ok
            end,
            NewStats = Stats#{
                total_errors => TotalErrors
            },
            {error, NewStats}
    end.

print_stats(Stats, ElapsedMinutes) ->
    TotalPoints = maps:get(total_points, Stats, 0),
    TotalBatches = maps:get(total_batches, Stats, 0),
    TotalErrors = maps:get(total_errors, Stats, 0),
    TotalWriteTime = maps:get(total_write_time, Stats, 0),
    LastWriteTime = maps:get(last_write_time, Stats, 0),
    LastAffectedRows = maps:get(last_affected_rows, Stats, 0),

    AvgWriteTime = case TotalBatches of
                      0 -> 0.0;
                      _ -> TotalWriteTime / TotalBatches
                  end,

    PointsPerSecond = case ElapsedMinutes < 0.001 of
                         true -> 0.0;
                         false -> TotalPoints / (ElapsedMinutes * 60)
                     end,

    io:format("=== Stress Test Stats (Time: ~.1f min) ===~n", [ElapsedMinutes]),
    io:format("Total Points Written: ~p~n", [TotalPoints]),
    io:format("Total Batches: ~p~n", [TotalBatches]),
    io:format("Total Errors: ~p~n", [TotalErrors]),
    io:format("Points/Second: ~.1f~n", [PointsPerSecond]),
    io:format("Avg Write Time: ~.1f ms~n", [AvgWriteTime]),
    io:format("Last Write Time: ~p ms~n", [LastWriteTime]),
    io:format("Last Batch Size: ~p~n", [LastAffectedRows]),
    io:format("========================================~n").

stress_loop(Client, Metric, StartTime, Stats) ->
    ElapsedMs = erlang:monotonic_time(millisecond) - StartTime,
    ElapsedMinutes = ElapsedMs / (60 * 1000),

    if ElapsedMinutes >= ?TEST_DURATION_MINUTES ->
           print_stats(Stats, ElapsedMinutes),
           io:format("Stress test completed after ~.1f minutes~n", [ElapsedMinutes]),
           ok;
       true ->
           BatchStartTime = erlang:monotonic_time(millisecond),
           CurrentTimestamp = get_current_timestamp_ms(),

           {WriteResult, NewStats} = write_batch(Client, Metric, CurrentTimestamp, ?BATCH_SIZE, Stats),

           BatchEndTime = erlang:monotonic_time(millisecond),
           BatchDuration = BatchEndTime - BatchStartTime,

           case WriteResult of
               ok ->
                   SleepTime = max(0, ?BATCH_INTERVAL_MS - BatchDuration),
                   timer:sleep(SleepTime);
               error ->
                   timer:sleep(1000)
           end,

           TotalBatches = maps:get(total_batches, NewStats, 0),
           if TotalBatches rem 100 =:= 0 ->
                  print_stats(NewStats, ElapsedMinutes);
              true ->
                  ok
           end,

           stress_loop(Client, Metric, StartTime, NewStats)
    end.

start_client() ->
    Host = greptime_host(),
    Options = [
        {endpoints, [{http, Host, 4001}]},
        {pool, stress_test_pool},
        {pool_size, 8},
        {pool_type, random},
        {timeunit, ms},
        {auth, {basic, #{username => ?GREPTIME_USERNAME, password => ?GREPTIME_PASSWORD}}}
    ],

    case greptimedb:start_client(Options) of
        {ok, Client} ->
            case greptimedb:is_alive(Client) of
                true ->
                    io:format("Connected to GreptimeDB at ~s:4001~n", [Host]),
                    {ok, Client};
                false ->
                    io:format("Failed to connect to GreptimeDB~n"),
                    {error, connection_failed}
            end;
        {error, Reason} ->
            io:format("Failed to start client: ~p~n", [Reason]),
            {error, Reason}
    end.

test_single_write() ->
    application:ensure_all_started(greptimedb),
    case start_client() of
        {ok, Client} ->
            Metric = <<"test_simple">>,
            Points = [#{fields => #{<<"temperature">> => 25.5},
                       tags => #{<<"device_id">> => <<"dev_001">>},
                       timestamp => get_current_timestamp_ms()}],

            io:format("Testing single write...~n"),
            Result = greptimedb:write(Client, Metric, Points),
            io:format("Result: ~p~n", [Result]),
            greptimedb:stop_client(Client);
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end.

run() ->
    run(<<"stress_test_metrics">>).

run(Metric) ->
    application:ensure_all_started(greptimedb),

    io:format("Starting GreptimeDB stress test...~n"),
    io:format("Duration: ~p minutes~n", [?TEST_DURATION_MINUTES]),
    io:format("Target Rate: ~p rows/second~n", [?ROWS_PER_SECOND]),
    io:format("Batch Size: ~p~n", [?BATCH_SIZE]),
    io:format("Batch Interval: ~p ms~n", [?BATCH_INTERVAL_MS]),
    io:format("Metric: ~p~n", [Metric]),

    case start_client() of
        {ok, Client} ->
            StartTime = erlang:monotonic_time(millisecond),
            Stats = #{},

            try
                stress_loop(Client, Metric, StartTime, Stats)
            catch
                Error:Reason:Stacktrace ->
                    io:format("Error during stress test: ~p:~p~n", [Error, Reason]),
                    io:format("Stacktrace: ~p~n", [Stacktrace])
            after
                greptimedb:stop_client(Client),
                io:format("Client stopped~n")
            end;
        {error, _} ->
            io:format("Cannot start stress test without database connection~n"),
            error
    end.
