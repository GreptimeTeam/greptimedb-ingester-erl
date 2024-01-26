greptimedb-client-erl
=====
![Tests](https://github.com/GreptimeTeam/greptimedb-client-erl/workflows/Erlang%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/GreptimeTeam/greptimedb-client-erl/badge.svg?branch=main)](https://coveralls.io/github/GreptimeTeam/greptimedb-client-erl?branch=main)

An Erlang client library for [GreptimeDB](https://github.com/GreptimeTeam/greptimedb).

> **_NOTE:_** GreptimeDB and this project is under heavy development. Do not use it in production at the moment.
>  0.1.0: only working for GreptimeDB 0.2, otherwise for the latest GreptimeDB releases.

## Usage

Start the application:

```erlang
    application:ensure_all_started(greptimedb).
```

Start the client:

```erlang
    Options =
      [{endpoints, [{http, "localhost", 4001}]},
       {pool, greptimedb_client_pool},
       {pool_size, 5},
       {pool_type, random},
       {timeunit, ms}].
    {ok, Client} = greptimedb:start_client(Options).
```

Write data by rows:

```erlang
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
        greptimedb:write(Client, Metric, Points).
```

Write data specifying `timeunit`:

```erlang
   Metric = #{table => <<"temperatures_nanosec">>,
              timeunit => nanosecond},
   Points =
        [#{fields => #{<<"temperature">> => 1},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverA">>,
                 <<"qos">> => greptimedb_values:int64_value(0),
                 <<"region">> => <<"hangzhou">>},
           timestamp => 1705946037724448346}],

    {ok, #{response := {affected_rows, #{value := 1}}}} =
        greptimedb:write(Client, Metric, Points).
```

Write in async:

```erlang
Ref = make_ref(),
Pid = self(),
ResultCallback = {fun(Reply) -> Pid ! {{Ref, reply}, Reply} end, []},

ok = greptimedb:async_write(Client, Metric, Points, ResultCallback),
receive
    {{Ref, reply}, Reply} ->
        io:format("Reply ~w~n", [Reply])
end.
```

Batch write:

```erlang
Metric1 = <<"temperatures">>,
Points1 = [...],
Metric2 = <<"humidities">>,
Points2 = [...],
Batch = [{Metric1, Points1}, {Metric2, Points}],

{ok, _} = greptimedb:write_batch(Client, Batch).
```

Batch write in async:

```erlang
Batch = ...,
Ref = make_ref(),
Pid = self(),
ResultCallback = {fun(Reply) -> Pid ! {{Ref, reply}, Reply} end, []},

ok = greptimedb:async_write_batch(Client, Batch, ResultCallback),
receive
    {{Ref, reply}, Reply} ->
        io:format("Reply ~w~n", [Reply])
end.
```

Streaming write:

```erlang
    Points1 = [ ... ],
    Points2 = [ ... ],

    {ok, Stream} = greptimedb:write_stream(Client),
    greptimedb_stream:write(Stream, "Metric1", Points1),
    greptimedb_stream:write(Stream, "Metric2", Points2),
    {ok, _} = greptimedb_stream:finish(Stream).
```

Stop the client:

```erlang
    greptimedb:stop_client(Client).
```

Connect GreptimeDB with authentication:

```erlang
    Options =
      [{endpoints, [{http, "localhost", 4001}]},
       {pool, greptimedb_client_pool},
       {pool_size, 5},
       {pool_type, random},
       {timeunit, ms},
       {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}].
    {ok, Client} = greptimedb:start_client(Options).
```

## APIs guide

### Client options

A proper list contains:

* `endpoints`: List of the GreptimeDB server address in the form of `{http, host, port}`
* `pool`, `pool_size` etc.: the client pool settings
* `gprc_options`: grpxbox [client options](https://github.com/tsloughter/grpcbox#defining-channels)
* `auth`:  authentication options,  `{auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}` for example.
* `timeunit`: Timestamp unit, supports:
    * `ns` or `nanosecond`
    * `us` or `microsecond`
    * `ms` or `millisecond`
    * `s` or `second`

### Write and datatypes
The metric name can be a string or binary. If you want to set the database, the metric name can be set in the form of `{dbname, metric}`. The data will be written into `greptime-public` by default.

Write each row by `greptimedb:write/3` function. Every row contains:

* `fields`: the metric fields, the default type is `FLOAT64`.
* `tags`: the metric tags, the default type is `STRING`.
* `timestamp`: the metric timestamp, the default type is `TIMESTAMP_MILLISECOND`.

Of course, you can write other types by using functions in [greptimedb_values](https://github.com/GreptimeTeam/greptimedb-client-erl/blob/main/src/greptimedb_values.erl).


## Build and test

Build:

    $ rebar3 compile

Test:

    $ rebar3 do ct,eunit

## Performance

```
Finish benchmark,
  series: 5000,
  concurrency: 10,
  cost: 48 seconds,
  rows: 10000000,
  TPS: 208333.33333333334
```
