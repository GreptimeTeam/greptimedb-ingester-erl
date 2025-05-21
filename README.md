greptimedb-ingester-erl
=====
![Tests](https://github.com/GreptimeTeam/greptimedb-client-erl/workflows/Erlang%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/GreptimeTeam/greptimedb-client-erl/badge.svg?branch=main)](https://coveralls.io/github/GreptimeTeam/greptimedb-client-erl?branch=main)

An Erlang client library for [GreptimeDB](https://github.com/GreptimeTeam/greptimedb).

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
       {grpc_hints, #{}},
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

Check if the client is alive:

```erlang
    true = greptimedb:is_alive(Client),
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

## Write to GreptimeCloud

[GreptimeCloud](https://greptime.com/product/cloud) is a fully-managed GreptimeDB as a service in the cloud.

After you creating a service, you must have the following info via `connect`:
* `Host` the service host to connect,
* `Port`, gRPC port, default is `5001`,
* `Database`, the database to write,
* `Username`, the service username,
* `Password`, the service password.

Connect to GreptimeCloud with authentication:

```erlang
  Host = ...,
  Database = ...,
  Username = ...,
  Password = ...,

  Options =
      [{endpoints, [{https, Host, 5001}]},
       {pool, greptimedb_client_pool},
       {pool_size, 5},
       {pool_type, random},
       {timeunit, ms},
       {dbname, Database},
       {auth, {basic, #{username => Username, password => Password }}}],

  {ok, Client} = greptimedb:start_client(Options),

  Metric = <"temperatures">>,
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

  greptimedb:write(Client, Metric, Points).

```

We change the endpoint scheme from `http` to `https` and set the `dbname` option.

## APIs guide

### Client options

A proper list contains:

* `endpoints`: List of the GreptimeDB server address in the form of `{http, host, port}`
* `pool`, `pool_size` etc.: the client pool settings
* `grpc_opts`: grpxbox [client options](https://github.com/tsloughter/grpcbox#defining-channels)
* `grpc_hints`: a map for GreptimeDB gRPC insertion hints, for example`#{ <<"append_mode">> => <<"true">> }` to enable append mode when creating tables automatically. Valid hints include:
    * `append_mode`: `true` or `false` to enable append mode, default is `false`,
    * `ttl`: time to live, the table `ttl` option,
    * `merge_mode`:  `last_row` or `last_non_null`, default is `last_row`,
    * `auto_create_table`: `true` or `false`, whether to create tables automatically when writing data, default is `false`,
    * More about these table options, please read the [doc](https://docs.greptime.com/reference/sql/create/#table-options).
* `ssl_opts`: when the endpoint scheme is `https`, the ssl options to use(`[]` by default).
* `auth`:  authentication options,  `{auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}` for example.
* `timeunit`: Timestamp unit, supports:
    * `ns` or `nanosecond`
    * `us` or `microsecond`
    * `ms` or `millisecond`
    * `s` or `second`
* `dbname`: the default database to write, `public` by default. Change it to the servce database name when connecting to GreptimeCloud.

### Write and datatypes
The metric name can be a string or binary. If you want to set the database, the metric name can be set in the form of `{dbname, metric}`. The data will be written into `greptime-public` by default.

Write each row by `greptimedb:write/3` function. Every row contains:

* `fields`: the metric fields, the default type is `FLOAT64`.
* `tags`: the metric tags, the default type is `STRING`.
* `timestamp`: the metric timestamp, the default type is `TIMESTAMP_MILLISECOND`.

Of course, you can write other types by using functions in [greptimedb_values](https://github.com/GreptimeTeam/greptimedb-ingester-erl/blob/main/src/greptimedb_values.erl).


## Build and test

Build:
```bash
rebar3 compile
```

Test:

Start the GreptimeDB:

```bash
docker run -p 127.0.0.1:4000-4003:4000-4003 \
  -v "$(pwd)/greptimedb:/greptimedb_data" \
  --name greptime --rm \
  greptime/greptimedb:latest standalone start \
  --http-addr 0.0.0.0:4000 \
  --rpc-bind-addr 0.0.0.0:4001 \
  --mysql-addr 0.0.0.0:4002 \
  --postgres-addr 0.0.0.0:4003 \
  --user-provider=static_user_provider:cmd:greptime_user=greptime_pwd
```

Then run the tests:
```bash
rebar3 do ct,eunit
```

## Performance

```
Finish benchmark,
  series: 5000,
  concurrency: 10,
  cost: 48 seconds,
  rows: 10000000,
  TPS: 208333.33333333334
```
