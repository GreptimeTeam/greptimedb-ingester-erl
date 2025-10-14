# GreptimeDB Erlang Client

![Tests](https://github.com/GreptimeTeam/greptimedb-client-erl/workflows/Erlang%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/GreptimeTeam/greptimedb-client-erl/badge.svg?branch=main)](https://coveralls.io/github/GreptimeTeam/greptimedb-client-erl?branch=main)

An Erlang client library for [GreptimeDB](https://github.com/GreptimeTeam/greptimedb).

## Table of Contents

- [Quick Start](#quick-start)
- [Writing Data](#writing-data)
  - [Basic Write](#basic-write)
  - [Custom Time Unit](#custom-time-unit)
  - [Custom Timestamp Column](#custom-timestamp-column)
  - [Batch Write](#batch-write)
  - [Async Write](#async-write)
  - [Streaming Write](#streaming-write)
- [Connection Options](#connection-options)
  - [Client Options](#client-options)
  - [Authentication](#authentication)
  - [GreptimeCloud](#greptimecloud)
- [Data Types](#data-types)
- [Development](#development)
  - [Build](#build)
  - [Testing](#testing)
- [Performance](#performance)

## Quick Start

Start the application:

```erlang
application:ensure_all_started(greptimedb).
```

Create a client:

```erlang
Options = [
    {endpoints, [{http, "localhost", 4001}]},
    {pool, greptimedb_client_pool},
    {pool_size, 5},
    {pool_type, random},
    {timeunit, ms}
],
{ok, Client} = greptimedb:start_client(Options).
```

Stop the client:

```erlang
greptimedb:stop_client(Client).
```

Check if the client is alive:

```erlang
true = greptimedb:is_alive(Client).
```

## Writing Data

### Basic Write

Write data points to GreptimeDB:

```erlang
Metric = <<"temperatures">>,
Points = [
    #{fields => #{<<"temperature">> => 25.5},
      tags => #{<<"host">> => <<"serverA">>, <<"region">> => <<"hangzhou">>},
      timestamp => 1619775142098},
    #{fields => #{<<"temperature">> => 27.0},
      tags => #{<<"host">> => <<"serverB">>, <<"region">> => <<"ningbo">>},
      timestamp => 1619775143098}
],

{ok, #{response := {affected_rows, #{value := 2}}}} =
    greptimedb:write(Client, Metric, Points).
```

### Custom Time Unit

You can specify time unit globally when creating the client, or per-metric when writing data.

**Global time unit configuration:**

```erlang
Options = [
    {endpoints, [{http, "localhost", 4001}]},
    {pool, greptimedb_client_pool},
    {pool_size, 5},
    {timeunit, nanosecond}  % All timestamps will use nanosecond by default
],
{ok, Client} = greptimedb:start_client(Options),

Points = [
    #{fields => #{<<"temperature">> => 25.5},
      tags => #{<<"host">> => <<"serverA">>},
      timestamp => 1705946037724448346}  % nanosecond timestamp
],

greptimedb:write(Client, <<"temperatures">>, Points).
```

**Per-metric time unit override:**

```erlang
% Override global timeunit for specific metric
Metric = #{table => <<"temperatures_millisec">>, timeunit => millisecond},
Points = [
    #{fields => #{<<"temperature">> => 25.5},
      tags => #{<<"host">> => <<"serverA">>},
      timestamp => 1619775142098}  % millisecond timestamp
],

{ok, #{response := {affected_rows, #{value := 1}}}} =
    greptimedb:write(Client, Metric, Points).
```

### Custom Timestamp Column

By default, GreptimeDB uses `greptime_timestamp` as the timestamp column name. You can customize this:

```erlang
Options = [
    {endpoints, [{http, "localhost", 4001}]},
    {pool, greptimedb_client_pool},
    {pool_size, 5},
    {ts_column, <<"event_time">>}  % Custom timestamp column name
],
{ok, Client} = greptimedb:start_client(Options).

% Now the timestamp column will be named 'event_time' instead of 'greptime_timestamp'
Points = [
    #{fields => #{<<"temperature">> => 25.5},
      tags => #{<<"sensor_id">> => <<"sensor_001">>},
      timestamp => 1619775142098}
],

greptimedb:write(Client, <<"sensors">>, Points).
```

When you query the data, use your custom column name:

```sql
SELECT event_time, temperature, sensor_id FROM sensors ORDER BY event_time;
```

### Batch Write

Write multiple metrics in a single request:

```erlang
Metric1 = <<"temperatures">>,
Points1 = [...],
Metric2 = <<"humidities">>,
Points2 = [...],
Batch = [{Metric1, Points1}, {Metric2, Points2}],

{ok, _} = greptimedb:write_batch(Client, Batch).
```

### Async Write

Write data asynchronously with callbacks:

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

Batch async write:

```erlang
Batch = [...],
Ref = make_ref(),
Pid = self(),
ResultCallback = {fun(Reply) -> Pid ! {{Ref, reply}, Reply} end, []},

ok = greptimedb:async_write_batch(Client, Batch, ResultCallback),
receive
    {{Ref, reply}, Reply} ->
        io:format("Reply ~w~n", [Reply])
end.
```

### Streaming Write

For high-throughput scenarios:

```erlang
Points1 = [...],
Points2 = [...],

{ok, Stream} = greptimedb:write_stream(Client),
greptimedb_stream:write(Stream, "Metric1", Points1),
greptimedb_stream:write(Stream, "Metric2", Points2),
{ok, _} = greptimedb_stream:finish(Stream).
```

## Connection Options

### Client Options

Available client options:

* **`endpoints`**: List of GreptimeDB server addresses in the form `{http|https, host, port}`
* **`pool`, `pool_size`**: Client pool settings
* **`grpc_opts`**: grpcbox [client options](https://github.com/tsloughter/grpcbox#defining-channels)
* **`grpc_hints`**: Map for GreptimeDB gRPC insertion hints:
    * `append_mode`: `<<"true">>` or `<<"false">>` (default `<<"false">>`)
    * `ttl`: Time to live, e.g., `<<"7 days">>`
    * `merge_mode`: `<<"last_row">>` or `<<"last_non_null">>` (default `<<"last_row">>`)
    * `auto_create_table`: `<<"true">>` or `<<"false">>` (default `<<"true">>`)
    * More about [table options](https://docs.greptime.com/reference/sql/create/#table-options)
* **`ssl_opts`**: SSL options for HTTPS endpoints (default `[]`)
* **`auth`**: Authentication options (see [Authentication](#authentication))
* **`timeunit`**: Default timestamp unit:
    * `ns` or `nanosecond`
    * `us` or `microsecond`
    * `ms` or `millisecond` (default)
    * `s` or `second`
* **`dbname`**: Default database name (default `<<"public">>`)
* **`ts_column`**: Custom timestamp column name (default `<<"greptime_timestamp">>`)

Example with all options:

```erlang
Options = [
    {endpoints, [{http, "localhost", 4001}]},
    {pool, greptimedb_client_pool},
    {pool_size, 10},
    {pool_type, random},
    {timeunit, ms},
    {dbname, <<"my_database">>},
    {ts_column, <<"event_time">>},
    {grpc_hints, #{
        <<"append_mode">> => <<"true">>,
        <<"ttl">> => <<"30 days">>,
        <<"auto_create_table">> => <<"true">>
    }},
    {auth, {basic, #{username => <<"user">>, password => <<"pass">>}}}
],
{ok, Client} = greptimedb:start_client(Options).
```

### Authentication

Connect with authentication:

```erlang
Options = [
    {endpoints, [{http, "localhost", 4001}]},
    {pool, greptimedb_client_pool},
    {pool_size, 5},
    {auth, {basic, #{username => <<"greptime_user">>, password => <<"greptime_pwd">>}}}
],
{ok, Client} = greptimedb:start_client(Options).
```

### GreptimeCloud

[GreptimeCloud](https://greptime.com/product/cloud) is a fully-managed GreptimeDB service.

After creating a service, you'll need:
* **Host**: Service hostname
* **Port**: gRPC port (usually `5001`)
* **Database**: Database name
* **Username**: Service username
* **Password**: Service password

```erlang
Host = <<"your-host.greptime.cloud">>,
Database = <<"your_database">>,
Username = <<"your_username">>,
Password = <<"your_password">>,

Options = [
    {endpoints, [{https, Host, 5001}]},  % Note: https for cloud
    {pool, greptimedb_client_pool},
    {pool_size, 5},
    {pool_type, random},
    {timeunit, ms},
    {dbname, Database},
    {auth, {basic, #{username => Username, password => Password}}}
],

{ok, Client} = greptimedb:start_client(Options),

Metric = <<"temperatures">>,
Points = [...],
greptimedb:write(Client, Metric, Points).
```

## Data Types

### Default Types

* **`fields`**: Metric values (default type: `FLOAT64`)
* **`tags`**: Metric metadata (default type: `STRING`)
* **`timestamp`**: Time information (default type: `TIMESTAMP_MILLISECOND`)

### Custom Types

Use functions from `greptimedb_values` module for specific types:

```erlang
Points = [
    #{fields => #{
        <<"temperature">> => 25.5,  % FLOAT64 (default)
        <<"pressure">> => greptimedb_values:int32_value(1013),
        <<"active">> => greptimedb_values:boolean_value(true)
      },
      tags => #{
        <<"location">> => <<"room1">>,  % STRING (default)
        <<"sensor_id">> => greptimedb_values:int64_value(12345)
      },
      timestamp => 1619775142098}
].
```

Available type functions in `greptimedb_values`:
* `int32_value/1`, `int64_value/1`
* `uint32_value/1`, `uint64_value/1`
* `float64_value/1`, `boolean_value/1`
* `binary_value/1`, `string_value/1`
* `date_value/1`, `datetime_value/1`
* `timestamp_second_value/1`, `timestamp_millisecond_value/1`
* `timestamp_microsecond_value/1`, `timestamp_nanosecond_value/1`

## Development

### Build

```bash
rebar3 compile
```

### Testing

Start GreptimeDB:

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

Run tests:

```bash
rebar3 do ct,eunit
```

## Performance

Benchmark results on local machine and db:

```
OS: Darwin MacBook-Pro.local 24.6.0 Darwin Kernel Version 24.6.0
Chip: Apple M4 Max
```

```
Finish benchmark:
  series: 5000, 
  batch size: 100,
  concurrency: 10, 
  cost: 65 seconds,
  rows: 10000000,
  TPS: 153846.15384615384 (rows/second)
```
