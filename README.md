# GreptimeDB Erlang Client

![Tests](https://github.com/GreptimeTeam/greptimedb-ingester-erl/workflows/Erlang%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/GreptimeTeam/greptimedb-ingester-erl/badge.svg?branch=main)](https://coveralls.io/github/GreptimeTeam/greptimedb-ingester-erl?branch=main)

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

Write multiple metrics in a single request, using `Points` from the basic write example:

```erlang
Batch = [{<<"temperatures_a">>, Points}, {<<"temperatures_b">>, Points}],

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

Batch async write, using `Points` from the basic write example:

```erlang
Batch = [{<<"temperatures_a">>, Points}, {<<"temperatures_b">>, Points}],
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

Write multiple requests through one stream, using `Points` from the basic write example:

```erlang
{ok, Stream} = greptimedb:write_stream(Client),
ok = greptimedb_stream:write(Stream, <<"temperatures_a">>, Points),
ok = greptimedb_stream:write(Stream, <<"temperatures_b">>, Points),
{ok, _} = greptimedb_stream:finish(Stream).
```

`request_timeout` sets the gRPC deadline when the stream opens. `finish/1`
waits up to `request_timeout + 2000` milliseconds for the result. To set a
separate wait timeout, use `finish/2` instead of `finish/1`:

```erlang
{ok, _} = greptimedb_stream:finish(Stream, 5000).
```

The wait timeout does not extend the stream's gRPC deadline. `grpc_hints` also
apply to tables created through streaming writes.

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
* **`connect_timeout`**: Milliseconds to wait for the TCP connection to an endpoint (default `5000`)
* **`request_timeout`**: Milliseconds a write may take, including streaming writes and
  `async_write`, where time spent waiting in the batching queue counts against it (default `10000`)
* **`health_check_timeout`**: gRPC deadline in milliseconds for `is_alive` (default `10000`)
* **`tcp_user_timeout`**: Milliseconds unacknowledged data may stay outstanding before the
  kernel drops the connection, set as `TCP_USER_TIMEOUT` on the socket (default `0`, disabled).
  Supported on Linux; leave `0` on other platforms. This option does not enable
  TCP keepalive or HTTP/2 pings.
* **`ssl_opts`**: SSL options for HTTPS endpoints (default `[]`)
* **`auth`**: Authentication options (see [Authentication](#authentication))
* **`timeunit`**: Default timestamp unit:
    * `ns` or `nanosecond`
    * `us` or `microsecond`
    * `ms` or `millisecond` (default)
    * `s` or `second`
* **`dbname`**: Default database name (default `<<"public">>`)
* **`ts_column`**: Custom timestamp column name (default `<<"greptime_timestamp">>`)

`request_timeout` and `health_check_timeout` are sent with each request, so they
apply to the client that made it. `connect_timeout` and `tcp_user_timeout` belong
to the connection and are fixed when the pool starts: a client that reuses a
running pool, which `start_client/1` reports as `{error, {already_started, Client}}`,
keeps the values that pool was started with.

Example with connection and write options:

```erlang
Options = [
    {endpoints, [{http, "localhost", 4001}]},
    {pool, greptimedb_client_pool},
    {pool_size, 10},
    {pool_type, random},
    {timeunit, ms},
    {dbname, <<"my_database">>},
    {ts_column, <<"event_time">>},
    {connect_timeout, 5000},
    {request_timeout, 10000},
    {health_check_timeout, 10000},
    {tcp_user_timeout, 30000},
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
* `float32_value/1`, `float64_value/1`, `boolean_value/1`
* `binary_value/1`, `string_value/1`
* `date_value/1`, `datetime_value/1`
* `timestamp_second_value/1`, `timestamp_millisecond_value/1`
* `timestamp_microsecond_value/1`, `timestamp_nanosecond_value/1`
* `decimal128_value/4` — `decimal128_value(Hi, Lo, Precision, Scale)`
* `json_value/1`, `json2_value/1`

`DECIMAL128` is stored as a 128-bit signed integer split into two `int64`
halves. To reconstruct the logical value, mask each half to an unsigned
64-bit chunk, combine into a 128-bit pattern, then reinterpret as signed
two's-complement (Erlang integers are arbitrary-precision, so a raw `bor`
with a negative `Lo` sign-extends and gives the wrong result):

```erlang
Mask = (1 bsl 64) - 1,
Raw  = ((Hi band Mask) bsl 64) bor (Lo band Mask),
Int128 = if Raw >= (1 bsl 127) -> Raw - (1 bsl 128); true -> Raw end,
Value = Int128 / math:pow(10, Scale).   %% float result; use a bignum/decimal lib for exact precision
```

For example, `123.45` with precision 10 and scale 2:

```erlang
greptimedb_values:decimal128_value(0, 12345, 10, 2).
```

#### JSON and JSON2

`json_value/1` writes a legacy `JSON` column. It takes encoded JSON text
(binary or iodata). The server parses the text and rejects the request if it
is not valid JSON.

`json2_value/1` writes a `JSON2` column. It takes a decoded JSON term, in the
representation returned by OTP's `json:decode/1` or `jsx:decode(Bin, [return_maps])`:

| JSON | Erlang |
|------|--------|
| object | map with binary keys |
| array | list |
| string | binary |
| number | integer or float |
| `true` / `false` / `null` | `true` / `false` / `null` |

Constraints for `json2_value/1`:

* The top-level value must be a map. To write SQL NULL, omit the field from the point.
* JSON2 values are only supported in fields. A JSON2 tag raises
  `{json2_tag_not_supported, #{column := Name}}`.
* Integers must be in the int64 or uint64 range.
* Other terms raise `{invalid_json2_value, #{reason := Reason, value := Term}}`.
* A table with a `JSON2` column must be append-only. When the table is created
  on insertion, set `{grpc_hints, #{<<"append_mode">> => <<"true">>}}`.

```erlang
Points = [
    #{fields => #{
        <<"attrs">> => greptimedb_values:json_value(<<"{\"region\":\"eu\"}">>),
        <<"payload">> => greptimedb_values:json2_value(#{<<"user">> => <<"alice">>,
                                                         <<"tags">> => [<<"a">>, <<"b">>],
                                                         <<"score">> => 9.5})
      },
      tags => #{<<"host">> => <<"h1">>},
      timestamp => 1619775142098}
].
```

Within a batch, a column's type is fixed by the first point that contains it.
A raw value in a `JSON` or `JSON2` column raises
`{json_requires_typed_value, #{column := Name, value := Value}}`. A JSON value
in a column of another type raises `{value_schema_mismatch, #{column := Name, ...}}`.

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
  greptime/greptimedb:v1.2.1 standalone start \
  --http-addr 0.0.0.0:4000 \
  --grpc-bind-addr 0.0.0.0:4001 \
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
