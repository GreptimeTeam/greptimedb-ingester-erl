greptimedb-client-erl
=====
![Tests](https://github.com/GreptimeTeam/greptimedb-client-erl/workflows/Erlang%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/GreptimeTeam/greptimedb-client-erl/badge.svg?branch=main)](https://coveralls.io/github/GreptimeTeam/greptimedb-client-erl?branch=main)

An Erlang client library for [GreptimeDB](https://github.com/GreptimeTeam/greptimedb).

> **_NOTE:_** GreptimeDB and this project is under heavy development. Do not use it in production at the moment.

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
       {pool_type, random}].
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

### Write and datatypes

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
