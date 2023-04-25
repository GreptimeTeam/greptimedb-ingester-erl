greptimedb-client-erl
=====
![Tests](https://github.com/GreptimeTeam/greptimedb-client-erl/workflows/Erlang%20CI/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/GreptimeTeam/greptimedb-client-erl/badge.svg?branch=)](https://coveralls.io/github/GreptimeTeam/greptimedb-client-erl?branch=)

A client library for GreptimeDB

Usage
-----
Start the application:

```erlang
    application:ensure_all_started(greptimedb).
```

Start the client:

```erlang
    Options =
      [{endpoints, [{http, "localhost", 4001}]},
       {pool, greptimedb_client_pool},
       {pool_size, 8},
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
                 <<"qos">> => #{values => #{i64_values => [0]}, datatype => 'INT64'},
                 <<"region">> => <<"hangzhou">>},
           timestamp => 1619775142098},
         #{fields => #{<<"temperature">> => 2},
           tags =>
               #{<<"from">> => <<"mqttx_4b963a8e">>,
                 <<"host">> => <<"serverB">>,
                 <<"region">> => <<"ningbo">>,
                 <<"to">> => <<"kafka">>},
           timestamp => 1619775143098}],

    {ok, #{response := {affected_rows, #{value := 2}}}} =
        greptimedb:write(Client, Metric, Points).
```

Stop the client:

```erlang
    greptimedb:stop_client(Client).
```

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 do ct,eunit
