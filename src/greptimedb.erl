%% Copyright 2023 Greptime Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(greptimedb).

-export([start_client/1, stop_client/1, write_batch/2, write/3, write_stream/1,
         async_write/4, async_write_batch/3, is_alive/1, is_alive/2, ddl/1]).

-export_type([metric/0, point/0, timeunit/0]).

-type table() :: atom() | binary() | list().
-type dbname() :: atom() | binary() | list().
-type timeunit() :: ns | us| ms | s | nanosecond | microsecond | millisecond | second.
-type metric() :: table()
                | {dbname(), table()}
                | #{dbname => dbname(), table := table(), timeunit => timeunit()}.
-type point() :: #{tags => map(),
                   fields => map(),
                   timestamp => integer()}.

-spec start_client(list()) ->
                      {ok, Client :: map()} |
                      {error, {already_started, Client :: map()}} |
                      {error, Reason :: term()}.

start_client(Options0) ->
    Pool = proplists:get_value(pool, Options0),
    Options = lists:keydelete(protocol, 1, Options0),

    Client =
        #{pool => Pool,
          protocol => http,
          cli_opts => Options},
    case ecpool:start_sup_pool(Pool, greptimedb_worker, Options) of
        {ok, _} ->
            {ok, Client};
        {error, {already_started, _}} ->
            {error, {already_started, Client}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Write points to the metric table, return the result.
-spec write(Client, Metric, Points) -> {ok, term()} | {error, term()}
    when Client :: map(),
         Metric :: metric(),
         Points :: [point()].
write(Client, Metric, Points) ->
    write_batch(Client, [{Metric, Points}]).

%% @doc Write a batch of data points to the database, return the result.
-spec write_batch(Client, MetricAndPoints) -> {ok, term()} | {error, term()}
    when Client :: map(),
         MetricAndPoints :: [{metric(), [point()]}].
write_batch(Client, MetricAndPoints) ->
    try
        Request = greptimedb_encoder:insert_requests(Client, MetricAndPoints),
        handle(Client, Request)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] write ~0p failed: ~0p ~0p ~p", [MetricAndPoints, E, R, S]),
            {error, R}
    end.

%% @doc Create a gRPC stream to write data, return the stream or an error.
-spec write_stream(Client) -> {ok, term()} | {error, term()} when Client :: map().
write_stream(Client) ->
    try
        rpc_write_stream(Client)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] create write stream failed: ~0p ~0p ~p", [E, R, S]),
            {error, R}
    end.

%% @doc Send an async request to write points to the metric table. The callback is evaluated when an error happens or response is received.
-spec async_write(Client, Metric, Points, ResultCallback) -> ok | {error, term()}
    when Client :: map(),
         Metric :: metric(),
         Points :: [point()],
         ResultCallback :: {function(), list()}.
async_write(Client, Metric, Points, ResultCallback) ->
    async_write_batch(Client, [{Metric, Points}], ResultCallback).

%% @doc Send a batch of async request. The callback is evaluated when an error happens or response is received.
-spec async_write_batch(Client, MetricAndPoints, ResultCallback) -> ok | {error, term()}
    when Client :: map(),
         MetricAndPoints ::  [{metric(), [point()]}],
         ResultCallback :: {function(), list()}.
async_write_batch(Client, MetricAndPoints, ResultCallback) ->
    Request = greptimedb_encoder:insert_requests(Client, MetricAndPoints),
    async_handle(Client, Request, ResultCallback).

ddl(_Client) ->
    todo.

-spec is_alive(Client :: map()) -> true | false.
is_alive(Client) ->
    is_alive(Client, false).

-spec is_alive(Client :: map(), ReturnReason :: boolean()) ->
                  true | false | {false, Reason :: term()}.
is_alive(Client, ReturnReason) ->
    try
        case health_check(Client) of
            {ok, _Resp} ->
                true;
            Return ->
                maybe_return_reason(Return, ReturnReason)
        end
    catch
        E:R:S ->
            logger:error("[GreptimeDB] health check failed: ~0p ~0p ~p", [E, R, S]),
            maybe_return_reason({error, R}, ReturnReason)
    end.

-spec stop_client(Client :: map()) -> ok | term().
stop_client(#{pool := Pool}) ->
    ecpool:stop_sup_pool(Pool).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle(#{pool := Pool} = _Client, Request) ->
    Fun = fun(Worker) -> greptimedb_worker:handle(Worker, Request) end,
    try
        ecpool:with_client(Pool, Fun)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] grpc handle fail: ~0p ~0p ~0p", [E, R, S]),
            {error, {E, R}}
    end.

async_handle(#{pool := Pool} = _Client, Request, ResultCallback) ->
    Fun = fun(Worker) -> greptimedb_worker:async_handle(Worker, Request, ResultCallback) end,
    try
        ecpool:with_client(Pool, Fun)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] grpc async_handle fail: ~0p ~0p ~0p", [E, R, S]),
            {error, {E, R}}
    end.

health_check(#{pool := Pool} = _Client) ->
    Fun = fun(Worker) -> greptimedb_worker:health_check(Worker) end,
    try
        ecpool:with_client(Pool, Fun)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] grpc health check failed: ~0p ~0p ~0p", [E, R, S]),
            {error, {E, R}}
    end.

rpc_write_stream(#{pool := Pool, cli_opts := Options} = _Client) ->
    Fun = fun(Worker) ->
             case greptimedb_worker:stream(Worker) of
                 {ok, S} ->
                     {ok, S#{cli_opts => Options}};
                 Other ->
                     Other
             end
          end,
    try
        ecpool:with_client(Pool, Fun)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] grpc write fail: ~0p ~0p ~0p", [E, R, S]),
            {error, {E, R}}
    end.

maybe_return_reason({error, Reason}, true) ->
    {false, Reason};
maybe_return_reason(Error, true) ->
    {false, Error};
maybe_return_reason(_, _) ->
    false.
