-module(greptimedb_stream).

-export([write/3, write_batch/2, write_request/2, finish/1, finish/2]).

%% @doc write the points of the metric to the gRPC stream, returns the result.
-spec write(Stream, Metric, Points) -> {ok, term()} | {error, term()}
    when Stream :: map(),
         Metric :: Table | {DbName, Table},
         DbName :: atom() | binary() | list(),
         Table :: atom() | binary() | list(),
         Points :: [Point],
         Point ::
             #{tags => map(),
               fields => map(),
               timestamp => integer()}.
write(Stream, Metric, Points) ->
    write_batch(Stream, [{Metric, Points}]).

%% @doc Write a batch of data points to the gRPC stream, return the result.
-spec write_batch(Stream, MetricAndPoints) -> {ok, term()} | {error, term()}
    when Stream :: map(),
         MetricAndPoints :: [MetricAndPoint],
         MetricAndPoint :: {Metric, Points},
         Metric :: Table | {DbName, Table},
         DbName :: atom() | binary() | list(),
         Table :: atom() | binary() | list(),
         Points :: [Point],
         Point ::
             #{tags => map(),
               fields => map(),
               timestamp => integer()}.
write_batch(Stream, MetricAndPoints) ->
    Request = greptimedb_encoder:insert_requests(Stream, MetricAndPoints),
    write_request(Stream, Request).

write_request(Stream, Request) ->
    try
        grpcbox_client:send(Stream, Request)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] stream write ~0p failed: ~0p ~0p ~p", [Request, E, R, S]),
            {error, R}
    end.

%% @doc Finish the gRPC stream and wait the result, past the client's
%% `request_timeout' so the gRPC deadline error surfaces first.
-spec finish(Stream :: map()) -> {ok, term()} | {error, term(), term()} | timeout | stream_finished.
finish(Stream) ->
    #{request_timeout := Timeout} = stream_timeouts(Stream),
    finish(Stream, greptimedb_worker:caller_timeout(Timeout)).

%% @doc Finish the gRPC stream and wait the result with timeout in milliseconds.
-spec finish(Stream :: map(), Timeout :: integer()) -> {ok, term()} | {error, term(), term()} | timeout | stream_finished.
stream_timeouts(#{timeouts := Timeouts}) ->
    Timeouts;
stream_timeouts(Stream) ->
    greptimedb_worker:timeouts(maps:get(cli_opts, Stream, [])).

finish(Stream, Timeout) ->
    try
        ok = grpcbox_client:close_send(Stream),
        grpcbox_client:recv_data(Stream, Timeout)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] close write stream failed: ~0p ~0p ~p", [E, R, S]),
            {error, R}
    end.
