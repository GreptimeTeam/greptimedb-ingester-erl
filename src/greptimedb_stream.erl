-module(greptimedb_stream).

-export([write/3, finish/1]).

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
    try
        Request = greptimedb_encoder:insert_requests(Stream, [{Metric, Points}]),
        grpcbox_client:send(Stream, Request)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] stream write ~0p failed: ~0p ~0p ~0p ~p",
                         [Metric, Points, E, R, S]),
            {error, R}
    end.

-spec finish(Stream :: map()) -> {ok, term()} | {error, term()}.
finish(Stream) ->
    finish(Stream, 5000).

-spec finish(Stream :: map(), Timeout :: integer()) -> {ok, term()} | {error, term()}.
finish(Stream, Timeout) ->
    try
        ok = grpcbox_client:close_send(Stream),
        grpcbox_client:recv_data(Stream, Timeout)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] close write stream failed: ~0p ~0p ~p", [E, R, S]),
            {error, R}
    end.
