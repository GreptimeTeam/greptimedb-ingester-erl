-module(greptimedb_stream).

-export([write/3, finish/1]).

write(Stream, Metric, Points) ->
    try
        Request = greptimedb_encoder:insert_request(Metric, Points),
        logger:debug("[GreptimeDB] write request ~w~n", [Request]),
        grpcbox_client:send(Stream, Request)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] stream write ~0p failed: ~0p ~0p ~0p ~p",
                         [Metric, Points, E, R, S]),
            {error, R}
    end.

finish(Stream) ->
    finish(Stream, 5000).

finish(Stream, Timeout) ->
    try
        ok = grpcbox_client:close_send(Stream),
        grpcbox_client:recv_data(Stream, Timeout)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] close write stream failed: ~0p ~0p ~p", [E, R, S]),
            {error, R}
    end.
