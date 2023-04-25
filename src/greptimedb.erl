-module(greptimedb).

-export([start_client/1, send/3, ddl/1]).

start_client(Options0) ->
    Pool = proplists:get_value(pool, Options0),
    Options = lists:keydelete(protocol, 1, lists:keydelete(pool, 1, Options0)),

    Client = #{pool => Pool, protocol => http},
    case ecpool:start_sup_pool(Pool, greptimedb_worker, Options) of
        {ok, _} ->
            {ok, Client};
        {error, {already_started, _}} ->
            {error, {already_started, Client}};
        {error, Reason} ->
            {error, Reason}
    end.

send(#{protocol := Protocol} = Client, Metric, Points) ->
    try
        case Protocol of
            http ->
                send0(Client, Metric, Points)
        end
    catch
        E:R:S ->
            logger:error("[GreptimeDB] write ~0p failed: ~0p ~0p ~0p ~p",
                         [Metric, Points, E, R, S]),
            {error, R}
    end.

send0(#{pool := Pool} = _Client, Metric, Points) ->
    Fun = fun(Worker) -> greptimedb_worker:send(Worker, Metric, Points) end,
    try
        ecpool:with_client(Pool, Fun)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] grpc write fail: ~0p ~0p ~0p", [E, R, S]),
            {error, {E, R}}
    end.

ddl(_Client) ->
    ok.
