%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service greptime.v1.PrometheusGateway.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(greptime_v_1_prometheus_gateway_bhvr).

%% Unary RPC
-callback handle(ctx:t(), database_pb:promql_request()) ->
    {ok, database_pb:promql_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

