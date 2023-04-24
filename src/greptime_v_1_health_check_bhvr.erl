%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service greptime.v1.HealthCheck.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(greptime_v_1_health_check_bhvr).

%% Unary RPC
-callback health_check(ctx:t(), health_pb:health_check_request()) ->
    {ok, health_pb:health_check_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

