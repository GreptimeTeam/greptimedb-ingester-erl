%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service greptime.v1.GreptimeDatabase.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(greptime_v_1_greptime_database_bhvr).

%% Unary RPC
-callback handle(ctx:t(), greptimedb_database_pb:greptime_request()) ->
    {ok, greptimedb_database_pb:greptime_response(), ctx:t()} | grpcbox_stream:grpc_error_response().

%%
-callback handle_requests(reference(), grpcbox_stream:t()) ->
    {ok, greptimedb_database_pb:greptime_response(), ctx:t()} | grpcbox_stream:grpc_error_response().
