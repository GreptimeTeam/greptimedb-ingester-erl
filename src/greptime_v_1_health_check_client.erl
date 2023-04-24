%%%-------------------------------------------------------------------
%% @doc Client module for grpc service greptime.v1.HealthCheck.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(greptime_v_1_health_check_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).
-define(SERVICE, 'greptime.v1.HealthCheck').
-define(PROTO_MODULE, health_pb).
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType),
        #grpcbox_def{service = ?SERVICE,
                     message_type = MessageType,
                     marshal_fun = ?MARSHAL_FUN(Input),
                     unmarshal_fun = ?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec health_check(health_pb:health_check_request()) ->
                      {ok, health_pb:health_check_response(), grpcbox:metadata()} |
                      grpcbox_stream:grpc_error_response() |
                      {error, any()}.
health_check(Input) ->
    health_check(ctx:new(), Input, #{}).

-spec health_check(ctx:t() | health_pb:health_check_request(),
                   health_pb:health_check_request() | grpcbox_client:options()) ->
                      {ok, health_pb:health_check_response(), grpcbox:metadata()} |
                      grpcbox_stream:grpc_error_response() |
                      {error, any()}.
health_check(Ctx, Input) when ?is_ctx(Ctx) ->
    health_check(Ctx, Input, #{});
health_check(Input, Options) ->
    health_check(ctx:new(), Input, Options).

-spec health_check(ctx:t(), health_pb:health_check_request(), grpcbox_client:options()) ->
                      {ok, health_pb:health_check_response(), grpcbox:metadata()} |
                      grpcbox_stream:grpc_error_response() |
                      {error, any()}.
health_check(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/greptime.v1.HealthCheck/HealthCheck">>,
                         Input,
                         ?DEF(health_check_request,
                              health_check_response,
                              <<"greptime.v1.HealthCheckRequest">>),
                         Options).
