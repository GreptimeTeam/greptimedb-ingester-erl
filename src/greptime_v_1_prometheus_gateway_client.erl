%%%-------------------------------------------------------------------
%% @doc Client module for grpc service greptime.v1.PrometheusGateway.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(greptime_v_1_prometheus_gateway_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).
-define(SERVICE, 'greptime.v1.PrometheusGateway').
-define(PROTO_MODULE, prom_pb).
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType),
        #grpcbox_def{service = ?SERVICE,
                     message_type = MessageType,
                     marshal_fun = ?MARSHAL_FUN(Input),
                     unmarshal_fun = ?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec handle(prom_pb:promql_request()) ->
                {ok, prom_pb:promql_response(), grpcbox:metadata()} |
                grpcbox_stream:grpc_error_response() |
                {error, any()}.
handle(Input) ->
    handle(ctx:new(), Input, #{}).

-spec handle(ctx:t() | prom_pb:promql_request(),
             prom_pb:promql_request() | grpcbox_client:options()) ->
                {ok, prom_pb:promql_response(), grpcbox:metadata()} |
                grpcbox_stream:grpc_error_response() |
                {error, any()}.
handle(Ctx, Input) when ?is_ctx(Ctx) ->
    handle(Ctx, Input, #{});
handle(Input, Options) ->
    handle(ctx:new(), Input, Options).

-spec handle(ctx:t(), prom_pb:promql_request(), grpcbox_client:options()) ->
                {ok, prom_pb:promql_response(), grpcbox:metadata()} |
                grpcbox_stream:grpc_error_response() |
                {error, any()}.
handle(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx,
                         <<"/greptime.v1.PrometheusGateway/Handle">>,
                         Input,
                         ?DEF(promql_request, promql_response, <<"greptime.v1.PromqlRequest">>),
                         Options).
