%%%-------------------------------------------------------------------
%% @doc Client module for grpc service greptime.v1.GreptimeDatabase.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(greptime_v_1_greptime_database_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'greptime.v1.GreptimeDatabase').
-define(PROTO_MODULE, 'greptimedb_database_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec handle(greptimedb_database_pb:greptime_request()) ->
    {ok, greptimedb_database_pb:greptime_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
handle(Input) ->
    handle(ctx:new(), Input, #{}).

-spec handle(ctx:t() | greptimedb_database_pb:greptime_request(), greptimedb_database_pb:greptime_request() | grpcbox_client:options()) ->
    {ok, greptimedb_database_pb:greptime_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
handle(Ctx, Input) when ?is_ctx(Ctx) ->
    handle(Ctx, Input, #{});
handle(Input, Options) ->
    handle(ctx:new(), Input, Options).

-spec handle(ctx:t(), greptimedb_database_pb:greptime_request(), grpcbox_client:options()) ->
    {ok, greptimedb_database_pb:greptime_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response() | {error, any()}.
handle(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/greptime.v1.GreptimeDatabase/Handle">>, Input, ?DEF(greptime_request, greptime_response, <<"greptime.v1.GreptimeRequest">>), Options).

%% @doc
-spec handle_requests() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
handle_requests() ->
    handle_requests(ctx:new(), #{}).

-spec handle_requests(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
handle_requests(Ctx) when ?is_ctx(Ctx) ->
    handle_requests(Ctx, #{});
handle_requests(Options) ->
    handle_requests(ctx:new(), Options).

-spec handle_requests(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
handle_requests(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/greptime.v1.GreptimeDatabase/HandleRequests">>, ?DEF(greptime_request, greptime_response, <<"greptime.v1.GreptimeRequest">>), Options).
