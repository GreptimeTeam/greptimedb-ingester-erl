%% Copyright 2023 Greptime Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(greptimedb_worker).

-behaviour(gen_server).

-behavihour(ecpool_worker).

-include_lib("grpcbox/include/grpcbox.hrl").

-export([handle/3, stream/2, ddl/0, health_check/2, timeouts/1, running_timeouts/1,
         caller_timeout/2]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, async_handle/4]).
-export([connect/1]).

-export_type([timeouts/0]).

-type timeouts() :: #{connect_timeout := pos_integer(),
                      request_timeout := pos_integer(),
                      health_check_timeout := pos_integer(),
                      tcp_user_timeout := non_neg_integer()}.

-record(state, {channel, requests, hints, timeouts}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_CONNECT_TIMEOUT, 5_000).
-define(DEFAULT_REQUEST_TIMEOUT, 10_000).
-define(DEFAULT_HEALTH_CHECK_TIMEOUT, 10_000).
-define(DEFAULT_TCP_USER_TIMEOUT, 0).
%% Extra time a caller waits on top of the gRPC deadline. Without it the caller
%% gives up first and reports its own timeout instead of the real gRPC error.
-define(CALLER_TIMEOUT_MARGIN, 2_000).
-define(GTDB_HINT_HEADER, <<"x-greptime-hints">>).
-define(ASYNC_BATCH_SIZE, 200).
-define(ASYNC_BATCH_LINGER, 20).
-define(ASYNC_REQ(Req, ExpireAt, ResultCallback),
        {async, Req, ExpireAt, ResultCallback}
       ).
-define(REQ(Req, ExpireAt),
        {Req, ExpireAt}
       ).
-define(PEND_REQ(ReplyTo, Req), {ReplyTo, Req}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init(Args) ->
    process_flag(trap_exit, true),
    {pool, PoolName0} = lists:keyfind(pool, 1, Args),
    PoolName = case is_binary(PoolName0) of
                   true -> PoolName0;
                   false -> iolist_to_binary(io_lib:format("~0tp", [PoolName0]))
               end,
    {ecpool_worker_id, WorkerId} = lists:keyfind(ecpool_worker_id, 1, Args),
    Endpoints = proplists:get_value(endpoints, Args),
    Hints0 = proplists:get_value(grpc_hints, Args, #{}),
    Hints = maps:fold(fun(Key, Value, #{?GTDB_HINT_HEADER := OldValue} = Acc) ->
                              Sep = case OldValue of
                                        <<>> -> <<"">>;
                                        _    -> <<",">>
                                    end,
                              Acc#{?GTDB_HINT_HEADER => <<OldValue/binary, Sep/binary, Key/binary,"=",Value/binary>>}
                      end, #{?GTDB_HINT_HEADER => <<>>}, Hints0),
    SslOptions = proplists:get_value(ssl_opts, Args, []),
    Options = proplists:get_value(grpc_opts, Args, #{}),
    #{connect_timeout := ConnectTimeout,
      tcp_user_timeout := TcpUserTimeout} = Timeouts = timeouts(Args),
    %% grpcbox only forwards the per-endpoint connection settings to chatterbox,
    %% the channel options are not consulted for these.
    ConnSettings = #{connect_timeout => ConnectTimeout,
                     tcp_user_timeout => TcpUserTimeout},
    Channels =
        lists:map(fun({Scheme, Host, Port}) ->
                     {Scheme, Host, Port, ssl_options(Scheme, SslOptions), ConnSettings}
                  end, Endpoints),
    Channel = iolist_to_binary([PoolName, ":", integer_to_binary(WorkerId)]),
    {ok, _} = start_channel(Channel, Channels, Options),
    logger:debug("[GreptimeDB] genserver has started (~s)~n", [Channel]),
    {ok, #state{channel = Channel, hints = Hints, timeouts = Timeouts,
                requests = #{ pending => queue:new(), pending_count => 0}}}.

handle_call({handle, Request}, _From,
            #state{channel = Channel, hints = Hints,
                   timeouts = #{request_timeout := Timeout}} = State) ->
    Ctx = new_ctx(Timeout, Hints),
    Reply = greptime_v_1_greptime_database_client:handle(Ctx, Request, #{channel => Channel}),
    logger:debug("[GreptimeDB] handle_call reply: ~w~n", [Reply]),
    case Reply of
        {ok, Resp, _} ->
            {reply, {ok, Resp}, State};
        {error, {?GRPC_STATUS_UNAUTHENTICATED, Msg}, Other} ->
            {reply, {error, {unauth, Msg, Other}}, State};
        Err ->
            {reply, Err, State}
    end;
handle_call(health_check, _From,
            #state{channel = Channel, hints = Hints,
                   timeouts = #{health_check_timeout := Timeout}} = State) ->
    Request = #{},
    Ctx = new_ctx(Timeout, Hints),
    Reply =
        greptime_v_1_health_check_client:health_check(Ctx, Request, #{channel => Channel}),
    case Reply of
        {ok, Resp, _} ->
            {reply, {ok, Resp}, State};
        Err ->
            {reply, Err, State}
    end;
handle_call(channel, _From, #state{channel = Channel, hints = Hints} = State) ->
    {reply, {ok, Channel, Hints}, State};
handle_call(timeouts, _From, #state{timeouts = Timeouts} = State) ->
    {reply, {ok, Timeouts}, State}.

handle_info(?ASYNC_REQ(Request, ExpireAt, ResultCallback), State0) ->
    Req = ?REQ(Request, ExpireAt),
    State1 = enqueue_req(ResultCallback, Req, State0),
    State = maybe_shoot(State1, false),
    noreply_state(State);

handle_info(timeout, State0) ->
    State = maybe_shoot(State0, true),
    noreply_state(State);

handle_info(Info, State) ->
    logger:debug("~p unexpected_info: ~p, channel: ~p", [?MODULE, Info, State#state.channel]),

    {noreply, State, ?ASYNC_BATCH_LINGER}.


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(Reason, #state{channel = Channel} = State) ->
    logger:debug("[GreptimeDB] genserver has stopped (~w)~n", [self()]),
    grpcbox_channel:stop(Channel),
    {stop, Reason, State}.


%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @doc Resolve the timeouts a worker and its callers both derive from the
%% client options.
-spec timeouts(list()) -> timeouts().
timeouts(Options) ->
    GrpcOpts = proplists:get_value(grpc_opts, Options, #{}),
    ConnectTimeout = maps:get(connect_timeout, GrpcOpts, ?DEFAULT_CONNECT_TIMEOUT),
    #{connect_timeout => proplists:get_value(connect_timeout, Options, ConnectTimeout),
      request_timeout => proplists:get_value(request_timeout, Options, ?DEFAULT_REQUEST_TIMEOUT),
      health_check_timeout =>
          proplists:get_value(health_check_timeout, Options, ?DEFAULT_HEALTH_CHECK_TIMEOUT),
      tcp_user_timeout =>
          proplists:get_value(tcp_user_timeout, Options, ?DEFAULT_TCP_USER_TIMEOUT)}.

-spec caller_timeout(request_timeout | health_check_timeout, timeouts()) -> pos_integer().
caller_timeout(Key, Timeouts) ->
    maps:get(Key, Timeouts) + ?CALLER_TIMEOUT_MARGIN.

start_channel(Channel, Channels, Options) ->
    case grpcbox_channel_sup:start_child(Channel, Channels, Options) of
        {error, {already_started, StaleChannel}} ->
            %% A worker may terminate before its grpcbox channel is stopped, leaving
            %% the channel alive under the global supervisor. Remove that orphan
            %% before reusing the deterministic channel name.
            logger:warning(
              "[GreptimeDB] removing stale grpc channel ~s (~p)",
              [Channel, StaleChannel]),
            ok = stop_stale_channel(Channel),
            grpcbox_channel_sup:start_child(Channel, Channels, Options);
        Result ->
            Result
    end.

stop_stale_channel(Channel) ->
    try
        _ = grpcbox_channel:stop(Channel),
        ok
    catch
        exit:noproc -> ok;
        exit:{noproc, _} -> ok
    end.

ssl_options(https, []) ->
    %% https://www.erlang.org/doc/man/ssl#type-client_option
    [
     {verify, verify_peer},
     {cacerts, public_key:cacerts_get()},
     %% hostname may be wildcard
     {customize_hostname_check, [{match_fun,public_key:pkix_verify_hostname_match_fun(https)}]}
    ];

ssl_options(_, SslOptions) ->
    SslOptions.

new_ctx(TimeoutMs, Values) ->
    Ctx0 = ctx:with_values(#{md_outgoing_key => Values}),
    ctx:with_deadline_after(Ctx0, TimeoutMs, millisecond).

now_() ->
    erlang:system_time(millisecond).


fresh_expire_at(Timeout) ->
    now_() + Timeout.

enqueue_latest_fn(#{prioritise_latest := true}) ->
    fun queue:in_r/2;
enqueue_latest_fn(_) ->
    fun queue:in/2.

peek_oldest_fn(#{prioritise_latest := true}) ->
    {fun queue:peek_r/1, fun queue:out_r/1};
peek_oldest_fn(_) ->
    {fun queue:peek/1, fun queue:out/1}.

%% For async-request, we evaluate the result-callback with {error, timeout}
maybe_reply_timeout({F, A}) when is_function(F) ->
    _ = erlang:apply(F, A ++ [{error, timeout}]),
    ok;
maybe_reply_timeout(_) ->
    %% This is not a callback, but the gen_server:call's From
    %% The caller should have alreay given up waiting for a reply,
    %% so no need to call gen_server:reply(From, {error, timeout})
    ok.

reply({F, A}, Result) when is_function(F) ->
    _ = erlang:apply(F, A ++ [Result]),
    ok;
reply(From, Result) ->
    gen_server:reply(From, Result).

noreply_state(#state{requests = #{pending_count := N}} = State) when N > 0 ->
    {noreply, State, ?ASYNC_BATCH_LINGER};

noreply_state(State) ->
    {noreply, State}.


%%%===================================================================
%%% Async requests queue functions
%%%===================================================================
enqueue_req(ReplyTo, Req, #state{requests = Requests0} = State) ->
    #{
      pending := Pending,
      pending_count := PC
     } = Requests0,
    InFun = enqueue_latest_fn(Requests0),
    NewPending = InFun(?PEND_REQ(ReplyTo, Req), Pending),
    Requests = Requests0#{pending := NewPending, pending_count := PC + 1},
    State#state{requests = drop_expired(Requests)}.


%% Try to write requests
maybe_shoot(#state{requests = Requests0} = State0, Force) ->
    do_shoot(State0#state{requests = drop_expired(Requests0)}, Force).


do_shoot(#state{requests = #{pending := Pending0, pending_count := N} = Requests0, channel = Channel} = State0, _Force) when
      N >= ?ASYNC_BATCH_SIZE ->
    do_shoot(State0, Requests0, Pending0, N, Channel);

do_shoot(#state{requests = #{pending := Pending0, pending_count := N} = Requests0, channel = Channel} = State0, true) when
      N > 0 ->
    do_shoot(State0, Requests0, Pending0, N, Channel);
do_shoot(State, _Force) ->
    State.

do_shoot(#state{hints = Hints, timeouts = #{request_timeout := Timeout}} = State0,
         Requests0, Pending0, N, Channel) ->
    {{value, ?PEND_REQ(ReplyTo, Req)}, Pending} = queue:out(Pending0),
    Requests = Requests0#{pending := Pending, pending_count := N - 1},
    State1 = State0#state{requests = Requests},
    Ctx = new_ctx(Timeout, Hints),
    try
        case greptime_v_1_greptime_database_client:handle_requests(Ctx, #{channel => Channel}) of
            {ok, Stream} ->
                shoot(Stream, Req, State1, [ReplyTo]);
            Error ->
                reply(ReplyTo, Error),
                State1

        end
    catch
        E:R:S ->
            logger:error("[GreptimeDB] failed to shoot(pending=~0p,channel=~0p): ~0p ~0p ~p", [N, Channel, E, R, S]),
            reply(ReplyTo, R),
            State1
    end.

shoot(Stream, ?REQ(Req, _),
      #state{requests = #{pending_count := 0}, timeouts = Timeouts} = State, ReplyToList) ->
    %% Write the last request and finish stream. This stream carries no
    %% cli_opts, so finish/1 cannot resolve the timeout itself.
    case greptimedb_stream:write_request(Stream, Req) of
        ok ->
            Result =  case greptimedb_stream:finish(
                             Stream, caller_timeout(request_timeout, Timeouts)) of
                          {ok, Resp} ->
                              {ok, Resp};
                          {error, {?GRPC_STATUS_UNAUTHENTICATED, Msg}, Other} ->
                              {error, {unauth, Msg, Other}};
                          Err ->
                              {error, Err}
                      end,

            lists:foreach(fun(ReplyTo) ->
                                  reply(ReplyTo, Result)
                          end, ReplyToList);
        Error ->
            lists:foreach(fun(ReplyTo) ->
                                  reply(ReplyTo, Error)
                          end, ReplyToList)
    end,
    State;

shoot(Stream, ?REQ(Req, _), #state{requests = #{pending := Pending0, pending_count := N} = Requests0} = State0, ReplyToList) ->
    case greptimedb_stream:write_request(Stream, Req) of
        ok ->
            {{value, ?PEND_REQ(ReplyTo, NextReq)}, Pending} = queue:out(Pending0),
            Requests = Requests0#{pending := Pending, pending_count := N - 1},
            State1 = State0#state{requests = Requests},
            shoot(Stream, NextReq, State1, [ReplyTo | ReplyToList]);
        Error ->
            lists:foreach(fun(ReplyTo) ->
                                  reply(ReplyTo, Error)
                          end, ReplyToList),
            State0
    end.

%% Continue droping expired requests, to avoid the state RAM usage
%% explosion if http client can not keep up.
drop_expired(#{pending_count := 0} = Requests) ->
    Requests;
drop_expired(Requests) ->
    drop_expired(Requests, now_()).

drop_expired(#{pending_count := 0} = Requests, _Now) ->
    Requests;
drop_expired(#{pending := Pending, pending_count := PC} = Requests, Now) ->
    {PeekFun, OutFun} = peek_oldest_fn(Requests),
    {value, ?PEND_REQ(ReplyTo, ?REQ(_, ExpireAt))} = PeekFun(Pending),
    case is_integer(ExpireAt) andalso Now > ExpireAt of
        true ->
            {_, NewPendings} = OutFun(Pending),
            NewRequests = Requests#{pending => NewPendings, pending_count => PC - 1},
            ok = maybe_reply_timeout(ReplyTo),
            drop_expired(NewRequests, Now);
        false ->
            Requests
    end.

%%%===================================================================
%%% Public functions
%%%===================================================================
handle(Pid, Request, Timeouts) ->
    gen_server:call(Pid, {handle, Request}, caller_timeout(request_timeout, Timeouts)).

async_handle(Pid, Request, ResultCallback, #{request_timeout := Timeout}) ->
    ExpireAt = fresh_expire_at(Timeout),
    _ = erlang:send(Pid, ?ASYNC_REQ(Request, ExpireAt, ResultCallback)),
    ok.

health_check(Pid, Timeouts) ->
    gen_server:call(Pid, health_check, caller_timeout(health_check_timeout, Timeouts)).

%% @doc The timeouts a running worker was started with. A reused pool keeps
%% them, so they can differ from what a later caller's options resolve to.
running_timeouts(Pid) ->
    gen_server:call(Pid, timeouts, ?CALLER_TIMEOUT_MARGIN).

stream(Pid, #{request_timeout := Timeout} = Timeouts) ->
    try
        case gen_server:call(Pid, channel, caller_timeout(request_timeout, Timeouts)) of
            {ok, Channel, Hints} ->
                %% The stream must be created by the calling process: grpcbox
                %% delivers its messages to whoever opened it.
                Ctx = new_ctx(Timeout, Hints),
                greptime_v_1_greptime_database_client:handle_requests(Ctx, #{channel => Channel});
            Err -> Err
        end
    catch
        E:R:S ->
            logger:error("[GreptimeDB] failed to create stream for ~0p: ~0p ~0p ~p", [Pid, E, R, S]),
            {error, R}
    end.

ddl() ->
    todo.

%%%===================================================================
%%% ecpool callback
%%%===================================================================
connect(Options) ->
    start_link(Options).


%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).

timeouts_test() ->
    ?assertEqual(#{connect_timeout => ?DEFAULT_CONNECT_TIMEOUT,
                   request_timeout => ?DEFAULT_REQUEST_TIMEOUT,
                   health_check_timeout => ?DEFAULT_HEALTH_CHECK_TIMEOUT,
                   tcp_user_timeout => ?DEFAULT_TCP_USER_TIMEOUT},
                 timeouts([])),
    ?assertMatch(#{tcp_user_timeout := 30_000},
                 timeouts([{tcp_user_timeout, 30_000}])),
    ?assertMatch(#{request_timeout := 3_000, health_check_timeout := 4_000},
                 timeouts([{request_timeout, 3_000}, {health_check_timeout, 4_000}])),
    ?assertMatch(#{connect_timeout := 2_000},
                 timeouts([{grpc_opts, #{connect_timeout => 2_000}}])),
    ?assertMatch(#{connect_timeout := 1_000},
                 timeouts([{connect_timeout, 1_000}, {grpc_opts, #{connect_timeout => 2_000}}])).

prioritise_latest_test() ->
    Opts = #{prioritise_latest => true},
    Seq = [1, 2, 3, 4],
    In = enqueue_latest_fn(Opts),
    {PeekOldest, OutOldest} = peek_oldest_fn(Opts),
    Q = lists:foldl(fun(I, QIn) -> In(I, QIn) end, queue:new(), Seq),
    ?assertEqual({value, 1}, PeekOldest(Q)),
    ?assertMatch({{value, 1}, _}, OutOldest(Q)),
    ?assertMatch({{value, 4}, _}, queue:out(Q)).

prioritise_oldest_test() ->
    Opts = #{prioritise_latest => false},
    Seq = [1, 2, 3, 4],
    In = enqueue_latest_fn(Opts),
    {PeekOldest, OutOldest} = peek_oldest_fn(Opts),
    Q = lists:foldl(fun(I, QIn) -> In(I, QIn) end, queue:new(), Seq),
    ?assertEqual({value, 1}, PeekOldest(Q)),
    ?assertMatch({{value, 1}, _}, OutOldest(Q)),
    ?assertMatch({{value, 1}, _}, queue:out(Q)).

-endif.
