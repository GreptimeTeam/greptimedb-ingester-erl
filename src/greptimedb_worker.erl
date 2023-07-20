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

-export([handle/2, stream/1, ddl/0, health_check/1]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([connect/1]).

-record(state, {channel}).

-define(CALL_TIMEOUT, 12_000).
-define(HEALTH_CHECK_TIMEOUT, 1_000).
-define(REQUEST_TIMEOUT, 10_000).
-define(CONNECT_TIMEOUT, 5_000).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init(Args) ->
    logger:debug("[GreptimeDB] genserver has started (~w)~n", [self()]),
    Endpoints = proplists:get_value(endpoints, Args),
    Options = proplists:get_value(gprc_options, Args, #{connect_timeout => ?CONNECT_TIMEOUT}),
    Channels =
        lists:map(fun({Schema, Host, Port}) -> {Schema, Host, Port, []} end, Endpoints),
    Channel = list_to_atom(pid_to_list(self())),
    {ok, _} = grpcbox_channel_sup:start_child(Channel, Channels, Options),
    {ok, #state{channel = Channel}}.

handle_call({handle, Request}, _From, #state{channel = Channel} = State) ->
    Ctx = ctx:with_deadline_after(?REQUEST_TIMEOUT, millisecond),
    Reply = greptime_v_1_greptime_database_client:handle(Ctx, Request, #{channel => Channel}),
    case Reply of
        {ok, Resp, _} ->
            {reply, {ok, Resp}, State};
        {error, {?GRPC_STATUS_UNAUTHENTICATED, Msg}, Other} ->
            {reply, {error, {unauth, Msg, Other}}, State};
        Err ->
            {reply, Err, State}
    end;
handle_call(health_check, _From, #state{channel = Channel} = State) ->
    Request = #{},
    Ctx = ctx:with_deadline_after(?HEALTH_CHECK_TIMEOUT, millisecond),
    Reply =
        greptime_v_1_health_check_client:health_check(Ctx, Request, #{channel => Channel}),
    case Reply of
        {ok, Resp, _} ->
            {reply, {ok, Resp}, State};
        Err ->
            {reply, Err, State}
    end;
handle_call(channel, _From, #state{channel = Channel} = State) ->
    {reply, {ok, Channel}, State}.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{channel = Channel} = State) ->
    logger:debug("[GreptimeDB] genserver has stopped (~w)~n", [self()]),
    grpcbox_channel:stop(Channel),
    {stop, Reason, State}.

%%%===================================================================
%%% Public functions
%%%===================================================================
handle(Pid, Request) ->
    gen_server:call(Pid, {handle, Request}, ?CALL_TIMEOUT).

health_check(Pid) ->
    gen_server:call(Pid, health_check, ?HEALTH_CHECK_TIMEOUT).

stream(Pid) ->
    {ok, Channel} = gen_server:call(Pid, channel, ?CALL_TIMEOUT),
    Ctx = ctx:with_deadline_after(?REQUEST_TIMEOUT, millisecond),
    greptime_v_1_greptime_database_client:handle_requests(Ctx, #{channel => Channel}).

ddl() ->
    todo.

%%%===================================================================
%%% ecpool callback
%%%===================================================================
connect(Options) ->
    start_link(Options).
