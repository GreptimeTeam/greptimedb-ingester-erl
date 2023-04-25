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

-module(greptimedb).

-export([start_client/1, stop_client/1, write/3, ddl/1]).

-spec start_client(list()) ->
                      {ok, Client :: map()} |
                      {error, {already_started, Client :: map()}} |
                      {error, Reason :: term()}.
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

write(#{protocol := Protocol} = Client, Metric, Points) ->
    try
        case Protocol of
            http ->
                Request = greptimedb_encoder:insert_request(Metric, Points),
                rpc_call(Client, Request)
        end
    catch
        E:R:S ->
            logger:error("[GreptimeDB] write ~0p failed: ~0p ~0p ~0p ~p",
                         [Metric, Points, E, R, S]),
            {error, R}
    end.

ddl(_Client) ->
    todo.

-spec stop_client(Client :: map()) -> ok | term().
stop_client(#{pool := Pool, protocol := Protocol}) ->
    case Protocol of
        http ->
            ecpool:stop_sup_pool(Pool)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

rpc_call(#{pool := Pool} = _Client, Request) ->
    Fun = fun(Worker) -> greptimedb_worker:rpc_call(Worker, Request) end,
    try
        ecpool:with_client(Pool, Fun)
    catch
        E:R:S ->
            logger:error("[GreptimeDB] grpc write fail: ~0p ~0p ~0p", [E, R, S]),
            {error, {E, R}}
    end.
