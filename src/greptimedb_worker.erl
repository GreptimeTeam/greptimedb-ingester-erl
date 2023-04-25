-module(greptimedb_worker).

-behaviour(gen_server).

-behavihour(ecpool_worker).

-export([rpc_call/2, ddl/0]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([connect/1]).

-record(state, {channel}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init(Args) ->
    logger:debug("[GreptimeDB] genserver has started (~w)~n", [self()]),
    Endpoints = proplists:get_value(endpoints, Args),
    Options = proplists:get_value(gprc_options, Args, #{}),
    Channels =
        lists:map(fun({Schema, Host, Port}) -> {Schema, Host, Port, []} end, Endpoints),
    Channel = list_to_atom(pid_to_list(self())),
    {ok, _} = grpcbox_channel_sup:start_child(Channel, Channels, Options),
    {ok, #state{channel = Channel}}.

handle_call({handle, Request}, _From, #state{channel = Channel} = State) ->
    Reply = greptime_v_1_greptime_database_client:handle(Request, #{channel => Channel}),
    case Reply of
        {ok, Resp, _} ->
            {reply, {ok, Resp}, State};
        Err ->
            {reply, Err, State}
    end.

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
rpc_call(Pid, Request) ->
    gen_server:call(Pid, {handle, Request}).

ddl() ->
    todo.

%%%===================================================================
%%% ecpool callback
%%%===================================================================
connect(Options) ->
    start_link(Options).
