-module(greptimedb_worker).

-behaviour(gen_server).

-behavihour(ecpool_worker).

-export([send/3, ddl/0]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([connect/1]).

-ifdef(TEST).

-export([collect_columns/1]).

-endif.

-record(state, {channel}).

-define(TS_COLUMN, "greptime_timestamp").
-define(DEFAULT_CATALOG, "greptime").
-define(DEFAULT_SCHEMA, "public").

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init(Args) ->
    error_logger:info_msg("greptimedb genserver has started (~w)~n", [self()]),
    Endpoints = proplists:get_value(endpoints, Args),
    Options = proplists:get_value(gprc_options, Args, #{}),
    Channels =
        lists:map(fun({Schema, Host, Port}) -> {Schema, Host, Port, []} end, Endpoints),
    Channel = list_to_atom(pid_to_list(self())),
    {ok, _} = grpcbox_channel_sup:start_child(Channel, Channels, Options),
    {ok, #state{channel = Channel}}.

handle_call({send, {Catalog, Schema, Table}, Points},
            _From,
            #state{channel = Channel} = State) ->
    RowCount = length(Points),
    Columns =
        lists:map(fun(Column) -> pad_null_mask(Column, RowCount) end, collect_columns(Points)),
    Header = #{catalog => Catalog, schema => Schema},
    Request =
        {insert,
         #{table_name => Table,
           columns => Columns,
           row_count => RowCount}},
    Reply =
        greptime_v_1_greptime_database_client:handle(#{header => Header, request => Request},
                                                     #{channel => Channel}),
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
    grpcbox_channel:stop(Channel),
    {stop, Reason, State}.

%%%===================================================================
%%% Public functions
%%%===================================================================
send(Pid, {Catalog, Schema, Table}, Points) ->
    gen_server:call(Pid, {send, {Catalog, Schema, Table}, Points});
send(Pid, Metric, Points) ->
    send(Pid, {?DEFAULT_CATALOG, ?DEFAULT_SCHEMA, Metric}, Points).

ddl() ->
    todo.

%%%===================================================================
%%% ecpool callback
%%%===================================================================
connect(Options) ->
    start_link(Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================
values_size(#{i8_values := Values}) ->
    length(Values);
values_size(#{i16_values := Values}) ->
    length(Values);
values_size(#{i32_values := Values}) ->
    length(Values);
values_size(#{i64_values := Values}) ->
    length(Values);
values_size(#{u8_values := Values}) ->
    length(Values);
values_size(#{u16_values := Values}) ->
    length(Values);
values_size(#{u32_values := Values}) ->
    length(Values);
values_size(#{u64_values := Values}) ->
    length(Values);
values_size(#{f32_values := Values}) ->
    length(Values);
values_size(#{f64_values := Values}) ->
    length(Values);
values_size(#{bool_values := Values}) ->
    length(Values);
values_size(#{binary_values := Values}) ->
    length(Values);
values_size(#{string_values := Values}) ->
    length(Values);
values_size(#{date_values := Values}) ->
    length(Values);
values_size(#{ts_second_values := Values}) ->
    length(Values);
values_size(#{ts_millisecond_values := Values}) ->
    length(Values);
values_size(#{ts_nanosecond_values := Values}) ->
    length(Values).

pad_null_mask(#{values := Values, null_mask := NullMask} = Column, RowCount) ->
    ValuesSize = values_size(Values),
    NewColumn =
        if ValuesSize == RowCount ->
               maps:remove(null_mask, Column);
           true ->
               Pad = 8 - (bit_size(NullMask) - floor(bit_size(NullMask) / 8) * 8),
               Column#{null_mask => <<0:Pad/integer, NullMask/bits>>}
        end,
    NewColumn.

convert_columns(#{fields := Fields,
                  tags := Tags,
                  timestamp := Ts}) ->
    TsColumn = ts_column(Ts),
    FieldColumns = maps:map(fun(K, V) -> field_column(K, V) end, Fields),
    TagColumns = maps:map(fun(K, V) -> tag_column(K, V) end, Tags),
    maps:put(
        maps:get(column_name, TsColumn), TsColumn, maps:merge(FieldColumns, TagColumns)).

merge_column(#{null_mask := NullMask} = Column, NewColumn) ->
    Values = maps:get(values, Column, #{}),
    NewValues = maps:get(values, NewColumn),
    MergedValues =
        maps:merge_with(fun(_K, V1, V2) -> lists:foldr(fun(X, XS) -> [X | XS] end, V2, V1) end,
                        Values,
                        NewValues),
    NewColumn1 = maps:merge(Column, NewColumn),
    NewColumn1#{values => MergedValues, null_mask => <<NullMask/bits, 1:1/integer>>}.

merge_columns(NextColumns, Columns) ->
    maps:fold(fun(Name, #{null_mask := NullMask} = Column, AccColumns) ->
                 MergedColumn =
                     case maps:find(Name, NextColumns) of
                         {ok, NewColumn} ->
                             merge_column(Column, NewColumn);
                         _ ->
                             Column#{null_mask => <<NullMask/bits, 0:1/integer>>}
                     end,
                 AccColumns#{Name => MergedColumn}
              end,
              Columns,
              Columns).

empty_column() ->
    #{null_mask => <<>>}.

merge_columns(Columns) ->
    Names =
        sets:to_list(
            sets:union(
                lists:map(fun(C) ->
                             sets:from_list(
                                 maps:keys(C))
                          end,
                          Columns))),
    EmptyColumns =
        maps:from_list(
            lists:map(fun(Name) -> {Name, empty_column()} end, Names)),
    lists:foldl(fun merge_columns/2, EmptyColumns, Columns).

%% collect columns from points
collect_columns(Points) ->
    collect_columns(Points, []).

collect_columns([], Columns) ->
    maps:values(merge_columns(Columns));
collect_columns([Point | T], Columns) ->
    collect_columns(T, [convert_columns(Point) | Columns]).

ts_column(Ts) when is_map(Ts) ->
    maps:merge(#{column_name => ?TS_COLUMN, semantic_type => 'TIMESTAMP'}, Ts);
ts_column(Ts) ->
    #{column_name => ?TS_COLUMN,
      semantic_type => 'TIMESTAMP',
      values => #{ts_millisecond_values => [Ts]},
      datatype => 'TIMESTAMP_MILLISECOND'}.

field_column(Name, V) when is_map(V) ->
    maps:merge(#{column_name => Name, semantic_type => 'FIELD'}, V);
field_column(Name, V) ->
    #{column_name => Name,
      semantic_type => 'FIELD',
      values => #{f64_values => [V]},
      datatype => 'FLOAT64'}.

tag_column(Name, V) when is_map(V) ->
    maps:merge(#{column_name => Name, semantic_type => 'TAG'}, V);
tag_column(Name, V) ->
    #{column_name => Name,
      semantic_type => 'TAG',
      values => #{string_values => [V]},
      datatype => 'STRING'}.
