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

-module(greptimedb_encoder).

-export([insert_requests/2]).

-define(TS_COLUMN, <<"greptime_timestamp">>).
-define(DEFAULT_DBNAME, "greptime-public").

%% @doc Converts a batch of data points into GreptimeDB gRPC row-based insert requests.
%%
%% This function transforms Erlang data structures into the row-based protobuf format
%% required by GreptimeDB's gRPC API for batch inserts.
%%
%% @param Client A map containing client options, including:
%%   - `cli_opts`: List of options such as auth, dbname, timeunit
%%
%% @param TableAndPoints A list of tuples where each tuple contains:
%%   - Metric: Can be one of:
%%     * Binary/string/atom table name
%%     * {DbName, TableName} tuple for backward compatibility
%%     * Map with keys: table (required), dbname, timeunit
%%   - Points: List of point maps, each containing:
%%     * fields: Map of field names to values (stored as metrics)
%%     * tags: Map of tag names to values (stored as metadata)
%%     * timestamp: Integer timestamp or map with typed value
%%
%% @returns A map containing:
%%   - `header`: Map with dbname and optional authorization
%%   - `request`: Tuple {row_inserts, #{inserts => [...]}} with row data
%%
%% Example:
%% ```
%% Client = #{cli_opts => [{auth, {basic, #{username => "user", password => "pass"}}},
%%                         {timeunit, ms}]},
%% Points = [#{fields => #{<<"temperature">> => 25.5},
%%            tags => #{<<"location">> => <<"room1">>},
%%            timestamp => 1619775142098}],
%% Request = insert_requests(Client, [{<<"sensors">>, Points}]).
%% '''
insert_requests(Client, TableAndPoints) ->
    insert_requests(Client, TableAndPoints, unknown, []).

insert_requests(#{cli_opts := Options} = _Client, [], DbName, Inserts) ->
    AuthHeader = proplists:get_value(auth, Options, {}),
    Header =
        case AuthHeader of
            {} ->
                #{dbname => DbName};
            Scheme ->
                #{dbname => DbName, authorization => #{auth_scheme => Scheme}}
        end,
    #{header => Header, request => {row_inserts, #{inserts => Inserts}}};
insert_requests(#{cli_opts := Options} = Client,
                [{Metric, Points} | T],
                PrevDbName,
                Inserts) ->
    {DbName, Insert} = make_row_insert_request(Options, metric(Options, Metric), Points),
    case PrevDbName of
        unknown ->
            insert_requests(Client, T, DbName, [Insert | Inserts]);
        Name when Name == DbName ->
            insert_requests(Client, T, Name, [Insert | Inserts])
    end.

make_row_insert_request(_Options,
                        #{dbname := DbName,
                          table := Table,
                          timeunit := Timeunit},
                        Points) ->
    {Schema, Rows} = convert_to_rows(Timeunit, Points),
    {DbName, #{table_name => Table, rows => #{schema => Schema, rows => Rows}}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

metric(Options, Metric) ->
    metric_with_default(default_metric(Options), Metric).

default_metric(Options) ->
    #{dbname => proplists:get_value(dbname, Options, ?DEFAULT_DBNAME),
      timeunit => proplists:get_value(timeunit, Options, ms)}.

metric_with_default(Default, #{table := _} = Metric) ->
    maps:merge(Default, Metric);
metric_with_default(Default, {DbName, Table}) ->
    Default#{dbname => DbName, table => Table};
metric_with_default(Default, Table)
    when is_atom(Table); is_list(Table); is_binary(Table) ->
    Default#{table => Table}.

%% @private
%% @doc Converts points to row-based format with schema and rows.
%%
%% Creates a schema from all unique columns across points, then
%% converts each point to a row following the schema order.
%%
%% @param Timeunit Time unit for timestamp columns (ns, us, ms, s)
%% @param Points List of data points
%% @returns {Schema, Rows} tuple
convert_to_rows(Timeunit, Points) ->
    Schema = create_schema(Timeunit, Points),
    %% Construct column index：Name -> {Index, SemanticType, DataType}
    IndexMap = index_schema(Schema),
    Rows = [point_to_row_sparse(Timeunit, Point, IndexMap) || Point <- Points],
    {Schema, Rows}.

%% @private
%% @doc Creates a column schema from all points.
%%
%% Extracts all unique columns from points and creates a consistent
%% schema with column names, datatypes, and semantic types.
%%
%% @param Timeunit Time unit for timestamp columns
%% @param Points List of data points
%% @returns List of column schema maps
create_schema(Timeunit, Points) ->
    % Collect all unique column info from all points
    AllColumns =
        lists:foldl(fun(Point, Acc) ->
                       Columns = extract_columns_info(Timeunit, Point),
                       merge_column_info(Columns, Acc)
                    end,
                    #{},
                    Points),
    % Convert to list and sort for consistent order
    % Timestamp first, then tags, then fields
    SortedColumns = sort_columns(maps:to_list(AllColumns)),
    lists:map(fun({_Name, Schema}) -> Schema end, SortedColumns).

%% @private
%% @doc Extracts column information from a single point.
%%
%%
%% @param Timeunit Time unit for timestamp columns
%% @param Point Map with fields, tags, and timestamp
%% @returns Map of column_name -> column schema
extract_columns_info(Timeunit,
                     #{fields := Fields,
                       tags := Tags,
                       timestamp := Ts}) ->
    TsInfo = ts_column_info(Timeunit, Ts),

    FieldPairs =
        maps:fold(fun(Name, V, Acc) -> [{Name, field_column_info(Name, V)} | Acc] end,
                  [],
                  Fields),

    TagPairs =
        maps:fold(fun(Name, V, Acc) -> [{Name, tag_column_info(Name, V)} | Acc] end, [], Tags),

    % Ensure "tags override fields" semantics: put TagPairs last (in from_list, last occurrence wins)
    % Reverse to maintain original order within fields and tags
    Pairs = [{?TS_COLUMN, TsInfo}] ++ lists:reverse(FieldPairs) ++ lists:reverse(TagPairs),
    maps:from_list(Pairs).

%% @private
%% @doc Merges column information from new columns into existing schema.
%%
%%
%% @param NewColumns New column information
%% @param ExistingColumns Existing accumulated column information
%% @returns Merged column information
merge_column_info(NewColumns, ExistingColumns) ->
    maps:fold(fun(K, V, Acc) ->
                 case maps:is_key(K, Acc) of
                     true ->
                         Acc;  % Column already exists, skip
                     false ->
                         Acc#{K => V}  % Add new column
                 end
              end,
              ExistingColumns,
              NewColumns).

%% @private
%% @doc Sorts columns by semantic type: timestamp, tags, then fields.
%%
%% @param Columns List of {Name, Schema} tuples
%% @returns Sorted list
sort_columns(Columns) ->
    WithKey =
        [{semantic_type_order(Sem), {Name, Schema}}
         || {Name, #{semantic_type := Sem} = Schema} <- Columns],
    Sorted = lists:keysort(1, WithKey),
    [Pair || {_Key, Pair} <- Sorted].

semantic_type_order('TIMESTAMP') ->
    1;
semantic_type_order('TAG') ->
    2;
semantic_type_order('FIELD') ->
    3.

%% @private
%% @doc Build Name -> {Index, SemanticType, DataType} index for fast sparse row fill
index_schema(Schema) ->
    {_, Map} =
        lists:foldl(fun(#{column_name := Name,
                          semantic_type := Sem,
                          datatype := DT},
                        {I, Acc}) ->
                       {I + 1, Acc#{Name => {I, Sem, DT}}}
                    end,
                    {1, #{}},
                    Schema),
    Map.

%% @private
%% @doc Sparse-friendly row builder using the prebuilt IndexMap.
%% Only fills present fields/tags/timestamp; others remain as shared empty map.
point_to_row_sparse(Timeunit, Point0, IndexMap) ->
    Fields = maps:get(fields, Point0, #{}),
    Tags = maps:get(tags, Point0, #{}),
    Ts = maps:get(timestamp, Point0),

    N = maps:size(IndexMap),
    %% Pre‑initialize the default value as an empty map (shared single object to avoid N allocations)
    T0 = erlang:make_tuple(N, #{}),

    %% timestamp
    {TsIdx, 'TIMESTAMP', TsDT} = maps:get(?TS_COLUMN, IndexMap),
    TsVal = ts_row_value(Timeunit, TsDT, Ts),
    T1 = setelement(TsIdx, T0, TsVal),

    %% fields
    T2 = maps:fold(fun(Name, V, AccT) ->
                      case maps:get(Name, IndexMap, undefined) of
                          {Idx, 'FIELD', DT} ->
                              Val = case V of
                                        V when is_map(V) ->
                                            V; % Already in row format
                                        V ->
                                            field_row_value(DT, V)
                                    end,
                              setelement(Idx, AccT, Val);
                          _ ->
                              AccT
                      end
                   end,
                   T1,
                   Fields),

    %% tags
    T3 = maps:fold(fun(Name, V, AccT) ->
                      case maps:get(Name, IndexMap, undefined) of
                          {Idx, 'TAG', DT} ->
                              Val = case V of
                                        V when is_map(V) ->
                                            V; % Already in row format
                                        V ->
                                            tag_row_value(DT, V)
                                    end,
                              setelement(Idx, AccT, Val);
                          _ ->
                              AccT
                      end
                   end,
                   T2,
                   Tags),

    #{values => erlang:tuple_to_list(T3)}.

%% Column info functions (for schema creation)

ts_column_info(_Timeunit, Ts) when is_map(Ts) ->
    % Infer datatype from the value_data structure
    DataType = infer_timestamp_datatype(Ts),
    #{column_name => ?TS_COLUMN,
      semantic_type => 'TIMESTAMP',
      datatype => DataType};
ts_column_info(Timeunit, _Ts) ->
    DataType = ts_datatype(Timeunit),
    #{column_name => ?TS_COLUMN,
      semantic_type => 'TIMESTAMP',
      datatype => DataType}.

field_column_info(Name, V) when is_map(V) ->
    DataType = infer_datatype(V),
    #{column_name => Name,
      semantic_type => 'FIELD',
      datatype => DataType};
field_column_info(Name, _V) ->
    #{column_name => Name,
      semantic_type => 'FIELD',
      datatype => 'FLOAT64'}.

tag_column_info(Name, V) when is_map(V) ->
    DataType = infer_datatype(V),
    #{column_name => Name,
      semantic_type => 'TAG',
      datatype => DataType};
tag_column_info(Name, _V) ->
    #{column_name => Name,
      semantic_type => 'TAG',
      datatype => 'STRING'}.

%% Row value functions (for data conversion)

ts_row_value(_Timeunit, _DataType, Ts) when is_map(Ts) ->
    Ts;  % Assume already in row format from greptimedb_values:timestamp_xxx_value/1
ts_row_value(Timeunit, DataType, Ts) ->
    ts_value_by_datatype(DataType, Timeunit, Ts).

ts_value_by_datatype('TIMESTAMP_NANOSECOND', _Timeunit, Ts) ->
    greptimedb_values:timestamp_nanosecond_value(Ts);
ts_value_by_datatype('TIMESTAMP_MICROSECOND', _Timeunit, Ts) ->
    greptimedb_values:timestamp_microsecond_value(Ts);
ts_value_by_datatype('TIMESTAMP_MILLISECOND', _Timeunit, Ts) ->
    greptimedb_values:timestamp_millisecond_value(Ts);
ts_value_by_datatype('TIMESTAMP_SECOND', _Timeunit, Ts) ->
    greptimedb_values:timestamp_second_value(Ts);
ts_value_by_datatype(_, Timeunit, Ts) ->
    % Fallback to timeunit-based conversion
    ts_value(Timeunit, Ts).

field_row_value('FLOAT64', V) ->
    greptimedb_values:float64_value(V);
field_row_value('INT32', V) ->
    greptimedb_values:int32_value(V);
field_row_value('INT64', V) ->
    greptimedb_values:int64_value(V);
field_row_value('UINT32', V) ->
    greptimedb_values:uint32_value(V);
field_row_value('UINT64', V) ->
    greptimedb_values:uint64_value(V);
field_row_value('BOOLEAN', V) ->
    greptimedb_values:boolean_value(V);
field_row_value('STRING', V) ->
    greptimedb_values:string_value(V);
field_row_value('BINARY', V) ->
    greptimedb_values:binary_value(V);
field_row_value('DATE', V) ->
    greptimedb_values:date_value(V);
field_row_value('DATETIME', V) ->
    greptimedb_values:datetime_value(V);
field_row_value(_, V) ->
    % Default to FLOAT64
    greptimedb_values:float64_value(V).

tag_row_value('STRING', V) ->
    greptimedb_values:string_value(V);
tag_row_value('INT32', V) ->
    greptimedb_values:int32_value(V);
tag_row_value('INT64', V) ->
    greptimedb_values:int64_value(V);
tag_row_value('UINT32', V) ->
    greptimedb_values:uint32_value(V);
tag_row_value('UINT64', V) ->
    greptimedb_values:uint64_value(V);
tag_row_value('BINARY', V) ->
    greptimedb_values:binary_value(V);
tag_row_value(_, V) ->
    % Default to STRING
    greptimedb_values:string_value(V).

%% @private
%% @doc Determines timestamp datatype based on time unit.
%%
%% @param Timeunit Time unit specification
%% @returns Datatype atom
ts_datatype(ns) ->
    'TIMESTAMP_NANOSECOND';
ts_datatype(nanosecond) ->
    'TIMESTAMP_NANOSECOND';
ts_datatype(us) ->
    'TIMESTAMP_MICROSECOND';
ts_datatype(microsecond) ->
    'TIMESTAMP_MICROSECOND';
ts_datatype(ms) ->
    'TIMESTAMP_MILLISECOND';
ts_datatype(millisecond) ->
    'TIMESTAMP_MILLISECOND';
ts_datatype(s) ->
    'TIMESTAMP_SECOND';
ts_datatype(second) ->
    'TIMESTAMP_SECOND'.

%% @private
%% @doc Converts timestamp to appropriate typed value based on time unit.
%%
%% @param Timeunit Time unit specification
%% @param Ts Timestamp value
%% @returns Map with typed timestamp value
ts_value(ns, Ts) ->
    greptimedb_values:timestamp_nanosecond_value(Ts);
ts_value(nanosecond, Ts) ->
    greptimedb_values:timestamp_nanosecond_value(Ts);
ts_value(us, Ts) ->
    greptimedb_values:timestamp_microsecond_value(Ts);
ts_value(microsecond, Ts) ->
    greptimedb_values:timestamp_microsecond_value(Ts);
ts_value(ms, Ts) ->
    greptimedb_values:timestamp_millisecond_value(Ts);
ts_value(millisecond, Ts) ->
    greptimedb_values:timestamp_millisecond_value(Ts);
ts_value(s, Ts) ->
    greptimedb_values:timestamp_second_value(Ts);
ts_value(second, Ts) ->
    greptimedb_values:timestamp_second_value(Ts).

%% @private
%% @doc Infers timestamp datatype from the value structure.
%%
%% @param Value Map containing value_data
%% @returns Datatype atom
infer_timestamp_datatype(#{value_data := {Type, _Value}}) ->
    case Type of
        timestamp_nanosecond_value ->
            'TIMESTAMP_NANOSECOND';
        timestamp_microsecond_value ->
            'TIMESTAMP_MICROSECOND';
        timestamp_millisecond_value ->
            'TIMESTAMP_MILLISECOND';
        timestamp_second_value ->
            'TIMESTAMP_SECOND';
        _ ->
            'TIMESTAMP_MILLISECOND' % Default
    end.

%% @private
%% @doc Infers datatype from the value structure.
%%
%% @param Value Map containing value_data
%% @returns Datatype atom
infer_datatype(#{value_data := {Type, _Value}}) ->
    case Type of
        i8_value ->
            'INT8';
        i16_value ->
            'INT16';
        i32_value ->
            'INT32';
        i64_value ->
            'INT64';
        u8_value ->
            'UINT8';
        u16_value ->
            'UINT16';
        u32_value ->
            'UINT32';
        u64_value ->
            'UINT64';
        f32_value ->
            'FLOAT32';
        f64_value ->
            'FLOAT64';
        bool_value ->
            'BOOLEAN';
        binary_value ->
            'BINARY';
        string_value ->
            'STRING';
        date_value ->
            'DATE';
        datetime_value ->
            'DATETIME';
        timestamp_nanosecond_value ->
            'TIMESTAMP_NANOSECOND';
        timestamp_microsecond_value ->
            'TIMESTAMP_MICROSECOND';
        timestamp_millisecond_value ->
            'TIMESTAMP_MILLISECOND';
        timestamp_second_value ->
            'TIMESTAMP_SECOND';
        _ ->
            'STRING' % Default
    end.
