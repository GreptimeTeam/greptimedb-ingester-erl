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

%% @doc Converts a batch of data points into GreptimeDB gRPC insert requests.
%%
%% This function transforms Erlang data structures into the protobuf format
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
%%   - `request`: Tuple {inserts, #{inserts => [...]}} with column data
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
    #{header => Header, request => {inserts, #{inserts => Inserts}}};
insert_requests(#{cli_opts := Options} = Client,
                [{Metric, Points} | T],
                PrevDbName,
                Inserts) ->
    {DbName, Insert} = make_insert_request(Options, metric(Options, Metric), Points),
    case PrevDbName of
        unknown ->
            insert_requests(Client, T, DbName, [Insert | Inserts]);
        Name when Name == DbName ->
            insert_requests(Client, T, Name, [Insert | Inserts])
    end.

make_insert_request(_Options,
                    #{dbname := DbName,
                      table := Table,
                      timeunit := Timeunit},
                    Points) ->
    RowCount = length(Points),
    Columns =
        lists:map(fun(Column) -> pad_null_mask(Column, RowCount) end,
                  collect_columns(Timeunit, Points)),
    {DbName,
     #{table_name => Table,
       columns => Columns,
       row_count => RowCount}}.

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
%% @doc Collects and merges columns from all data points.
%%
%% Iterates through all points, extracting columns from each and merging them
%% into a unified column structure. Handles sparse data by tracking null values.
%%
%% @param Timeunit Time unit for timestamp columns (ns, us, ms, s)
%% @param Points List of data points
%% @returns List of merged column structures
collect_columns(Timeunit, Points) ->
    collect_columns(Timeunit, Points, []).

%% @private
%% @doc Recursive helper for collect_columns/2.
collect_columns(_Timeunit, [], Columns) ->
    merge_columns(Columns);
collect_columns(Timeunit, [Point | T], Columns) ->
    collect_columns(Timeunit, T, [convert_columns(Timeunit, Point) | Columns]).

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
values_size(#{timestamp_second_values := Values}) ->
    length(Values);
values_size(#{timestamp_millisecond_values := Values}) ->
    length(Values);
values_size(#{timestamp_microsecond_values := Values}) ->
    length(Values);
values_size(#{timestamp_nanosecond_values := Values}) ->
    length(Values).

merge_values(#{i8_values := V1} = L, #{i8_values := V2}) ->
    L#{i8_values := [V2 | V1]};
merge_values(#{i16_values := V1} = L, #{i16_values := V2}) ->
    L#{i16_values := [V2 | V1]};
merge_values(#{i32_values := V1} = L, #{i32_values := V2}) ->
    L#{i32_values := [V2 | V1]};
merge_values(#{i64_values := V1} = L, #{i64_values := V2}) ->
    L#{i64_values := [V2 | V1]};
merge_values(#{u8_values := V1} = L, #{u8_values := V2}) ->
    L#{u8_values := [V2 | V1]};
merge_values(#{u16_values := V1} = L, #{u16_values := V2}) ->
    L#{u16_values := [V2 | V1]};
merge_values(#{u32_values := V1} = L, #{u32_values := V2}) ->
    L#{u32_values := [V2 | V1]};
merge_values(#{u64_values := V1} = L, #{u64_values := V2}) ->
    L#{u64_values := [V2 | V1]};
merge_values(#{f32_values := V1} = L, #{f32_values := V2}) ->
    L#{f32_values := [V2 | V1]};
merge_values(#{f64_values := V1} = L, #{f64_values := V2}) ->
    L#{f64_values := [V2 | V1]};
merge_values(#{bool_values := V1} = L, #{bool_values := V2}) ->
    L#{bool_values := [V2 | V1]};
merge_values(#{binary_values := V1} = L, #{binary_values := V2}) ->
    L#{binary_values := [V2 | V1]};
merge_values(#{string_values := V1} = L, #{string_values := V2}) ->
    L#{string_values := [V2 | V1]};
merge_values(#{date_values := V1} = L, #{date_values := V2}) ->
    L#{date_values := [V2 | V1]};
merge_values(#{timestamp_second_values := V1} = L, #{timestamp_second_values := V2}) ->
    L#{timestamp_second_values := [V2 | V1]};
merge_values(#{timestamp_millisecond_values := V1} = L,
             #{timestamp_millisecond_values := V2}) ->
    L#{timestamp_millisecond_values := [V2 | V1]};
merge_values(#{timestamp_microsecond_values := V1} = L,
             #{timestamp_microsecond_values := V2}) ->
    L#{timestamp_microsecond_values := [V2 | V1]};
merge_values(#{timestamp_nanosecond_values := V1} = L,
             #{timestamp_nanosecond_values := V2}) ->
    L#{timestamp_nanosecond_values := [V2 | V1]};
merge_values(V1, V2) when map_size(V1) == 0 ->
    V2.

%% @private
%% @doc Pads null mask to byte boundary or removes it if not needed.
%%
%% If a column has values for all rows, the null mask is removed.
%% Otherwise, the null mask is padded with zeros to align to byte boundaries
%% as required by the GreptimeDB protocol.
%%
%% @param Column Column map with values and null_mask
%% @param RowCount Total number of rows in the batch
%% @returns Updated column with adjusted null mask
pad_null_mask(#{values := Values, null_mask := NullMask} = Column, RowCount) ->
    case values_size(Values) of
        RowCount ->
            maps:remove(null_mask, Column);
        _ ->
            BitSize = bit_size(NullMask),
            PadBits = (8 - BitSize rem 8) rem 8,
            Column#{null_mask := <<0:PadBits, NullMask/bits>>}
    end.

%% @private
%% @doc Converts a single point into column structures.
%%
%% Transforms fields, tags, and timestamp from a point into column maps
%% keyed by column name. Each column includes semantic type and values.
%%
%% @param Timeunit Time unit for timestamp conversion
%% @param Point Map with fields, tags, and timestamp
%% @returns Map of column_name -> column structure
convert_columns(Timeunit,
                #{fields := Fields,
                  tags := Tags,
                  timestamp := Ts}) ->
    TsColumn = ts_column(Timeunit, Ts),
    FieldColumns = maps:map(fun(K, V) -> field_column(K, V) end, Fields),
    TagColumns = maps:map(fun(K, V) -> tag_column(K, V) end, Tags),
    maps:put(
        maps:get(column_name, TsColumn), TsColumn, maps:merge(FieldColumns, TagColumns)).

%% @private
%% @doc Merges a column with data from the next row.
%%
%% Updates the null mask to track whether the column has a value in this row:
%% - 1 bit: column has a value in this row
%% - 0 bit: column is null/missing in this row
%%
%% @param Column Existing column accumulator with null mask
%% @param Name Column name to look for in NextColumns
%% @param NextColumns Map of columns from the current row being processed
%% @returns Updated column with merged values and extended null mask
merge_column(#{null_mask := NullMask} = Column, Name, NextColumns) ->
    case NextColumns of
        #{Name := NewColumn} ->
            Values = maps:get(values, Column, #{}),
            NewValues = maps:get(values, NewColumn),
            MergedValues = merge_values(Values, NewValues),
            case map_size(Column) of
                1 ->
                    NewColumn#{values := MergedValues, null_mask => <<NullMask/bits, 1:1/integer>>};
                _ ->
                    Column#{values := MergedValues, null_mask := <<NullMask/bits, 1:1/integer>>}
            end;
        _ ->
            Column#{null_mask := <<NullMask/bits, 0:1/integer>>}
    end.

%% @private
%% @doc Merges columns from a new row into the accumulator.
%%
%% This is the fold function used by merge_columns/1. It processes one row
%% at a time, calling merge_column/3 for each column in the accumulator.
%%
%% @param NextColumns Columns from the current row
%% @param Columns Accumulator of {Name, Column} tuples
%% @returns Updated accumulator with merged columns
merge_columns(NextColumns, Columns) ->
    lists:map(fun({Name, Column}) -> {Name, merge_column(Column, Name, NextColumns)} end,
              Columns).

%% @private
%% @doc Flattens nested lists of values.
%%
%% Used to flatten the accumulated value lists after merging.
%% Values are accumulated as nested lists during merging and need
%% to be flattened into a single list for the final column structure.
%%
%% @param L Nested list structure
%% @returns Flattened list
flatten([H]) ->
    [H];
flatten([[H] | T]) ->
    flatten(T, [H]).

flatten([], Acc) ->
    Acc;
flatten([H], Acc) ->
    [H | Acc];
flatten([[H] | T], Acc) ->
    flatten(T, [H | Acc]).

%% @private
%% @doc Merges columns from all rows into a unified structure.
%%
%% Creates a column for every unique column name across all rows,
%% tracking null values where columns are missing in specific rows.
%%
%% Example with 3 rows having different columns:
%% ```
%% Row 1: #{"temp" => 20, "humidity" => 60}
%% Row 2: #{"temp" => 22}                      % missing humidity
%% Row 3: #{"temp" => 21, "pressure" => 1013}  % missing humidity, has pressure
%%
%% Result after merging:
%% - temp:     values=[20,22,21], null_mask=<<1:1,1:1,1:1>> (all rows have it)
%% - humidity: values=[60],       null_mask=<<1:1,0:1,0:1>> (only row 1 has it)
%% - pressure: values=[1013],     null_mask=<<0:1,0:1,1:1>> (only row 3 has it)
%% ```
%%
%% The null mask bitfield: 1=has value, 0=null/missing, read left-to-right for rows.
%%
%% The process:
%% 1. Collects all unique column names (temp, humidity, pressure)
%% 2. Creates empty columns with null masks
%% 3. Merges data from each row using merge_columns/2 and merge_column/3
%% 4. Flattens the accumulated value lists
%%
%% @param Columns List of column maps from all rows
%% @returns List of merged columns with complete data and null masks
merge_columns(Columns) ->
    Names =
        sets:to_list(
            sets:from_list(
                lists:flatten(
                    lists:map(fun(C) -> maps:keys(C) end, Columns)))),
    EmptyColumns = lists:map(fun(Name) -> {Name, #{null_mask => <<>>}} end, Names),
    lists:map(fun({_Name, Column}) ->
                 maps:update_with(values,
                                  fun(Values) -> maps:map(fun(_K, VS) -> flatten(VS) end, Values)
                                  end,
                                  Column)
              end,
              lists:foldl(fun merge_columns/2, EmptyColumns, lists:reverse(Columns))).

ts_column(_Timeunit, Ts) when is_map(Ts) ->
    maps:merge(#{column_name => ?TS_COLUMN, semantic_type => 'TIMESTAMP'}, Ts);
ts_column(Timeunit, Ts) ->
    TsValue = ts_value(Timeunit, Ts),
    TsValue#{column_name => ?TS_COLUMN, semantic_type => 'TIMESTAMP'}.

%% @private
%% @doc Converts timestamp to appropriate typed value based on time unit.
%%
%% Supports both short and long time unit names:
%% - ns/nanosecond
%% - us/microsecond
%% - ms/millisecond (default)
%% - s/second
%%
%% @param Timeunit Time unit specification
%% @param Ts Timestamp value
%% @returns Map with typed timestamp value and datatype
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
