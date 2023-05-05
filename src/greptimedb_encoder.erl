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

-export([insert_request/3]).

-define(TS_COLUMN, <<"greptime_timestamp">>).
-define(DEFAULT_DBNAME, "greptime-public").

insert_request(#{cli_opts := Options} = _Client, {DbName, Table}, Points) ->
    RowCount = length(Points),
    Columns =
        lists:map(fun(Column) -> pad_null_mask(Column, RowCount) end, collect_columns(Points)),
    AuthHeader = proplists:get_value(auth, Options, {}),
    Header =
        case AuthHeader of
            {} ->
                #{dbname => DbName};
            Scheme ->
                #{dbname => DbName,
                  authorization => #{auth_scheme => Scheme}}
        end,

    Request =
        {insert,
         #{table_name => Table,
           columns => Columns,
           row_count => RowCount}},
    #{header => Header, request => Request};
insert_request(Client, Table, Points) ->
    insert_request(Client, {?DEFAULT_DBNAME, Table}, Points).

%%%===================================================================
%%% Internal functions
%%%===================================================================
collect_columns(Points) ->
    collect_columns(Points, []).

collect_columns([], Columns) ->
    maps:values(merge_columns(Columns));
collect_columns([Point | T], Columns) ->
    collect_columns(T, [convert_columns(Point) | Columns]).

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
    lists:foldr(fun merge_columns/2, EmptyColumns, Columns).

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
