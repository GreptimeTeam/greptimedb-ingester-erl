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
insert_requests(Client, [{Table, Points} | T], PrevDbName, Inserts) ->
    {DbName, Insert} = insert_request(Table, Points),
    case PrevDbName of
        unknown ->
            insert_requests(Client, T, DbName, [Insert | Inserts]);
        Name when Name == DbName ->
            insert_requests(Client, T, Name, [Insert | Inserts])
    end.

insert_request({DbName, Table}, Points) ->
    RowCount = length(Points),
    Columns =
        lists:map(fun(Column) -> pad_null_mask(Column, RowCount) end, collect_columns(Points)),
    {DbName,
     #{table_name => Table,
       columns => Columns,
       row_count => RowCount}};
insert_request(Table, Points) ->
    insert_request({?DEFAULT_DBNAME, Table}, Points).

%%%===================================================================
%%% Internal functions
%%%===================================================================
collect_columns(Points) ->
    collect_columns(Points, []).

collect_columns([], Columns) ->
    merge_columns(Columns);
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
values_size(#{ts_microsecond_values := Values}) ->
    length(Values);
values_size(#{ts_nanosecond_values := Values}) ->
    length(Values).

merge_values(V1, V2) when map_size(V1) == 0 ->
    V2;
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
merge_values(#{ts_second_values := V1} = L, #{ts_second_values := V2}) ->
    L#{ts_second_values := [V2 | V1]};
merge_values(#{ts_millisecond_values := V1} = L, #{ts_millisecond_values := V2}) ->
    L#{ts_millisecond_values := [V2 | V1]};
merge_values(#{ts_microsecond_values := V1} = L, #{ts_microsecond_values := V2}) ->
    L#{ts_microsecond_values := [V2 | V1]};
merge_values(#{ts_nanosecond_values := V1} = L, #{ts_nanosecond_values := V2}) ->
    L#{ts_nanosecond_values := [V2 | V1]}.

pad_null_mask(#{values := Values, null_mask := NullMask} = Column, RowCount) ->
    ValuesSize = values_size(Values),
    NewColumn =
        if ValuesSize == RowCount ->
               maps:remove(null_mask, Column);
           true ->
               Pad = 8 - (bit_size(NullMask) - floor(bit_size(NullMask) / 8) * 8),
               Column#{null_mask := <<0:Pad/integer, NullMask/bits>>}
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

merge_columns(NextColumns, Columns) ->
    lists:map(fun({Name, Column}) -> {Name, merge_column(Column, Name, NextColumns)} end,
              Columns).

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
