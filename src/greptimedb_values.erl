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

-module(greptimedb_values).

-export([int32_value/1, int64_value/1, float64_value/1, boolean_value/1, binary_value/1,
         string_value/1, date_value/1, datetime_value/1, timestamp_second_value/1,
         timestamp_millisecond_value/1, timestamp_microsecond_value/1,
         timestamp_nanosecond_value/1]).

int32_value(V) ->
    #{values => #{i32_values => [V]}, datatype => 'INT32'}.

int64_value(V) ->
    #{values => #{i64_values => [V]}, datatype => 'INT64'}.

float64_value(V) ->
    #{values => #{f64_values => [V]}, datatype => 'FLOAT64'}.

boolean_value(V) ->
    #{values => #{bool_values => [V]}, datatype => 'BOOLEAN'}.

binary_value(V) ->
    #{values => #{binary_values => [V]}, datatype => 'BINARY'}.

string_value(V) ->
    #{values => #{string_values => [V]}, datatype => 'STRING'}.

date_value(V) ->
    #{values => #{date_values => [V]}, datatype => 'DATE'}.

datetime_value(V) ->
    #{values => #{datetime_values => [V]}, datatype => 'DATETIME'}.

timestamp_second_value(V) ->
    #{values => #{ts_second_values => [V]}, datatype => 'TIMESTAMP_SECOND'}.

timestamp_millisecond_value(V) ->
    #{values => #{ts_millisecond_values => [V]}, datatype => 'TIMESTAMP_MILLISECOND'}.

timestamp_microsecond_value(V) ->
    #{values => #{ts_microsecond_values => [V]}, datatype => 'TIMESTAMP_MICROSECOND'}.

timestamp_nanosecond_value(V) ->
    #{values => #{ts_nanosecond_values => [V]}, datatype => 'TIMESTAMP_NANOSECOND'}.
