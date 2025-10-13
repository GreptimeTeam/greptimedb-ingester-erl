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
         string_value/1, date_value/1, datetime_value/1, timestamp_second_value/1, uint32_value/1,
         uint64_value/1, timestamp_millisecond_value/1, timestamp_microsecond_value/1,
         timestamp_nanosecond_value/1]).

int32_value(V) ->
    #{value_data => {i32_value, V}}.

int64_value(V) ->
    #{value_data => {i64_value, V}}.

uint32_value(V) ->
    #{value_data => {u32_value, V}}.

uint64_value(V) ->
    #{value_data => {u64_value, V}}.

float64_value(V) ->
    #{value_data => {f64_value, V}}.

boolean_value(V) ->
    #{value_data => {bool_value, V}}.

binary_value(V) ->
    #{value_data => {binary_value, V}}.

string_value(V) ->
    #{value_data => {string_value, V}}.

date_value(V) ->
    #{value_data => {date_value, V}}.

datetime_value(V) ->
    #{value_data => {datetime_value, V}}.

timestamp_second_value(V) ->
    #{value_data => {timestamp_second_value, V}}.

timestamp_millisecond_value(V) ->
    #{value_data => {timestamp_millisecond_value, V}}.

timestamp_microsecond_value(V) ->
    #{value_data => {timestamp_microsecond_value, V}}.

timestamp_nanosecond_value(V) ->
    #{value_data => {timestamp_nanosecond_value, V}}.
