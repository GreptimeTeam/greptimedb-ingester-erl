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

-export([int32_value/1, int64_value/1, float32_value/1, float64_value/1, boolean_value/1, binary_value/1,
         string_value/1, date_value/1, datetime_value/1, timestamp_second_value/1, uint32_value/1,
         uint64_value/1, timestamp_millisecond_value/1, timestamp_microsecond_value/1,
         timestamp_nanosecond_value/1, decimal128_value/4, json_value/1, json2_value/1]).

-define(INT64_MIN, -(1 bsl 63)).
-define(UINT64_MAX, (1 bsl 64) - 1).

int32_value(V) ->
    #{value_data => {i32_value, V}}.

int64_value(V) ->
    #{value_data => {i64_value, V}}.

uint32_value(V) ->
    #{value_data => {u32_value, V}}.

uint64_value(V) ->
    #{value_data => {u64_value, V}}.

float32_value(V) ->
    #{value_data => {f32_value, V}}.

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

%% @doc Builds a Decimal128 value with the required precision and scale.
%%
%% GreptimeDB stores DECIMAL128 as a 128-bit signed integer split into two
%% int64 halves (`Hi' upper 64 bits, `Lo' lower 64 bits). To reconstruct the
%% logical value, mask each half to an unsigned 64-bit chunk, combine into a
%% 128-bit pattern, then reinterpret as signed two's-complement — Erlang
%% integers are arbitrary-precision, so a raw `bor' with a negative `Lo'
%% sign-extends and yields the wrong value. Equivalent Erlang:
%% ```
%%   Mask   = (1 bsl 64) - 1,
%%   Raw    = ((Hi band Mask) bsl 64) bor (Lo band Mask),
%%   Int128 = if Raw >= (1 bsl 127) -> Raw - (1 bsl 128); true -> Raw end,
%%   Value  = Int128 / math:pow(10, Scale).   %% float result; use a bignum/decimal lib for exact precision
%% '''
%%
%% `Precision' and `Scale' are carried on the value map so the encoder can
%% populate the column's datatype_extension in the row-based schema. They are
%% stripped from the protobuf-encoded Value message (the server reads them
%% from the schema, not the value).
decimal128_value(Hi, Lo, Precision, Scale)
  when is_integer(Hi), is_integer(Lo),
       is_integer(Precision), Precision > 0, Precision =< 38,
       is_integer(Scale), Scale >= 0, Scale =< Precision ->
    #{value_data => {decimal128_value, #{hi => Hi, lo => Lo}},
      precision => Precision,
      scale => Scale}.

%% @doc Builds a value for a legacy JSON column from encoded JSON text.
%%
%% The text is sent as is and parsed by the server; invalid JSON fails the
%% whole request. `json_type' tells the encoder to declare the column as JSON
%% and is stripped from the protobuf-encoded Value message.
json_value(Json) ->
    #{value_data => {string_value, iolist_to_binary(Json)},
      json_type => 'JSON_BINARY'}.

%% @doc Builds a value for a JSON2 column from a decoded JSON term.
%%
%% The term uses the same representation as OTP's `json:decode/1': maps with
%% binary keys, lists, binaries, integers, floats, `true', `false' and `null'.
%% The top level must be a map. Integers must fit in int64 or uint64. To write
%% SQL NULL, omit the field from the point. JSON2 values are only supported in
%% fields.
json2_value(Object) when is_map(Object) ->
    #{value_data => {json_value, encode_json(Object)}};
json2_value(Other) ->
    erlang:error({invalid_json2_value, #{reason => expected_object, value => Other}}).

encode_json(null) ->
    #{};
encode_json(true) ->
    #{value => {boolean, true}};
encode_json(false) ->
    #{value => {boolean, false}};
encode_json(V) when is_binary(V) ->
    #{value => {str, V}};
encode_json(V) when is_float(V) ->
    #{value => {float, V}};
encode_json(V) when is_integer(V), V >= 0, V =< ?UINT64_MAX ->
    #{value => {uint, V}};
encode_json(V) when is_integer(V), V < 0, V >= ?INT64_MIN ->
    #{value => {int, V}};
encode_json(V) when is_list(V) ->
    #{value => {array, #{items => [encode_json(Item) || Item <- V]}}};
encode_json(V) when is_map(V) ->
    Entries = maps:fold(fun(K, Item, Acc) -> [#{key => json_key(K), value => encode_json(Item)} | Acc] end,
                        [],
                        V),
    #{value => {object, #{entries => Entries}}};
encode_json(V) ->
    erlang:error({invalid_json2_value, #{reason => unsupported_term, value => V}}).

json_key(K) when is_binary(K) ->
    K;
json_key(K) ->
    erlang:error({invalid_json2_value, #{reason => invalid_key, value => K}}).
