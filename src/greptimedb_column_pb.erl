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

-module(greptimedb_column_pb).

-export([encode_msg/2, encode_msg/3]).
-export([decode_msg/2, decode_msg/3]).
-export([merge_msgs/3, merge_msgs/4]).
-export([verify_msg/2, verify_msg/3]).
-export([get_msg_defs/0]).
-export([get_msg_names/0]).
-export([get_group_names/0]).
-export([get_msg_or_group_names/0]).
-export([get_enum_names/0]).
-export([find_msg_def/1, fetch_msg_def/1]).
-export([find_enum_def/1, fetch_enum_def/1]).
-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).
-export(['enum_symbol_by_value_greptime.v1.SemanticType'/1, 'enum_value_by_symbol_greptime.v1.SemanticType'/1]).
-export(['enum_symbol_by_value_greptime.v1.ColumnDataType'/1, 'enum_value_by_symbol_greptime.v1.ColumnDataType'/1]).
-export([get_service_names/0]).
-export([get_service_def/1]).
-export([get_rpc_names/1]).
-export([find_rpc_def/2, fetch_rpc_def/2]).
-export([fqbin_to_service_name/1]).
-export([service_name_to_fqbin/1]).
-export([fqbins_to_service_and_rpc_name/2]).
-export([service_and_rpc_name_to_fqbins/2]).
-export([fqbin_to_msg_name/1]).
-export([msg_name_to_fqbin/1]).
-export([fqbin_to_enum_name/1]).
-export([enum_name_to_fqbin/1]).
-export([get_package_name/0]).
-export([uses_packages/0]).
-export([source_basename/0]).
-export([get_all_source_basenames/0]).
-export([get_all_proto_names/0]).
-export([get_msg_containment/1]).
-export([get_pkg_containment/1]).
-export([get_service_containment/1]).
-export([get_rpc_containment/1]).
-export([get_enum_containment/1]).
-export([get_proto_by_msg_name_as_fqbin/1]).
-export([get_proto_by_service_name_as_fqbin/1]).
-export([get_proto_by_enum_name_as_fqbin/1]).
-export([get_protos_by_pkg_name_as_fqbin/1]).
-export([gpb_version_as_string/0, gpb_version_as_list/0]).
-export([gpb_version_source/0]).


%% enumerated types
-type 'greptime.v1.SemanticType'() :: 'TAG' | 'FIELD' | 'TIMESTAMP'.
-type 'greptime.v1.ColumnDataType'() :: 'BOOLEAN' | 'INT8' | 'INT16' | 'INT32' | 'INT64' | 'UINT8' | 'UINT16' | 'UINT32' | 'UINT64' | 'FLOAT32' | 'FLOAT64' | 'BINARY' | 'STRING' | 'DATE' | 'DATETIME' | 'TIMESTAMP_SECOND' | 'TIMESTAMP_MILLISECOND' | 'TIMESTAMP_MICROSECOND' | 'TIMESTAMP_NANOSECOND' | 'TIME_SECOND' | 'TIME_MILLISECOND' | 'TIME_MICROSECOND' | 'TIME_NANOSECOND' | 'INTERVAL_YEAR_MONTH' | 'INTERVAL_DAY_TIME' | 'INTERVAL_MONTH_DAY_NANO' | 'DURATION_SECOND' | 'DURATION_MILLISECOND' | 'DURATION_MICROSECOND' | 'DURATION_NANOSECOND' | 'DECIMAL128'.
-export_type(['greptime.v1.SemanticType'/0, 'greptime.v1.ColumnDataType'/0]).

%% message types
-type values() ::
      #{i8_values               => [integer()],     % = 1, repeated, 32 bits
        i16_values              => [integer()],     % = 2, repeated, 32 bits
        i32_values              => [integer()],     % = 3, repeated, 32 bits
        i64_values              => [integer()],     % = 4, repeated, 64 bits
        u8_values               => [non_neg_integer()], % = 5, repeated, 32 bits
        u16_values              => [non_neg_integer()], % = 6, repeated, 32 bits
        u32_values              => [non_neg_integer()], % = 7, repeated, 32 bits
        u64_values              => [non_neg_integer()], % = 8, repeated, 64 bits
        f32_values              => [float() | integer() | infinity | '-infinity' | nan], % = 9, repeated
        f64_values              => [float() | integer() | infinity | '-infinity' | nan], % = 10, repeated
        bool_values             => [boolean() | 0 | 1], % = 11, repeated
        binary_values           => [iodata()],      % = 12, repeated
        string_values           => [unicode:chardata()], % = 13, repeated
        date_values             => [integer()],     % = 14, repeated, 32 bits
        datetime_values         => [integer()],     % = 15, repeated, 64 bits
        timestamp_second_values => [integer()],     % = 16, repeated, 64 bits
        timestamp_millisecond_values => [integer()], % = 17, repeated, 64 bits
        timestamp_microsecond_values => [integer()], % = 18, repeated, 64 bits
        timestamp_nanosecond_values => [integer()], % = 19, repeated, 64 bits
        time_second_values      => [integer()],     % = 20, repeated, 64 bits
        time_millisecond_values => [integer()],     % = 21, repeated, 64 bits
        time_microsecond_values => [integer()],     % = 22, repeated, 64 bits
        time_nanosecond_values  => [integer()],     % = 23, repeated, 64 bits
        interval_year_month_values => [integer()],  % = 24, repeated, 32 bits
        interval_day_time_values => [integer()],    % = 25, repeated, 64 bits
        interval_month_day_nano_values => [interval_month_day_nano()], % = 26, repeated
        duration_second_values  => [integer()],     % = 27, repeated, 64 bits
        duration_millisecond_values => [integer()], % = 28, repeated, 64 bits
        duration_microsecond_values => [integer()], % = 29, repeated, 64 bits
        duration_nanosecond_values => [integer()],  % = 30, repeated, 64 bits
        decimal128_values       => [decimal_128()]  % = 31, repeated
       }.

-type column() ::
      #{column_name             => unicode:chardata(), % = 1, optional
        semantic_type           => 'TAG' | 'FIELD' | 'TIMESTAMP' | integer(), % = 2, optional, enum greptime.v1.SemanticType
        values                  => values(),        % = 3, optional
        null_mask               => iodata(),        % = 4, optional
        datatype                => 'BOOLEAN' | 'INT8' | 'INT16' | 'INT32' | 'INT64' | 'UINT8' | 'UINT16' | 'UINT32' | 'UINT64' | 'FLOAT32' | 'FLOAT64' | 'BINARY' | 'STRING' | 'DATE' | 'DATETIME' | 'TIMESTAMP_SECOND' | 'TIMESTAMP_MILLISECOND' | 'TIMESTAMP_MICROSECOND' | 'TIMESTAMP_NANOSECOND' | 'TIME_SECOND' | 'TIME_MILLISECOND' | 'TIME_MICROSECOND' | 'TIME_NANOSECOND' | 'INTERVAL_YEAR_MONTH' | 'INTERVAL_DAY_TIME' | 'INTERVAL_MONTH_DAY_NANO' | 'DURATION_SECOND' | 'DURATION_MILLISECOND' | 'DURATION_MICROSECOND' | 'DURATION_NANOSECOND' | 'DECIMAL128' | integer(), % = 5, optional, enum greptime.v1.ColumnDataType
        datatype_extension      => column_data_type_extension() % = 6, optional
       }.

-type request_header() ::
      #{catalog                 => unicode:chardata(), % = 1, optional
        schema                  => unicode:chardata(), % = 2, optional
        authorization           => auth_header(),   % = 3, optional
        dbname                  => unicode:chardata(), % = 4, optional
        tracing_context         => #{unicode:chardata() => unicode:chardata()} % = 5
       }.

-type response_header() ::
      #{status                  => status()         % = 1, optional
       }.

-type status() ::
      #{status_code             => non_neg_integer(), % = 1, optional, 32 bits
        err_msg                 => unicode:chardata() % = 2, optional
       }.

-type auth_header() ::
      #{auth_scheme             => {basic, basic()} | {token, token()} % oneof
       }.

-type basic() ::
      #{username                => unicode:chardata(), % = 1, optional
        password                => unicode:chardata() % = 2, optional
       }.

-type token() ::
      #{token                   => unicode:chardata() % = 1, optional
       }.

-type affected_rows() ::
      #{value                   => non_neg_integer() % = 1, optional, 32 bits
       }.

-type flight_metadata() ::
      #{affected_rows           => affected_rows()  % = 1, optional
       }.

-type interval_month_day_nano() ::
      #{months                  => integer(),       % = 1, optional, 32 bits
        days                    => integer(),       % = 2, optional, 32 bits
        nanoseconds             => integer()        % = 3, optional, 64 bits
       }.

-type decimal_128() ::
      #{hi                      => integer(),       % = 1, optional, 64 bits
        lo                      => integer()        % = 2, optional, 64 bits
       }.

-type column_data_type_extension() ::
      #{type_ext                => {decimal_type, decimal_type_extension()} % oneof
       }.

-type decimal_type_extension() ::
      #{precision               => integer(),       % = 1, optional, 32 bits
        scale                   => integer()        % = 2, optional, 32 bits
       }.

-export_type(['values'/0, 'column'/0, 'request_header'/0, 'response_header'/0, 'status'/0, 'auth_header'/0, 'basic'/0, 'token'/0, 'affected_rows'/0, 'flight_metadata'/0, 'interval_month_day_nano'/0, 'decimal_128'/0, 'column_data_type_extension'/0, 'decimal_type_extension'/0]).
-type '$msg_name'() :: values | column | request_header | response_header | status | auth_header | basic | token | affected_rows | flight_metadata | interval_month_day_nano | decimal_128 | column_data_type_extension | decimal_type_extension.
-type '$msg'() :: values() | column() | request_header() | response_header() | status() | auth_header() | basic() | token() | affected_rows() | flight_metadata() | interval_month_day_nano() | decimal_128() | column_data_type_extension() | decimal_type_extension().
-export_type(['$msg_name'/0, '$msg'/0]).

-if(?OTP_RELEASE >= 24).
-dialyzer({no_underspecs, encode_msg/2}).
-endif.
-spec encode_msg('$msg'(), '$msg_name'()) -> binary().
encode_msg(Msg, MsgName) when is_atom(MsgName) -> encode_msg(Msg, MsgName, []).

-if(?OTP_RELEASE >= 24).
-dialyzer({no_underspecs, encode_msg/3}).
-endif.
-spec encode_msg('$msg'(), '$msg_name'(), list()) -> binary().
encode_msg(Msg, MsgName, Opts) ->
    case proplists:get_bool(verify, Opts) of
        true -> verify_msg(Msg, MsgName, Opts);
        false -> ok
    end,
    TrUserData = proplists:get_value(user_data, Opts),
    case MsgName of
        values -> encode_msg_values(id(Msg, TrUserData), TrUserData);
        column -> encode_msg_column(id(Msg, TrUserData), TrUserData);
        request_header -> encode_msg_request_header(id(Msg, TrUserData), TrUserData);
        response_header -> encode_msg_response_header(id(Msg, TrUserData), TrUserData);
        status -> encode_msg_status(id(Msg, TrUserData), TrUserData);
        auth_header -> encode_msg_auth_header(id(Msg, TrUserData), TrUserData);
        basic -> encode_msg_basic(id(Msg, TrUserData), TrUserData);
        token -> encode_msg_token(id(Msg, TrUserData), TrUserData);
        affected_rows -> encode_msg_affected_rows(id(Msg, TrUserData), TrUserData);
        flight_metadata -> encode_msg_flight_metadata(id(Msg, TrUserData), TrUserData);
        interval_month_day_nano -> encode_msg_interval_month_day_nano(id(Msg, TrUserData), TrUserData);
        decimal_128 -> encode_msg_decimal_128(id(Msg, TrUserData), TrUserData);
        column_data_type_extension -> encode_msg_column_data_type_extension(id(Msg, TrUserData), TrUserData);
        decimal_type_extension -> encode_msg_decimal_type_extension(id(Msg, TrUserData), TrUserData)
    end.


encode_msg_values(Msg, TrUserData) -> encode_msg_values(Msg, <<>>, TrUserData).


encode_msg_values(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{i8_values := F1} ->
                 TrF1 = id(F1, TrUserData),
                 if TrF1 == [] -> Bin;
                    true -> e_field_values_i8_values(TrF1, Bin, TrUserData)
                 end;
             _ -> Bin
         end,
    B2 = case M of
             #{i16_values := F2} ->
                 TrF2 = id(F2, TrUserData),
                 if TrF2 == [] -> B1;
                    true -> e_field_values_i16_values(TrF2, B1, TrUserData)
                 end;
             _ -> B1
         end,
    B3 = case M of
             #{i32_values := F3} ->
                 TrF3 = id(F3, TrUserData),
                 if TrF3 == [] -> B2;
                    true -> e_field_values_i32_values(TrF3, B2, TrUserData)
                 end;
             _ -> B2
         end,
    B4 = case M of
             #{i64_values := F4} ->
                 TrF4 = id(F4, TrUserData),
                 if TrF4 == [] -> B3;
                    true -> e_field_values_i64_values(TrF4, B3, TrUserData)
                 end;
             _ -> B3
         end,
    B5 = case M of
             #{u8_values := F5} ->
                 TrF5 = id(F5, TrUserData),
                 if TrF5 == [] -> B4;
                    true -> e_field_values_u8_values(TrF5, B4, TrUserData)
                 end;
             _ -> B4
         end,
    B6 = case M of
             #{u16_values := F6} ->
                 TrF6 = id(F6, TrUserData),
                 if TrF6 == [] -> B5;
                    true -> e_field_values_u16_values(TrF6, B5, TrUserData)
                 end;
             _ -> B5
         end,
    B7 = case M of
             #{u32_values := F7} ->
                 TrF7 = id(F7, TrUserData),
                 if TrF7 == [] -> B6;
                    true -> e_field_values_u32_values(TrF7, B6, TrUserData)
                 end;
             _ -> B6
         end,
    B8 = case M of
             #{u64_values := F8} ->
                 TrF8 = id(F8, TrUserData),
                 if TrF8 == [] -> B7;
                    true -> e_field_values_u64_values(TrF8, B7, TrUserData)
                 end;
             _ -> B7
         end,
    B9 = case M of
             #{f32_values := F9} ->
                 TrF9 = id(F9, TrUserData),
                 if TrF9 == [] -> B8;
                    true -> e_field_values_f32_values(TrF9, B8, TrUserData)
                 end;
             _ -> B8
         end,
    B10 = case M of
              #{f64_values := F10} ->
                  TrF10 = id(F10, TrUserData),
                  if TrF10 == [] -> B9;
                     true -> e_field_values_f64_values(TrF10, B9, TrUserData)
                  end;
              _ -> B9
          end,
    B11 = case M of
              #{bool_values := F11} ->
                  TrF11 = id(F11, TrUserData),
                  if TrF11 == [] -> B10;
                     true -> e_field_values_bool_values(TrF11, B10, TrUserData)
                  end;
              _ -> B10
          end,
    B12 = case M of
              #{binary_values := F12} ->
                  TrF12 = id(F12, TrUserData),
                  if TrF12 == [] -> B11;
                     true -> e_field_values_binary_values(TrF12, B11, TrUserData)
                  end;
              _ -> B11
          end,
    B13 = case M of
              #{string_values := F13} ->
                  TrF13 = id(F13, TrUserData),
                  if TrF13 == [] -> B12;
                     true -> e_field_values_string_values(TrF13, B12, TrUserData)
                  end;
              _ -> B12
          end,
    B14 = case M of
              #{date_values := F14} ->
                  TrF14 = id(F14, TrUserData),
                  if TrF14 == [] -> B13;
                     true -> e_field_values_date_values(TrF14, B13, TrUserData)
                  end;
              _ -> B13
          end,
    B15 = case M of
              #{datetime_values := F15} ->
                  TrF15 = id(F15, TrUserData),
                  if TrF15 == [] -> B14;
                     true -> e_field_values_datetime_values(TrF15, B14, TrUserData)
                  end;
              _ -> B14
          end,
    B16 = case M of
              #{timestamp_second_values := F16} ->
                  TrF16 = id(F16, TrUserData),
                  if TrF16 == [] -> B15;
                     true -> e_field_values_timestamp_second_values(TrF16, B15, TrUserData)
                  end;
              _ -> B15
          end,
    B17 = case M of
              #{timestamp_millisecond_values := F17} ->
                  TrF17 = id(F17, TrUserData),
                  if TrF17 == [] -> B16;
                     true -> e_field_values_timestamp_millisecond_values(TrF17, B16, TrUserData)
                  end;
              _ -> B16
          end,
    B18 = case M of
              #{timestamp_microsecond_values := F18} ->
                  TrF18 = id(F18, TrUserData),
                  if TrF18 == [] -> B17;
                     true -> e_field_values_timestamp_microsecond_values(TrF18, B17, TrUserData)
                  end;
              _ -> B17
          end,
    B19 = case M of
              #{timestamp_nanosecond_values := F19} ->
                  TrF19 = id(F19, TrUserData),
                  if TrF19 == [] -> B18;
                     true -> e_field_values_timestamp_nanosecond_values(TrF19, B18, TrUserData)
                  end;
              _ -> B18
          end,
    B20 = case M of
              #{time_second_values := F20} ->
                  TrF20 = id(F20, TrUserData),
                  if TrF20 == [] -> B19;
                     true -> e_field_values_time_second_values(TrF20, B19, TrUserData)
                  end;
              _ -> B19
          end,
    B21 = case M of
              #{time_millisecond_values := F21} ->
                  TrF21 = id(F21, TrUserData),
                  if TrF21 == [] -> B20;
                     true -> e_field_values_time_millisecond_values(TrF21, B20, TrUserData)
                  end;
              _ -> B20
          end,
    B22 = case M of
              #{time_microsecond_values := F22} ->
                  TrF22 = id(F22, TrUserData),
                  if TrF22 == [] -> B21;
                     true -> e_field_values_time_microsecond_values(TrF22, B21, TrUserData)
                  end;
              _ -> B21
          end,
    B23 = case M of
              #{time_nanosecond_values := F23} ->
                  TrF23 = id(F23, TrUserData),
                  if TrF23 == [] -> B22;
                     true -> e_field_values_time_nanosecond_values(TrF23, B22, TrUserData)
                  end;
              _ -> B22
          end,
    B24 = case M of
              #{interval_year_month_values := F24} ->
                  TrF24 = id(F24, TrUserData),
                  if TrF24 == [] -> B23;
                     true -> e_field_values_interval_year_month_values(TrF24, B23, TrUserData)
                  end;
              _ -> B23
          end,
    B25 = case M of
              #{interval_day_time_values := F25} ->
                  TrF25 = id(F25, TrUserData),
                  if TrF25 == [] -> B24;
                     true -> e_field_values_interval_day_time_values(TrF25, B24, TrUserData)
                  end;
              _ -> B24
          end,
    B26 = case M of
              #{interval_month_day_nano_values := F26} ->
                  TrF26 = id(F26, TrUserData),
                  if TrF26 == [] -> B25;
                     true -> e_field_values_interval_month_day_nano_values(TrF26, B25, TrUserData)
                  end;
              _ -> B25
          end,
    B27 = case M of
              #{duration_second_values := F27} ->
                  TrF27 = id(F27, TrUserData),
                  if TrF27 == [] -> B26;
                     true -> e_field_values_duration_second_values(TrF27, B26, TrUserData)
                  end;
              _ -> B26
          end,
    B28 = case M of
              #{duration_millisecond_values := F28} ->
                  TrF28 = id(F28, TrUserData),
                  if TrF28 == [] -> B27;
                     true -> e_field_values_duration_millisecond_values(TrF28, B27, TrUserData)
                  end;
              _ -> B27
          end,
    B29 = case M of
              #{duration_microsecond_values := F29} ->
                  TrF29 = id(F29, TrUserData),
                  if TrF29 == [] -> B28;
                     true -> e_field_values_duration_microsecond_values(TrF29, B28, TrUserData)
                  end;
              _ -> B28
          end,
    B30 = case M of
              #{duration_nanosecond_values := F30} ->
                  TrF30 = id(F30, TrUserData),
                  if TrF30 == [] -> B29;
                     true -> e_field_values_duration_nanosecond_values(TrF30, B29, TrUserData)
                  end;
              _ -> B29
          end,
    case M of
        #{decimal128_values := F31} ->
            TrF31 = id(F31, TrUserData),
            if TrF31 == [] -> B30;
               true -> e_field_values_decimal128_values(TrF31, B30, TrUserData)
            end;
        _ -> B30
    end.

encode_msg_column(Msg, TrUserData) -> encode_msg_column(Msg, <<>>, TrUserData).


encode_msg_column(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{column_name := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     case is_empty_string(TrF1) of
                         true -> Bin;
                         false -> e_type_string(TrF1, <<Bin/binary, 10>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    B2 = case M of
             #{semantic_type := F2} ->
                 begin
                     TrF2 = id(F2, TrUserData),
                     if TrF2 =:= 'TAG'; TrF2 =:= 0 -> B1;
                        true -> 'e_enum_greptime.v1.SemanticType'(TrF2, <<B1/binary, 16>>, TrUserData)
                     end
                 end;
             _ -> B1
         end,
    B3 = case M of
             #{values := F3} ->
                 begin
                     TrF3 = id(F3, TrUserData),
                     if TrF3 =:= undefined -> B2;
                        true -> e_mfield_column_values(TrF3, <<B2/binary, 26>>, TrUserData)
                     end
                 end;
             _ -> B2
         end,
    B4 = case M of
             #{null_mask := F4} ->
                 begin
                     TrF4 = id(F4, TrUserData),
                     case iolist_size(TrF4) of
                         0 -> B3;
                         _ -> e_type_bytes(TrF4, <<B3/binary, 34>>, TrUserData)
                     end
                 end;
             _ -> B3
         end,
    B5 = case M of
             #{datatype := F5} ->
                 begin
                     TrF5 = id(F5, TrUserData),
                     if TrF5 =:= 'BOOLEAN'; TrF5 =:= 0 -> B4;
                        true -> 'e_enum_greptime.v1.ColumnDataType'(TrF5, <<B4/binary, 40>>, TrUserData)
                     end
                 end;
             _ -> B4
         end,
    case M of
        #{datatype_extension := F6} ->
            begin
                TrF6 = id(F6, TrUserData),
                if TrF6 =:= undefined -> B5;
                   true -> e_mfield_column_datatype_extension(TrF6, <<B5/binary, 50>>, TrUserData)
                end
            end;
        _ -> B5
    end.

encode_msg_request_header(Msg, TrUserData) -> encode_msg_request_header(Msg, <<>>, TrUserData).


encode_msg_request_header(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{catalog := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     case is_empty_string(TrF1) of
                         true -> Bin;
                         false -> e_type_string(TrF1, <<Bin/binary, 10>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    B2 = case M of
             #{schema := F2} ->
                 begin
                     TrF2 = id(F2, TrUserData),
                     case is_empty_string(TrF2) of
                         true -> B1;
                         false -> e_type_string(TrF2, <<B1/binary, 18>>, TrUserData)
                     end
                 end;
             _ -> B1
         end,
    B3 = case M of
             #{authorization := F3} ->
                 begin
                     TrF3 = id(F3, TrUserData),
                     if TrF3 =:= undefined -> B2;
                        true -> e_mfield_request_header_authorization(TrF3, <<B2/binary, 26>>, TrUserData)
                     end
                 end;
             _ -> B2
         end,
    B4 = case M of
             #{dbname := F4} ->
                 begin
                     TrF4 = id(F4, TrUserData),
                     case is_empty_string(TrF4) of
                         true -> B3;
                         false -> e_type_string(TrF4, <<B3/binary, 34>>, TrUserData)
                     end
                 end;
             _ -> B3
         end,
    case M of
        #{tracing_context := F5} ->
            TrF5 = 'tr_encode_request_header.tracing_context'(F5, TrUserData),
            if TrF5 == [] -> B4;
               true -> e_field_request_header_tracing_context(TrF5, B4, TrUserData)
            end;
        _ -> B4
    end.

encode_msg_response_header(Msg, TrUserData) -> encode_msg_response_header(Msg, <<>>, TrUserData).


encode_msg_response_header(#{} = M, Bin, TrUserData) ->
    case M of
        #{status := F1} ->
            begin
                TrF1 = id(F1, TrUserData),
                if TrF1 =:= undefined -> Bin;
                   true -> e_mfield_response_header_status(TrF1, <<Bin/binary, 10>>, TrUserData)
                end
            end;
        _ -> Bin
    end.

encode_msg_status(Msg, TrUserData) -> encode_msg_status(Msg, <<>>, TrUserData).


encode_msg_status(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{status_code := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     if TrF1 =:= 0 -> Bin;
                        true -> e_varint(TrF1, <<Bin/binary, 8>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    case M of
        #{err_msg := F2} ->
            begin
                TrF2 = id(F2, TrUserData),
                case is_empty_string(TrF2) of
                    true -> B1;
                    false -> e_type_string(TrF2, <<B1/binary, 18>>, TrUserData)
                end
            end;
        _ -> B1
    end.

encode_msg_auth_header(Msg, TrUserData) -> encode_msg_auth_header(Msg, <<>>, TrUserData).


encode_msg_auth_header(#{} = M, Bin, TrUserData) ->
    case M of
        #{auth_scheme := F1} ->
            case id(F1, TrUserData) of
                {basic, TF1} -> begin TrTF1 = id(TF1, TrUserData), e_mfield_auth_header_basic(TrTF1, <<Bin/binary, 10>>, TrUserData) end;
                {token, TF1} -> begin TrTF1 = id(TF1, TrUserData), e_mfield_auth_header_token(TrTF1, <<Bin/binary, 18>>, TrUserData) end
            end;
        _ -> Bin
    end.

encode_msg_basic(Msg, TrUserData) -> encode_msg_basic(Msg, <<>>, TrUserData).


encode_msg_basic(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{username := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     case is_empty_string(TrF1) of
                         true -> Bin;
                         false -> e_type_string(TrF1, <<Bin/binary, 10>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    case M of
        #{password := F2} ->
            begin
                TrF2 = id(F2, TrUserData),
                case is_empty_string(TrF2) of
                    true -> B1;
                    false -> e_type_string(TrF2, <<B1/binary, 18>>, TrUserData)
                end
            end;
        _ -> B1
    end.

encode_msg_token(Msg, TrUserData) -> encode_msg_token(Msg, <<>>, TrUserData).


encode_msg_token(#{} = M, Bin, TrUserData) ->
    case M of
        #{token := F1} ->
            begin
                TrF1 = id(F1, TrUserData),
                case is_empty_string(TrF1) of
                    true -> Bin;
                    false -> e_type_string(TrF1, <<Bin/binary, 10>>, TrUserData)
                end
            end;
        _ -> Bin
    end.

encode_msg_affected_rows(Msg, TrUserData) -> encode_msg_affected_rows(Msg, <<>>, TrUserData).


encode_msg_affected_rows(#{} = M, Bin, TrUserData) ->
    case M of
        #{value := F1} ->
            begin
                TrF1 = id(F1, TrUserData),
                if TrF1 =:= 0 -> Bin;
                   true -> e_varint(TrF1, <<Bin/binary, 8>>, TrUserData)
                end
            end;
        _ -> Bin
    end.

encode_msg_flight_metadata(Msg, TrUserData) -> encode_msg_flight_metadata(Msg, <<>>, TrUserData).


encode_msg_flight_metadata(#{} = M, Bin, TrUserData) ->
    case M of
        #{affected_rows := F1} ->
            begin
                TrF1 = id(F1, TrUserData),
                if TrF1 =:= undefined -> Bin;
                   true -> e_mfield_flight_metadata_affected_rows(TrF1, <<Bin/binary, 10>>, TrUserData)
                end
            end;
        _ -> Bin
    end.

encode_msg_interval_month_day_nano(Msg, TrUserData) -> encode_msg_interval_month_day_nano(Msg, <<>>, TrUserData).


encode_msg_interval_month_day_nano(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{months := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     if TrF1 =:= 0 -> Bin;
                        true -> e_type_int32(TrF1, <<Bin/binary, 8>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    B2 = case M of
             #{days := F2} ->
                 begin
                     TrF2 = id(F2, TrUserData),
                     if TrF2 =:= 0 -> B1;
                        true -> e_type_int32(TrF2, <<B1/binary, 16>>, TrUserData)
                     end
                 end;
             _ -> B1
         end,
    case M of
        #{nanoseconds := F3} ->
            begin
                TrF3 = id(F3, TrUserData),
                if TrF3 =:= 0 -> B2;
                   true -> e_type_int64(TrF3, <<B2/binary, 24>>, TrUserData)
                end
            end;
        _ -> B2
    end.

encode_msg_decimal_128(Msg, TrUserData) -> encode_msg_decimal_128(Msg, <<>>, TrUserData).


encode_msg_decimal_128(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{hi := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     if TrF1 =:= 0 -> Bin;
                        true -> e_type_int64(TrF1, <<Bin/binary, 8>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    case M of
        #{lo := F2} ->
            begin
                TrF2 = id(F2, TrUserData),
                if TrF2 =:= 0 -> B1;
                   true -> e_type_int64(TrF2, <<B1/binary, 16>>, TrUserData)
                end
            end;
        _ -> B1
    end.

encode_msg_column_data_type_extension(Msg, TrUserData) -> encode_msg_column_data_type_extension(Msg, <<>>, TrUserData).


encode_msg_column_data_type_extension(#{} = M, Bin, TrUserData) ->
    case M of
        #{type_ext := F1} -> case id(F1, TrUserData) of {decimal_type, TF1} -> begin TrTF1 = id(TF1, TrUserData), e_mfield_column_data_type_extension_decimal_type(TrTF1, <<Bin/binary, 10>>, TrUserData) end end;
        _ -> Bin
    end.

encode_msg_decimal_type_extension(Msg, TrUserData) -> encode_msg_decimal_type_extension(Msg, <<>>, TrUserData).


encode_msg_decimal_type_extension(#{} = M, Bin, TrUserData) ->
    B1 = case M of
             #{precision := F1} ->
                 begin
                     TrF1 = id(F1, TrUserData),
                     if TrF1 =:= 0 -> Bin;
                        true -> e_type_int32(TrF1, <<Bin/binary, 8>>, TrUserData)
                     end
                 end;
             _ -> Bin
         end,
    case M of
        #{scale := F2} ->
            begin
                TrF2 = id(F2, TrUserData),
                if TrF2 =:= 0 -> B1;
                   true -> e_type_int32(TrF2, <<B1/binary, 16>>, TrUserData)
                end
            end;
        _ -> B1
    end.

e_field_values_i8_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_i8_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 10>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_i8_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_i8_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int32(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_i8_values(Rest, Bin2, TrUserData);
e_pfield_values_i8_values([], Bin, _TrUserData) -> Bin.

e_field_values_i16_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_i16_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 18>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_i16_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_i16_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int32(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_i16_values(Rest, Bin2, TrUserData);
e_pfield_values_i16_values([], Bin, _TrUserData) -> Bin.

e_field_values_i32_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_i32_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 26>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_i32_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_i32_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int32(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_i32_values(Rest, Bin2, TrUserData);
e_pfield_values_i32_values([], Bin, _TrUserData) -> Bin.

e_field_values_i64_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_i64_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 34>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_i64_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_i64_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_i64_values(Rest, Bin2, TrUserData);
e_pfield_values_i64_values([], Bin, _TrUserData) -> Bin.

e_field_values_u8_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_u8_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 42>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_u8_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_u8_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_varint(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_u8_values(Rest, Bin2, TrUserData);
e_pfield_values_u8_values([], Bin, _TrUserData) -> Bin.

e_field_values_u16_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_u16_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 50>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_u16_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_u16_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_varint(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_u16_values(Rest, Bin2, TrUserData);
e_pfield_values_u16_values([], Bin, _TrUserData) -> Bin.

e_field_values_u32_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_u32_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 58>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_u32_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_u32_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_varint(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_u32_values(Rest, Bin2, TrUserData);
e_pfield_values_u32_values([], Bin, _TrUserData) -> Bin.

e_field_values_u64_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_u64_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 66>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_u64_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_u64_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_varint(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_u64_values(Rest, Bin2, TrUserData);
e_pfield_values_u64_values([], Bin, _TrUserData) -> Bin.

e_field_values_f32_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    Bin2 = <<Bin/binary, 74>>,
    Bin3 = e_varint(length(Elems) * 4, Bin2),
    e_pfield_values_f32_values(Elems, Bin3, TrUserData);
e_field_values_f32_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_f32_values([V | Rest], Bin, TrUserData) ->
    TrV = id(V, TrUserData),
    Bin2 = if is_number(TrV) -> <<Bin/binary, TrV:32/little-float>>;
              TrV =:= infinity -> <<Bin/binary, 0:16, 128, 127>>;
              TrV =:= '-infinity' -> <<Bin/binary, 0:16, 128, 255>>;
              TrV =:= nan -> <<Bin/binary, 0:16, 192, 127>>
           end,
    e_pfield_values_f32_values(Rest, Bin2, TrUserData);
e_pfield_values_f32_values([], Bin, _TrUserData) -> Bin.

e_field_values_f64_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    Bin2 = <<Bin/binary, 82>>,
    Bin3 = e_varint(length(Elems) * 8, Bin2),
    e_pfield_values_f64_values(Elems, Bin3, TrUserData);
e_field_values_f64_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_f64_values([V | Rest], Bin, TrUserData) ->
    TrV = id(V, TrUserData),
    Bin2 = if is_number(TrV) -> <<Bin/binary, TrV:64/float-little>>;
              TrV =:= infinity -> <<Bin/binary, 0:48, 240, 127>>;
              TrV =:= '-infinity' -> <<Bin/binary, 0:48, 240, 255>>;
              TrV =:= nan -> <<Bin/binary, 0:48, 248, 127>>
           end,
    e_pfield_values_f64_values(Rest, Bin2, TrUserData);
e_pfield_values_f64_values([], Bin, _TrUserData) -> Bin.

e_field_values_bool_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_bool_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 90>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_bool_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_bool_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_bool(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_bool_values(Rest, Bin2, TrUserData);
e_pfield_values_bool_values([], Bin, _TrUserData) -> Bin.

e_field_values_binary_values([Elem | Rest], Bin, TrUserData) ->
    Bin2 = <<Bin/binary, 98>>,
    Bin3 = e_type_bytes(id(Elem, TrUserData), Bin2, TrUserData),
    e_field_values_binary_values(Rest, Bin3, TrUserData);
e_field_values_binary_values([], Bin, _TrUserData) -> Bin.

e_field_values_string_values([Elem | Rest], Bin, TrUserData) ->
    Bin2 = <<Bin/binary, 106>>,
    Bin3 = e_type_string(id(Elem, TrUserData), Bin2, TrUserData),
    e_field_values_string_values(Rest, Bin3, TrUserData);
e_field_values_string_values([], Bin, _TrUserData) -> Bin.

e_field_values_date_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_date_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 114>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_date_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_date_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int32(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_date_values(Rest, Bin2, TrUserData);
e_pfield_values_date_values([], Bin, _TrUserData) -> Bin.

e_field_values_datetime_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_datetime_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 122>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_datetime_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_datetime_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_datetime_values(Rest, Bin2, TrUserData);
e_pfield_values_datetime_values([], Bin, _TrUserData) -> Bin.

e_field_values_timestamp_second_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_timestamp_second_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 130, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_timestamp_second_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_timestamp_second_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_timestamp_second_values(Rest, Bin2, TrUserData);
e_pfield_values_timestamp_second_values([], Bin, _TrUserData) -> Bin.

e_field_values_timestamp_millisecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_timestamp_millisecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 138, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_timestamp_millisecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_timestamp_millisecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_timestamp_millisecond_values(Rest, Bin2, TrUserData);
e_pfield_values_timestamp_millisecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_timestamp_microsecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_timestamp_microsecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 146, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_timestamp_microsecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_timestamp_microsecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_timestamp_microsecond_values(Rest, Bin2, TrUserData);
e_pfield_values_timestamp_microsecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_timestamp_nanosecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_timestamp_nanosecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 154, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_timestamp_nanosecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_timestamp_nanosecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_timestamp_nanosecond_values(Rest, Bin2, TrUserData);
e_pfield_values_timestamp_nanosecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_time_second_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_time_second_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 162, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_time_second_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_time_second_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_time_second_values(Rest, Bin2, TrUserData);
e_pfield_values_time_second_values([], Bin, _TrUserData) -> Bin.

e_field_values_time_millisecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_time_millisecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 170, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_time_millisecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_time_millisecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_time_millisecond_values(Rest, Bin2, TrUserData);
e_pfield_values_time_millisecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_time_microsecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_time_microsecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 178, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_time_microsecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_time_microsecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_time_microsecond_values(Rest, Bin2, TrUserData);
e_pfield_values_time_microsecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_time_nanosecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_time_nanosecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 186, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_time_nanosecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_time_nanosecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_time_nanosecond_values(Rest, Bin2, TrUserData);
e_pfield_values_time_nanosecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_interval_year_month_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_interval_year_month_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 194, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_interval_year_month_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_interval_year_month_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int32(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_interval_year_month_values(Rest, Bin2, TrUserData);
e_pfield_values_interval_year_month_values([], Bin, _TrUserData) -> Bin.

e_field_values_interval_day_time_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_interval_day_time_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 202, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_interval_day_time_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_interval_day_time_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_interval_day_time_values(Rest, Bin2, TrUserData);
e_pfield_values_interval_day_time_values([], Bin, _TrUserData) -> Bin.

e_mfield_values_interval_month_day_nano_values(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_interval_month_day_nano(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_field_values_interval_month_day_nano_values([Elem | Rest], Bin, TrUserData) ->
    Bin2 = <<Bin/binary, 210, 1>>,
    Bin3 = e_mfield_values_interval_month_day_nano_values(id(Elem, TrUserData), Bin2, TrUserData),
    e_field_values_interval_month_day_nano_values(Rest, Bin3, TrUserData);
e_field_values_interval_month_day_nano_values([], Bin, _TrUserData) -> Bin.

e_field_values_duration_second_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_duration_second_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 218, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_duration_second_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_duration_second_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_duration_second_values(Rest, Bin2, TrUserData);
e_pfield_values_duration_second_values([], Bin, _TrUserData) -> Bin.

e_field_values_duration_millisecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_duration_millisecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 226, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_duration_millisecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_duration_millisecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_duration_millisecond_values(Rest, Bin2, TrUserData);
e_pfield_values_duration_millisecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_duration_microsecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_duration_microsecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 234, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_duration_microsecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_duration_microsecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_duration_microsecond_values(Rest, Bin2, TrUserData);
e_pfield_values_duration_microsecond_values([], Bin, _TrUserData) -> Bin.

e_field_values_duration_nanosecond_values(Elems, Bin, TrUserData) when Elems =/= [] ->
    SubBin = e_pfield_values_duration_nanosecond_values(Elems, <<>>, TrUserData),
    Bin2 = <<Bin/binary, 242, 1>>,
    Bin3 = e_varint(byte_size(SubBin), Bin2),
    <<Bin3/binary, SubBin/binary>>;
e_field_values_duration_nanosecond_values([], Bin, _TrUserData) -> Bin.

e_pfield_values_duration_nanosecond_values([Value | Rest], Bin, TrUserData) ->
    Bin2 = e_type_int64(id(Value, TrUserData), Bin, TrUserData),
    e_pfield_values_duration_nanosecond_values(Rest, Bin2, TrUserData);
e_pfield_values_duration_nanosecond_values([], Bin, _TrUserData) -> Bin.

e_mfield_values_decimal128_values(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_decimal_128(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_field_values_decimal128_values([Elem | Rest], Bin, TrUserData) ->
    Bin2 = <<Bin/binary, 250, 1>>,
    Bin3 = e_mfield_values_decimal128_values(id(Elem, TrUserData), Bin2, TrUserData),
    e_field_values_decimal128_values(Rest, Bin3, TrUserData);
e_field_values_decimal128_values([], Bin, _TrUserData) -> Bin.

e_mfield_column_values(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_values(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_column_datatype_extension(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_column_data_type_extension(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_request_header_authorization(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_auth_header(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_request_header_tracing_context(Msg, Bin, TrUserData) ->
    SubBin = 'encode_msg_map<string,string>'(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_field_request_header_tracing_context([Elem | Rest], Bin, TrUserData) ->
    Bin2 = <<Bin/binary, 42>>,
    Bin3 = e_mfield_request_header_tracing_context('tr_encode_request_header.tracing_context[x]'(Elem, TrUserData), Bin2, TrUserData),
    e_field_request_header_tracing_context(Rest, Bin3, TrUserData);
e_field_request_header_tracing_context([], Bin, _TrUserData) -> Bin.

e_mfield_response_header_status(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_status(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_auth_header_basic(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_basic(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_auth_header_token(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_token(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_flight_metadata_affected_rows(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_affected_rows(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

e_mfield_column_data_type_extension_decimal_type(Msg, Bin, TrUserData) ->
    SubBin = encode_msg_decimal_type_extension(Msg, <<>>, TrUserData),
    Bin2 = e_varint(byte_size(SubBin), Bin),
    <<Bin2/binary, SubBin/binary>>.

'encode_msg_map<string,string>'(#{key := F1, value := F2}, Bin, TrUserData) ->
    B1 = begin TrF1 = id(F1, TrUserData), e_type_string(TrF1, <<Bin/binary, 10>>, TrUserData) end,
    begin TrF2 = id(F2, TrUserData), e_type_string(TrF2, <<B1/binary, 18>>, TrUserData) end.

'e_enum_greptime.v1.SemanticType'('TAG', Bin, _TrUserData) -> <<Bin/binary, 0>>;
'e_enum_greptime.v1.SemanticType'('FIELD', Bin, _TrUserData) -> <<Bin/binary, 1>>;
'e_enum_greptime.v1.SemanticType'('TIMESTAMP', Bin, _TrUserData) -> <<Bin/binary, 2>>;
'e_enum_greptime.v1.SemanticType'(V, Bin, _TrUserData) -> e_varint(V, Bin).

'e_enum_greptime.v1.ColumnDataType'('BOOLEAN', Bin, _TrUserData) -> <<Bin/binary, 0>>;
'e_enum_greptime.v1.ColumnDataType'('INT8', Bin, _TrUserData) -> <<Bin/binary, 1>>;
'e_enum_greptime.v1.ColumnDataType'('INT16', Bin, _TrUserData) -> <<Bin/binary, 2>>;
'e_enum_greptime.v1.ColumnDataType'('INT32', Bin, _TrUserData) -> <<Bin/binary, 3>>;
'e_enum_greptime.v1.ColumnDataType'('INT64', Bin, _TrUserData) -> <<Bin/binary, 4>>;
'e_enum_greptime.v1.ColumnDataType'('UINT8', Bin, _TrUserData) -> <<Bin/binary, 5>>;
'e_enum_greptime.v1.ColumnDataType'('UINT16', Bin, _TrUserData) -> <<Bin/binary, 6>>;
'e_enum_greptime.v1.ColumnDataType'('UINT32', Bin, _TrUserData) -> <<Bin/binary, 7>>;
'e_enum_greptime.v1.ColumnDataType'('UINT64', Bin, _TrUserData) -> <<Bin/binary, 8>>;
'e_enum_greptime.v1.ColumnDataType'('FLOAT32', Bin, _TrUserData) -> <<Bin/binary, 9>>;
'e_enum_greptime.v1.ColumnDataType'('FLOAT64', Bin, _TrUserData) -> <<Bin/binary, 10>>;
'e_enum_greptime.v1.ColumnDataType'('BINARY', Bin, _TrUserData) -> <<Bin/binary, 11>>;
'e_enum_greptime.v1.ColumnDataType'('STRING', Bin, _TrUserData) -> <<Bin/binary, 12>>;
'e_enum_greptime.v1.ColumnDataType'('DATE', Bin, _TrUserData) -> <<Bin/binary, 13>>;
'e_enum_greptime.v1.ColumnDataType'('DATETIME', Bin, _TrUserData) -> <<Bin/binary, 14>>;
'e_enum_greptime.v1.ColumnDataType'('TIMESTAMP_SECOND', Bin, _TrUserData) -> <<Bin/binary, 15>>;
'e_enum_greptime.v1.ColumnDataType'('TIMESTAMP_MILLISECOND', Bin, _TrUserData) -> <<Bin/binary, 16>>;
'e_enum_greptime.v1.ColumnDataType'('TIMESTAMP_MICROSECOND', Bin, _TrUserData) -> <<Bin/binary, 17>>;
'e_enum_greptime.v1.ColumnDataType'('TIMESTAMP_NANOSECOND', Bin, _TrUserData) -> <<Bin/binary, 18>>;
'e_enum_greptime.v1.ColumnDataType'('TIME_SECOND', Bin, _TrUserData) -> <<Bin/binary, 19>>;
'e_enum_greptime.v1.ColumnDataType'('TIME_MILLISECOND', Bin, _TrUserData) -> <<Bin/binary, 20>>;
'e_enum_greptime.v1.ColumnDataType'('TIME_MICROSECOND', Bin, _TrUserData) -> <<Bin/binary, 21>>;
'e_enum_greptime.v1.ColumnDataType'('TIME_NANOSECOND', Bin, _TrUserData) -> <<Bin/binary, 22>>;
'e_enum_greptime.v1.ColumnDataType'('INTERVAL_YEAR_MONTH', Bin, _TrUserData) -> <<Bin/binary, 23>>;
'e_enum_greptime.v1.ColumnDataType'('INTERVAL_DAY_TIME', Bin, _TrUserData) -> <<Bin/binary, 24>>;
'e_enum_greptime.v1.ColumnDataType'('INTERVAL_MONTH_DAY_NANO', Bin, _TrUserData) -> <<Bin/binary, 25>>;
'e_enum_greptime.v1.ColumnDataType'('DURATION_SECOND', Bin, _TrUserData) -> <<Bin/binary, 26>>;
'e_enum_greptime.v1.ColumnDataType'('DURATION_MILLISECOND', Bin, _TrUserData) -> <<Bin/binary, 27>>;
'e_enum_greptime.v1.ColumnDataType'('DURATION_MICROSECOND', Bin, _TrUserData) -> <<Bin/binary, 28>>;
'e_enum_greptime.v1.ColumnDataType'('DURATION_NANOSECOND', Bin, _TrUserData) -> <<Bin/binary, 29>>;
'e_enum_greptime.v1.ColumnDataType'('DECIMAL128', Bin, _TrUserData) -> <<Bin/binary, 30>>;
'e_enum_greptime.v1.ColumnDataType'(V, Bin, _TrUserData) -> e_varint(V, Bin).

-compile({nowarn_unused_function,e_type_sint/3}).
e_type_sint(Value, Bin, _TrUserData) when Value >= 0 -> e_varint(Value * 2, Bin);
e_type_sint(Value, Bin, _TrUserData) -> e_varint(Value * -2 - 1, Bin).

-compile({nowarn_unused_function,e_type_int32/3}).
e_type_int32(Value, Bin, _TrUserData) when 0 =< Value, Value =< 127 -> <<Bin/binary, Value>>;
e_type_int32(Value, Bin, _TrUserData) ->
    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
    e_varint(N, Bin).

-compile({nowarn_unused_function,e_type_int64/3}).
e_type_int64(Value, Bin, _TrUserData) when 0 =< Value, Value =< 127 -> <<Bin/binary, Value>>;
e_type_int64(Value, Bin, _TrUserData) ->
    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
    e_varint(N, Bin).

-compile({nowarn_unused_function,e_type_bool/3}).
e_type_bool(true, Bin, _TrUserData) -> <<Bin/binary, 1>>;
e_type_bool(false, Bin, _TrUserData) -> <<Bin/binary, 0>>;
e_type_bool(1, Bin, _TrUserData) -> <<Bin/binary, 1>>;
e_type_bool(0, Bin, _TrUserData) -> <<Bin/binary, 0>>.

-compile({nowarn_unused_function,e_type_string/3}).
e_type_string(S, Bin, _TrUserData) ->
    Utf8 = unicode:characters_to_binary(S),
    Bin2 = e_varint(byte_size(Utf8), Bin),
    <<Bin2/binary, Utf8/binary>>.

-compile({nowarn_unused_function,e_type_bytes/3}).
e_type_bytes(Bytes, Bin, _TrUserData) when is_binary(Bytes) ->
    Bin2 = e_varint(byte_size(Bytes), Bin),
    <<Bin2/binary, Bytes/binary>>;
e_type_bytes(Bytes, Bin, _TrUserData) when is_list(Bytes) ->
    BytesBin = iolist_to_binary(Bytes),
    Bin2 = e_varint(byte_size(BytesBin), Bin),
    <<Bin2/binary, BytesBin/binary>>.

-compile({nowarn_unused_function,e_type_fixed32/3}).
e_type_fixed32(Value, Bin, _TrUserData) -> <<Bin/binary, Value:32/little>>.

-compile({nowarn_unused_function,e_type_sfixed32/3}).
e_type_sfixed32(Value, Bin, _TrUserData) -> <<Bin/binary, Value:32/little-signed>>.

-compile({nowarn_unused_function,e_type_fixed64/3}).
e_type_fixed64(Value, Bin, _TrUserData) -> <<Bin/binary, Value:64/little>>.

-compile({nowarn_unused_function,e_type_sfixed64/3}).
e_type_sfixed64(Value, Bin, _TrUserData) -> <<Bin/binary, Value:64/little-signed>>.

-compile({nowarn_unused_function,e_type_float/3}).
e_type_float(V, Bin, _) when is_number(V) -> <<Bin/binary, V:32/little-float>>;
e_type_float(infinity, Bin, _) -> <<Bin/binary, 0:16, 128, 127>>;
e_type_float('-infinity', Bin, _) -> <<Bin/binary, 0:16, 128, 255>>;
e_type_float(nan, Bin, _) -> <<Bin/binary, 0:16, 192, 127>>.

-compile({nowarn_unused_function,e_type_double/3}).
e_type_double(V, Bin, _) when is_number(V) -> <<Bin/binary, V:64/little-float>>;
e_type_double(infinity, Bin, _) -> <<Bin/binary, 0:48, 240, 127>>;
e_type_double('-infinity', Bin, _) -> <<Bin/binary, 0:48, 240, 255>>;
e_type_double(nan, Bin, _) -> <<Bin/binary, 0:48, 248, 127>>.

-compile({nowarn_unused_function,e_unknown_elems/2}).
e_unknown_elems([Elem | Rest], Bin) ->
    BinR = case Elem of
               {varint, FNum, N} ->
                   BinF = e_varint(FNum bsl 3, Bin),
                   e_varint(N, BinF);
               {length_delimited, FNum, Data} ->
                   BinF = e_varint(FNum bsl 3 bor 2, Bin),
                   BinL = e_varint(byte_size(Data), BinF),
                   <<BinL/binary, Data/binary>>;
               {group, FNum, GroupFields} ->
                   Bin1 = e_varint(FNum bsl 3 bor 3, Bin),
                   Bin2 = e_unknown_elems(GroupFields, Bin1),
                   e_varint(FNum bsl 3 bor 4, Bin2);
               {fixed32, FNum, V} ->
                   BinF = e_varint(FNum bsl 3 bor 5, Bin),
                   <<BinF/binary, V:32/little>>;
               {fixed64, FNum, V} ->
                   BinF = e_varint(FNum bsl 3 bor 1, Bin),
                   <<BinF/binary, V:64/little>>
           end,
    e_unknown_elems(Rest, BinR);
e_unknown_elems([], Bin) -> Bin.

-compile({nowarn_unused_function,e_varint/3}).
e_varint(N, Bin, _TrUserData) -> e_varint(N, Bin).

-compile({nowarn_unused_function,e_varint/2}).
e_varint(N, Bin) when N =< 127 -> <<Bin/binary, N>>;
e_varint(N, Bin) ->
    Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
    e_varint(N bsr 7, Bin2).

is_empty_string("") -> true;
is_empty_string(<<>>) -> true;
is_empty_string(L) when is_list(L) -> not string_has_chars(L);
is_empty_string(B) when is_binary(B) -> false.

string_has_chars([C | _]) when is_integer(C) -> true;
string_has_chars([H | T]) ->
    case string_has_chars(H) of
        true -> true;
        false -> string_has_chars(T)
    end;
string_has_chars(B) when is_binary(B), byte_size(B) =/= 0 -> true;
string_has_chars(C) when is_integer(C) -> true;
string_has_chars(<<>>) -> false;
string_has_chars([]) -> false.


decode_msg(Bin, MsgName) when is_binary(Bin) -> decode_msg(Bin, MsgName, []).

decode_msg(Bin, MsgName, Opts) when is_binary(Bin) ->
    TrUserData = proplists:get_value(user_data, Opts),
    decode_msg_1_catch(Bin, MsgName, TrUserData).

-ifdef('OTP_RELEASE').
decode_msg_1_catch(Bin, MsgName, TrUserData) ->
    try decode_msg_2_doit(MsgName, Bin, TrUserData)
    catch
        error:{gpb_error,_}=Reason:StackTrace ->
            erlang:raise(error, Reason, StackTrace);
        Class:Reason:StackTrace -> error({gpb_error,{decoding_failure, {Bin, MsgName, {Class, Reason, StackTrace}}}})
    end.
-else.
decode_msg_1_catch(Bin, MsgName, TrUserData) ->
    try decode_msg_2_doit(MsgName, Bin, TrUserData)
    catch
        error:{gpb_error,_}=Reason ->
            erlang:raise(error, Reason,
                         erlang:get_stacktrace());
        Class:Reason ->
            StackTrace = erlang:get_stacktrace(),
            error({gpb_error,{decoding_failure, {Bin, MsgName, {Class, Reason, StackTrace}}}})
    end.
-endif.

decode_msg_2_doit(values, Bin, TrUserData) -> id(decode_msg_values(Bin, TrUserData), TrUserData);
decode_msg_2_doit(column, Bin, TrUserData) -> id(decode_msg_column(Bin, TrUserData), TrUserData);
decode_msg_2_doit(request_header, Bin, TrUserData) -> id(decode_msg_request_header(Bin, TrUserData), TrUserData);
decode_msg_2_doit(response_header, Bin, TrUserData) -> id(decode_msg_response_header(Bin, TrUserData), TrUserData);
decode_msg_2_doit(status, Bin, TrUserData) -> id(decode_msg_status(Bin, TrUserData), TrUserData);
decode_msg_2_doit(auth_header, Bin, TrUserData) -> id(decode_msg_auth_header(Bin, TrUserData), TrUserData);
decode_msg_2_doit(basic, Bin, TrUserData) -> id(decode_msg_basic(Bin, TrUserData), TrUserData);
decode_msg_2_doit(token, Bin, TrUserData) -> id(decode_msg_token(Bin, TrUserData), TrUserData);
decode_msg_2_doit(affected_rows, Bin, TrUserData) -> id(decode_msg_affected_rows(Bin, TrUserData), TrUserData);
decode_msg_2_doit(flight_metadata, Bin, TrUserData) -> id(decode_msg_flight_metadata(Bin, TrUserData), TrUserData);
decode_msg_2_doit(interval_month_day_nano, Bin, TrUserData) -> id(decode_msg_interval_month_day_nano(Bin, TrUserData), TrUserData);
decode_msg_2_doit(decimal_128, Bin, TrUserData) -> id(decode_msg_decimal_128(Bin, TrUserData), TrUserData);
decode_msg_2_doit(column_data_type_extension, Bin, TrUserData) -> id(decode_msg_column_data_type_extension(Bin, TrUserData), TrUserData);
decode_msg_2_doit(decimal_type_extension, Bin, TrUserData) -> id(decode_msg_decimal_type_extension(Bin, TrUserData), TrUserData).



decode_msg_values(Bin, TrUserData) ->
    dfp_read_field_def_values(Bin,
                              0,
                              0,
                              0,
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              id([], TrUserData),
                              TrUserData).

dfp_read_field_def_values(<<10, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_i8_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<8, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29,
                          F@_30, F@_31, TrUserData) ->
    d_field_values_i8_values(Rest,
                             Z1,
                             Z2,
                             F,
                             F@_1,
                             F@_2,
                             F@_3,
                             F@_4,
                             F@_5,
                             F@_6,
                             F@_7,
                             F@_8,
                             F@_9,
                             F@_10,
                             F@_11,
                             F@_12,
                             F@_13,
                             F@_14,
                             F@_15,
                             F@_16,
                             F@_17,
                             F@_18,
                             F@_19,
                             F@_20,
                             F@_21,
                             F@_22,
                             F@_23,
                             F@_24,
                             F@_25,
                             F@_26,
                             F@_27,
                             F@_28,
                             F@_29,
                             F@_30,
                             F@_31,
                             TrUserData);
dfp_read_field_def_values(<<18, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_i16_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<16, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_i16_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<26, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_i32_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<24, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_i32_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<34, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_i64_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_i64_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<42, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_u8_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<40, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_u8_values(Rest,
                             Z1,
                             Z2,
                             F,
                             F@_1,
                             F@_2,
                             F@_3,
                             F@_4,
                             F@_5,
                             F@_6,
                             F@_7,
                             F@_8,
                             F@_9,
                             F@_10,
                             F@_11,
                             F@_12,
                             F@_13,
                             F@_14,
                             F@_15,
                             F@_16,
                             F@_17,
                             F@_18,
                             F@_19,
                             F@_20,
                             F@_21,
                             F@_22,
                             F@_23,
                             F@_24,
                             F@_25,
                             F@_26,
                             F@_27,
                             F@_28,
                             F@_29,
                             F@_30,
                             F@_31,
                             TrUserData);
dfp_read_field_def_values(<<50, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_u16_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<48, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_u16_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<58, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_u32_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<56, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_u32_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<66, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_u64_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_u64_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<74, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_f32_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<77, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_f32_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<82, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_f64_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<81, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_f64_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
dfp_read_field_def_values(<<90, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_bool_values(Rest,
                                Z1,
                                Z2,
                                F,
                                F@_1,
                                F@_2,
                                F@_3,
                                F@_4,
                                F@_5,
                                F@_6,
                                F@_7,
                                F@_8,
                                F@_9,
                                F@_10,
                                F@_11,
                                F@_12,
                                F@_13,
                                F@_14,
                                F@_15,
                                F@_16,
                                F@_17,
                                F@_18,
                                F@_19,
                                F@_20,
                                F@_21,
                                F@_22,
                                F@_23,
                                F@_24,
                                F@_25,
                                F@_26,
                                F@_27,
                                F@_28,
                                F@_29,
                                F@_30,
                                F@_31,
                                TrUserData);
dfp_read_field_def_values(<<88, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_bool_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<98, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_binary_values(Rest,
                                 Z1,
                                 Z2,
                                 F,
                                 F@_1,
                                 F@_2,
                                 F@_3,
                                 F@_4,
                                 F@_5,
                                 F@_6,
                                 F@_7,
                                 F@_8,
                                 F@_9,
                                 F@_10,
                                 F@_11,
                                 F@_12,
                                 F@_13,
                                 F@_14,
                                 F@_15,
                                 F@_16,
                                 F@_17,
                                 F@_18,
                                 F@_19,
                                 F@_20,
                                 F@_21,
                                 F@_22,
                                 F@_23,
                                 F@_24,
                                 F@_25,
                                 F@_26,
                                 F@_27,
                                 F@_28,
                                 F@_29,
                                 F@_30,
                                 F@_31,
                                 TrUserData);
dfp_read_field_def_values(<<106, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_string_values(Rest,
                                 Z1,
                                 Z2,
                                 F,
                                 F@_1,
                                 F@_2,
                                 F@_3,
                                 F@_4,
                                 F@_5,
                                 F@_6,
                                 F@_7,
                                 F@_8,
                                 F@_9,
                                 F@_10,
                                 F@_11,
                                 F@_12,
                                 F@_13,
                                 F@_14,
                                 F@_15,
                                 F@_16,
                                 F@_17,
                                 F@_18,
                                 F@_19,
                                 F@_20,
                                 F@_21,
                                 F@_22,
                                 F@_23,
                                 F@_24,
                                 F@_25,
                                 F@_26,
                                 F@_27,
                                 F@_28,
                                 F@_29,
                                 F@_30,
                                 F@_31,
                                 TrUserData);
dfp_read_field_def_values(<<114, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_date_values(Rest,
                                Z1,
                                Z2,
                                F,
                                F@_1,
                                F@_2,
                                F@_3,
                                F@_4,
                                F@_5,
                                F@_6,
                                F@_7,
                                F@_8,
                                F@_9,
                                F@_10,
                                F@_11,
                                F@_12,
                                F@_13,
                                F@_14,
                                F@_15,
                                F@_16,
                                F@_17,
                                F@_18,
                                F@_19,
                                F@_20,
                                F@_21,
                                F@_22,
                                F@_23,
                                F@_24,
                                F@_25,
                                F@_26,
                                F@_27,
                                F@_28,
                                F@_29,
                                F@_30,
                                F@_31,
                                TrUserData);
dfp_read_field_def_values(<<112, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_date_values(Rest,
                               Z1,
                               Z2,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
dfp_read_field_def_values(<<122, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_datetime_values(Rest,
                                    Z1,
                                    Z2,
                                    F,
                                    F@_1,
                                    F@_2,
                                    F@_3,
                                    F@_4,
                                    F@_5,
                                    F@_6,
                                    F@_7,
                                    F@_8,
                                    F@_9,
                                    F@_10,
                                    F@_11,
                                    F@_12,
                                    F@_13,
                                    F@_14,
                                    F@_15,
                                    F@_16,
                                    F@_17,
                                    F@_18,
                                    F@_19,
                                    F@_20,
                                    F@_21,
                                    F@_22,
                                    F@_23,
                                    F@_24,
                                    F@_25,
                                    F@_26,
                                    F@_27,
                                    F@_28,
                                    F@_29,
                                    F@_30,
                                    F@_31,
                                    TrUserData);
dfp_read_field_def_values(<<120, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_datetime_values(Rest,
                                   Z1,
                                   Z2,
                                   F,
                                   F@_1,
                                   F@_2,
                                   F@_3,
                                   F@_4,
                                   F@_5,
                                   F@_6,
                                   F@_7,
                                   F@_8,
                                   F@_9,
                                   F@_10,
                                   F@_11,
                                   F@_12,
                                   F@_13,
                                   F@_14,
                                   F@_15,
                                   F@_16,
                                   F@_17,
                                   F@_18,
                                   F@_19,
                                   F@_20,
                                   F@_21,
                                   F@_22,
                                   F@_23,
                                   F@_24,
                                   F@_25,
                                   F@_26,
                                   F@_27,
                                   F@_28,
                                   F@_29,
                                   F@_30,
                                   F@_31,
                                   TrUserData);
dfp_read_field_def_values(<<130, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_timestamp_second_values(Rest,
                                            Z1,
                                            Z2,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
dfp_read_field_def_values(<<128, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_timestamp_second_values(Rest,
                                           Z1,
                                           Z2,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
dfp_read_field_def_values(<<138, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_timestamp_millisecond_values(Rest,
                                                 Z1,
                                                 Z2,
                                                 F,
                                                 F@_1,
                                                 F@_2,
                                                 F@_3,
                                                 F@_4,
                                                 F@_5,
                                                 F@_6,
                                                 F@_7,
                                                 F@_8,
                                                 F@_9,
                                                 F@_10,
                                                 F@_11,
                                                 F@_12,
                                                 F@_13,
                                                 F@_14,
                                                 F@_15,
                                                 F@_16,
                                                 F@_17,
                                                 F@_18,
                                                 F@_19,
                                                 F@_20,
                                                 F@_21,
                                                 F@_22,
                                                 F@_23,
                                                 F@_24,
                                                 F@_25,
                                                 F@_26,
                                                 F@_27,
                                                 F@_28,
                                                 F@_29,
                                                 F@_30,
                                                 F@_31,
                                                 TrUserData);
dfp_read_field_def_values(<<136, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_timestamp_millisecond_values(Rest,
                                                Z1,
                                                Z2,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
dfp_read_field_def_values(<<146, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_timestamp_microsecond_values(Rest,
                                                 Z1,
                                                 Z2,
                                                 F,
                                                 F@_1,
                                                 F@_2,
                                                 F@_3,
                                                 F@_4,
                                                 F@_5,
                                                 F@_6,
                                                 F@_7,
                                                 F@_8,
                                                 F@_9,
                                                 F@_10,
                                                 F@_11,
                                                 F@_12,
                                                 F@_13,
                                                 F@_14,
                                                 F@_15,
                                                 F@_16,
                                                 F@_17,
                                                 F@_18,
                                                 F@_19,
                                                 F@_20,
                                                 F@_21,
                                                 F@_22,
                                                 F@_23,
                                                 F@_24,
                                                 F@_25,
                                                 F@_26,
                                                 F@_27,
                                                 F@_28,
                                                 F@_29,
                                                 F@_30,
                                                 F@_31,
                                                 TrUserData);
dfp_read_field_def_values(<<144, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_timestamp_microsecond_values(Rest,
                                                Z1,
                                                Z2,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
dfp_read_field_def_values(<<154, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_timestamp_nanosecond_values(Rest,
                                                Z1,
                                                Z2,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
dfp_read_field_def_values(<<152, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_timestamp_nanosecond_values(Rest,
                                               Z1,
                                               Z2,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
dfp_read_field_def_values(<<162, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_time_second_values(Rest,
                                       Z1,
                                       Z2,
                                       F,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
dfp_read_field_def_values(<<160, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_time_second_values(Rest,
                                      Z1,
                                      Z2,
                                      F,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
dfp_read_field_def_values(<<170, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_time_millisecond_values(Rest,
                                            Z1,
                                            Z2,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
dfp_read_field_def_values(<<168, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_time_millisecond_values(Rest,
                                           Z1,
                                           Z2,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
dfp_read_field_def_values(<<178, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_time_microsecond_values(Rest,
                                            Z1,
                                            Z2,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
dfp_read_field_def_values(<<176, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_time_microsecond_values(Rest,
                                           Z1,
                                           Z2,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
dfp_read_field_def_values(<<186, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_time_nanosecond_values(Rest,
                                           Z1,
                                           Z2,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
dfp_read_field_def_values(<<184, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_time_nanosecond_values(Rest,
                                          Z1,
                                          Z2,
                                          F,
                                          F@_1,
                                          F@_2,
                                          F@_3,
                                          F@_4,
                                          F@_5,
                                          F@_6,
                                          F@_7,
                                          F@_8,
                                          F@_9,
                                          F@_10,
                                          F@_11,
                                          F@_12,
                                          F@_13,
                                          F@_14,
                                          F@_15,
                                          F@_16,
                                          F@_17,
                                          F@_18,
                                          F@_19,
                                          F@_20,
                                          F@_21,
                                          F@_22,
                                          F@_23,
                                          F@_24,
                                          F@_25,
                                          F@_26,
                                          F@_27,
                                          F@_28,
                                          F@_29,
                                          F@_30,
                                          F@_31,
                                          TrUserData);
dfp_read_field_def_values(<<194, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_interval_year_month_values(Rest,
                                               Z1,
                                               Z2,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
dfp_read_field_def_values(<<192, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_interval_year_month_values(Rest,
                                              Z1,
                                              Z2,
                                              F,
                                              F@_1,
                                              F@_2,
                                              F@_3,
                                              F@_4,
                                              F@_5,
                                              F@_6,
                                              F@_7,
                                              F@_8,
                                              F@_9,
                                              F@_10,
                                              F@_11,
                                              F@_12,
                                              F@_13,
                                              F@_14,
                                              F@_15,
                                              F@_16,
                                              F@_17,
                                              F@_18,
                                              F@_19,
                                              F@_20,
                                              F@_21,
                                              F@_22,
                                              F@_23,
                                              F@_24,
                                              F@_25,
                                              F@_26,
                                              F@_27,
                                              F@_28,
                                              F@_29,
                                              F@_30,
                                              F@_31,
                                              TrUserData);
dfp_read_field_def_values(<<202, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_interval_day_time_values(Rest,
                                             Z1,
                                             Z2,
                                             F,
                                             F@_1,
                                             F@_2,
                                             F@_3,
                                             F@_4,
                                             F@_5,
                                             F@_6,
                                             F@_7,
                                             F@_8,
                                             F@_9,
                                             F@_10,
                                             F@_11,
                                             F@_12,
                                             F@_13,
                                             F@_14,
                                             F@_15,
                                             F@_16,
                                             F@_17,
                                             F@_18,
                                             F@_19,
                                             F@_20,
                                             F@_21,
                                             F@_22,
                                             F@_23,
                                             F@_24,
                                             F@_25,
                                             F@_26,
                                             F@_27,
                                             F@_28,
                                             F@_29,
                                             F@_30,
                                             F@_31,
                                             TrUserData);
dfp_read_field_def_values(<<200, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_interval_day_time_values(Rest,
                                            Z1,
                                            Z2,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
dfp_read_field_def_values(<<210, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_interval_month_day_nano_values(Rest,
                                                  Z1,
                                                  Z2,
                                                  F,
                                                  F@_1,
                                                  F@_2,
                                                  F@_3,
                                                  F@_4,
                                                  F@_5,
                                                  F@_6,
                                                  F@_7,
                                                  F@_8,
                                                  F@_9,
                                                  F@_10,
                                                  F@_11,
                                                  F@_12,
                                                  F@_13,
                                                  F@_14,
                                                  F@_15,
                                                  F@_16,
                                                  F@_17,
                                                  F@_18,
                                                  F@_19,
                                                  F@_20,
                                                  F@_21,
                                                  F@_22,
                                                  F@_23,
                                                  F@_24,
                                                  F@_25,
                                                  F@_26,
                                                  F@_27,
                                                  F@_28,
                                                  F@_29,
                                                  F@_30,
                                                  F@_31,
                                                  TrUserData);
dfp_read_field_def_values(<<218, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_duration_second_values(Rest,
                                           Z1,
                                           Z2,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
dfp_read_field_def_values(<<216, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_duration_second_values(Rest,
                                          Z1,
                                          Z2,
                                          F,
                                          F@_1,
                                          F@_2,
                                          F@_3,
                                          F@_4,
                                          F@_5,
                                          F@_6,
                                          F@_7,
                                          F@_8,
                                          F@_9,
                                          F@_10,
                                          F@_11,
                                          F@_12,
                                          F@_13,
                                          F@_14,
                                          F@_15,
                                          F@_16,
                                          F@_17,
                                          F@_18,
                                          F@_19,
                                          F@_20,
                                          F@_21,
                                          F@_22,
                                          F@_23,
                                          F@_24,
                                          F@_25,
                                          F@_26,
                                          F@_27,
                                          F@_28,
                                          F@_29,
                                          F@_30,
                                          F@_31,
                                          TrUserData);
dfp_read_field_def_values(<<226, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_duration_millisecond_values(Rest,
                                                Z1,
                                                Z2,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
dfp_read_field_def_values(<<224, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_duration_millisecond_values(Rest,
                                               Z1,
                                               Z2,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
dfp_read_field_def_values(<<234, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_duration_microsecond_values(Rest,
                                                Z1,
                                                Z2,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
dfp_read_field_def_values(<<232, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_duration_microsecond_values(Rest,
                                               Z1,
                                               Z2,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
dfp_read_field_def_values(<<242, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_pfield_values_duration_nanosecond_values(Rest,
                                               Z1,
                                               Z2,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
dfp_read_field_def_values(<<240, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_duration_nanosecond_values(Rest,
                                              Z1,
                                              Z2,
                                              F,
                                              F@_1,
                                              F@_2,
                                              F@_3,
                                              F@_4,
                                              F@_5,
                                              F@_6,
                                              F@_7,
                                              F@_8,
                                              F@_9,
                                              F@_10,
                                              F@_11,
                                              F@_12,
                                              F@_13,
                                              F@_14,
                                              F@_15,
                                              F@_16,
                                              F@_17,
                                              F@_18,
                                              F@_19,
                                              F@_20,
                                              F@_21,
                                              F@_22,
                                              F@_23,
                                              F@_24,
                                              F@_25,
                                              F@_26,
                                              F@_27,
                                              F@_28,
                                              F@_29,
                                              F@_30,
                                              F@_31,
                                              TrUserData);
dfp_read_field_def_values(<<250, 1, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    d_field_values_decimal128_values(Rest,
                                     Z1,
                                     Z2,
                                     F,
                                     F@_1,
                                     F@_2,
                                     F@_3,
                                     F@_4,
                                     F@_5,
                                     F@_6,
                                     F@_7,
                                     F@_8,
                                     F@_9,
                                     F@_10,
                                     F@_11,
                                     F@_12,
                                     F@_13,
                                     F@_14,
                                     F@_15,
                                     F@_16,
                                     F@_17,
                                     F@_18,
                                     F@_19,
                                     F@_20,
                                     F@_21,
                                     F@_22,
                                     F@_23,
                                     F@_24,
                                     F@_25,
                                     F@_26,
                                     F@_27,
                                     F@_28,
                                     F@_29,
                                     F@_30,
                                     F@_31,
                                     TrUserData);
dfp_read_field_def_values(<<>>, 0, 0, _, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31, TrUserData) ->
    S1 = #{i8_values => lists_reverse(R1, TrUserData), i16_values => lists_reverse(R2, TrUserData), i32_values => lists_reverse(R3, TrUserData), i64_values => lists_reverse(R4, TrUserData), u8_values => lists_reverse(R5, TrUserData),
           u16_values => lists_reverse(R6, TrUserData), u32_values => lists_reverse(R7, TrUserData), u64_values => lists_reverse(R8, TrUserData), f32_values => lists_reverse(R9, TrUserData), f64_values => lists_reverse(R10, TrUserData),
           bool_values => lists_reverse(R11, TrUserData), binary_values => lists_reverse(R12, TrUserData), string_values => lists_reverse(R13, TrUserData), date_values => lists_reverse(R14, TrUserData), datetime_values => lists_reverse(R15, TrUserData),
           timestamp_second_values => lists_reverse(R16, TrUserData), timestamp_millisecond_values => lists_reverse(R17, TrUserData), timestamp_microsecond_values => lists_reverse(R18, TrUserData), timestamp_nanosecond_values => lists_reverse(R19, TrUserData),
           time_second_values => lists_reverse(R20, TrUserData), time_millisecond_values => lists_reverse(R21, TrUserData), time_microsecond_values => lists_reverse(R22, TrUserData), time_nanosecond_values => lists_reverse(R23, TrUserData),
           interval_year_month_values => lists_reverse(R24, TrUserData), interval_day_time_values => lists_reverse(R25, TrUserData), duration_second_values => lists_reverse(R27, TrUserData), duration_millisecond_values => lists_reverse(R28, TrUserData),
           duration_microsecond_values => lists_reverse(R29, TrUserData), duration_nanosecond_values => lists_reverse(R30, TrUserData)},
    S2 = if R26 == '$undef' -> S1;
            true -> S1#{interval_month_day_nano_values => lists_reverse(R26, TrUserData)}
         end,
    if R31 == '$undef' -> S2;
       true -> S2#{decimal128_values => lists_reverse(R31, TrUserData)}
    end;
dfp_read_field_def_values(Other, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29, F@_30,
                          F@_31, TrUserData) ->
    dg_read_field_def_values(Other,
                             Z1,
                             Z2,
                             F,
                             F@_1,
                             F@_2,
                             F@_3,
                             F@_4,
                             F@_5,
                             F@_6,
                             F@_7,
                             F@_8,
                             F@_9,
                             F@_10,
                             F@_11,
                             F@_12,
                             F@_13,
                             F@_14,
                             F@_15,
                             F@_16,
                             F@_17,
                             F@_18,
                             F@_19,
                             F@_20,
                             F@_21,
                             F@_22,
                             F@_23,
                             F@_24,
                             F@_25,
                             F@_26,
                             F@_27,
                             F@_28,
                             F@_29,
                             F@_30,
                             F@_31,
                             TrUserData).

dg_read_field_def_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                         F@_29, F@_30, F@_31, TrUserData)
    when N < 32 - 7 ->
    dg_read_field_def_values(Rest,
                             N + 7,
                             X bsl N + Acc,
                             F,
                             F@_1,
                             F@_2,
                             F@_3,
                             F@_4,
                             F@_5,
                             F@_6,
                             F@_7,
                             F@_8,
                             F@_9,
                             F@_10,
                             F@_11,
                             F@_12,
                             F@_13,
                             F@_14,
                             F@_15,
                             F@_16,
                             F@_17,
                             F@_18,
                             F@_19,
                             F@_20,
                             F@_21,
                             F@_22,
                             F@_23,
                             F@_24,
                             F@_25,
                             F@_26,
                             F@_27,
                             F@_28,
                             F@_29,
                             F@_30,
                             F@_31,
                             TrUserData);
dg_read_field_def_values(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                         F@_29, F@_30, F@_31, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 ->
            d_pfield_values_i8_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        8 ->
            d_field_values_i8_values(Rest,
                                     0,
                                     0,
                                     0,
                                     F@_1,
                                     F@_2,
                                     F@_3,
                                     F@_4,
                                     F@_5,
                                     F@_6,
                                     F@_7,
                                     F@_8,
                                     F@_9,
                                     F@_10,
                                     F@_11,
                                     F@_12,
                                     F@_13,
                                     F@_14,
                                     F@_15,
                                     F@_16,
                                     F@_17,
                                     F@_18,
                                     F@_19,
                                     F@_20,
                                     F@_21,
                                     F@_22,
                                     F@_23,
                                     F@_24,
                                     F@_25,
                                     F@_26,
                                     F@_27,
                                     F@_28,
                                     F@_29,
                                     F@_30,
                                     F@_31,
                                     TrUserData);
        18 ->
            d_pfield_values_i16_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        16 ->
            d_field_values_i16_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        26 ->
            d_pfield_values_i32_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        24 ->
            d_field_values_i32_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        34 ->
            d_pfield_values_i64_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        32 ->
            d_field_values_i64_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        42 ->
            d_pfield_values_u8_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        40 ->
            d_field_values_u8_values(Rest,
                                     0,
                                     0,
                                     0,
                                     F@_1,
                                     F@_2,
                                     F@_3,
                                     F@_4,
                                     F@_5,
                                     F@_6,
                                     F@_7,
                                     F@_8,
                                     F@_9,
                                     F@_10,
                                     F@_11,
                                     F@_12,
                                     F@_13,
                                     F@_14,
                                     F@_15,
                                     F@_16,
                                     F@_17,
                                     F@_18,
                                     F@_19,
                                     F@_20,
                                     F@_21,
                                     F@_22,
                                     F@_23,
                                     F@_24,
                                     F@_25,
                                     F@_26,
                                     F@_27,
                                     F@_28,
                                     F@_29,
                                     F@_30,
                                     F@_31,
                                     TrUserData);
        50 ->
            d_pfield_values_u16_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        48 ->
            d_field_values_u16_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        58 ->
            d_pfield_values_u32_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        56 ->
            d_field_values_u32_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        66 ->
            d_pfield_values_u64_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        64 ->
            d_field_values_u64_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        74 ->
            d_pfield_values_f32_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        77 ->
            d_field_values_f32_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        82 ->
            d_pfield_values_f64_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        81 ->
            d_field_values_f64_values(Rest,
                                      0,
                                      0,
                                      0,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
        90 ->
            d_pfield_values_bool_values(Rest,
                                        0,
                                        0,
                                        0,
                                        F@_1,
                                        F@_2,
                                        F@_3,
                                        F@_4,
                                        F@_5,
                                        F@_6,
                                        F@_7,
                                        F@_8,
                                        F@_9,
                                        F@_10,
                                        F@_11,
                                        F@_12,
                                        F@_13,
                                        F@_14,
                                        F@_15,
                                        F@_16,
                                        F@_17,
                                        F@_18,
                                        F@_19,
                                        F@_20,
                                        F@_21,
                                        F@_22,
                                        F@_23,
                                        F@_24,
                                        F@_25,
                                        F@_26,
                                        F@_27,
                                        F@_28,
                                        F@_29,
                                        F@_30,
                                        F@_31,
                                        TrUserData);
        88 ->
            d_field_values_bool_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        98 ->
            d_field_values_binary_values(Rest,
                                         0,
                                         0,
                                         0,
                                         F@_1,
                                         F@_2,
                                         F@_3,
                                         F@_4,
                                         F@_5,
                                         F@_6,
                                         F@_7,
                                         F@_8,
                                         F@_9,
                                         F@_10,
                                         F@_11,
                                         F@_12,
                                         F@_13,
                                         F@_14,
                                         F@_15,
                                         F@_16,
                                         F@_17,
                                         F@_18,
                                         F@_19,
                                         F@_20,
                                         F@_21,
                                         F@_22,
                                         F@_23,
                                         F@_24,
                                         F@_25,
                                         F@_26,
                                         F@_27,
                                         F@_28,
                                         F@_29,
                                         F@_30,
                                         F@_31,
                                         TrUserData);
        106 ->
            d_field_values_string_values(Rest,
                                         0,
                                         0,
                                         0,
                                         F@_1,
                                         F@_2,
                                         F@_3,
                                         F@_4,
                                         F@_5,
                                         F@_6,
                                         F@_7,
                                         F@_8,
                                         F@_9,
                                         F@_10,
                                         F@_11,
                                         F@_12,
                                         F@_13,
                                         F@_14,
                                         F@_15,
                                         F@_16,
                                         F@_17,
                                         F@_18,
                                         F@_19,
                                         F@_20,
                                         F@_21,
                                         F@_22,
                                         F@_23,
                                         F@_24,
                                         F@_25,
                                         F@_26,
                                         F@_27,
                                         F@_28,
                                         F@_29,
                                         F@_30,
                                         F@_31,
                                         TrUserData);
        114 ->
            d_pfield_values_date_values(Rest,
                                        0,
                                        0,
                                        0,
                                        F@_1,
                                        F@_2,
                                        F@_3,
                                        F@_4,
                                        F@_5,
                                        F@_6,
                                        F@_7,
                                        F@_8,
                                        F@_9,
                                        F@_10,
                                        F@_11,
                                        F@_12,
                                        F@_13,
                                        F@_14,
                                        F@_15,
                                        F@_16,
                                        F@_17,
                                        F@_18,
                                        F@_19,
                                        F@_20,
                                        F@_21,
                                        F@_22,
                                        F@_23,
                                        F@_24,
                                        F@_25,
                                        F@_26,
                                        F@_27,
                                        F@_28,
                                        F@_29,
                                        F@_30,
                                        F@_31,
                                        TrUserData);
        112 ->
            d_field_values_date_values(Rest,
                                       0,
                                       0,
                                       0,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
        122 ->
            d_pfield_values_datetime_values(Rest,
                                            0,
                                            0,
                                            0,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
        120 ->
            d_field_values_datetime_values(Rest,
                                           0,
                                           0,
                                           0,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
        130 ->
            d_pfield_values_timestamp_second_values(Rest,
                                                    0,
                                                    0,
                                                    0,
                                                    F@_1,
                                                    F@_2,
                                                    F@_3,
                                                    F@_4,
                                                    F@_5,
                                                    F@_6,
                                                    F@_7,
                                                    F@_8,
                                                    F@_9,
                                                    F@_10,
                                                    F@_11,
                                                    F@_12,
                                                    F@_13,
                                                    F@_14,
                                                    F@_15,
                                                    F@_16,
                                                    F@_17,
                                                    F@_18,
                                                    F@_19,
                                                    F@_20,
                                                    F@_21,
                                                    F@_22,
                                                    F@_23,
                                                    F@_24,
                                                    F@_25,
                                                    F@_26,
                                                    F@_27,
                                                    F@_28,
                                                    F@_29,
                                                    F@_30,
                                                    F@_31,
                                                    TrUserData);
        128 ->
            d_field_values_timestamp_second_values(Rest,
                                                   0,
                                                   0,
                                                   0,
                                                   F@_1,
                                                   F@_2,
                                                   F@_3,
                                                   F@_4,
                                                   F@_5,
                                                   F@_6,
                                                   F@_7,
                                                   F@_8,
                                                   F@_9,
                                                   F@_10,
                                                   F@_11,
                                                   F@_12,
                                                   F@_13,
                                                   F@_14,
                                                   F@_15,
                                                   F@_16,
                                                   F@_17,
                                                   F@_18,
                                                   F@_19,
                                                   F@_20,
                                                   F@_21,
                                                   F@_22,
                                                   F@_23,
                                                   F@_24,
                                                   F@_25,
                                                   F@_26,
                                                   F@_27,
                                                   F@_28,
                                                   F@_29,
                                                   F@_30,
                                                   F@_31,
                                                   TrUserData);
        138 ->
            d_pfield_values_timestamp_millisecond_values(Rest,
                                                         0,
                                                         0,
                                                         0,
                                                         F@_1,
                                                         F@_2,
                                                         F@_3,
                                                         F@_4,
                                                         F@_5,
                                                         F@_6,
                                                         F@_7,
                                                         F@_8,
                                                         F@_9,
                                                         F@_10,
                                                         F@_11,
                                                         F@_12,
                                                         F@_13,
                                                         F@_14,
                                                         F@_15,
                                                         F@_16,
                                                         F@_17,
                                                         F@_18,
                                                         F@_19,
                                                         F@_20,
                                                         F@_21,
                                                         F@_22,
                                                         F@_23,
                                                         F@_24,
                                                         F@_25,
                                                         F@_26,
                                                         F@_27,
                                                         F@_28,
                                                         F@_29,
                                                         F@_30,
                                                         F@_31,
                                                         TrUserData);
        136 ->
            d_field_values_timestamp_millisecond_values(Rest,
                                                        0,
                                                        0,
                                                        0,
                                                        F@_1,
                                                        F@_2,
                                                        F@_3,
                                                        F@_4,
                                                        F@_5,
                                                        F@_6,
                                                        F@_7,
                                                        F@_8,
                                                        F@_9,
                                                        F@_10,
                                                        F@_11,
                                                        F@_12,
                                                        F@_13,
                                                        F@_14,
                                                        F@_15,
                                                        F@_16,
                                                        F@_17,
                                                        F@_18,
                                                        F@_19,
                                                        F@_20,
                                                        F@_21,
                                                        F@_22,
                                                        F@_23,
                                                        F@_24,
                                                        F@_25,
                                                        F@_26,
                                                        F@_27,
                                                        F@_28,
                                                        F@_29,
                                                        F@_30,
                                                        F@_31,
                                                        TrUserData);
        146 ->
            d_pfield_values_timestamp_microsecond_values(Rest,
                                                         0,
                                                         0,
                                                         0,
                                                         F@_1,
                                                         F@_2,
                                                         F@_3,
                                                         F@_4,
                                                         F@_5,
                                                         F@_6,
                                                         F@_7,
                                                         F@_8,
                                                         F@_9,
                                                         F@_10,
                                                         F@_11,
                                                         F@_12,
                                                         F@_13,
                                                         F@_14,
                                                         F@_15,
                                                         F@_16,
                                                         F@_17,
                                                         F@_18,
                                                         F@_19,
                                                         F@_20,
                                                         F@_21,
                                                         F@_22,
                                                         F@_23,
                                                         F@_24,
                                                         F@_25,
                                                         F@_26,
                                                         F@_27,
                                                         F@_28,
                                                         F@_29,
                                                         F@_30,
                                                         F@_31,
                                                         TrUserData);
        144 ->
            d_field_values_timestamp_microsecond_values(Rest,
                                                        0,
                                                        0,
                                                        0,
                                                        F@_1,
                                                        F@_2,
                                                        F@_3,
                                                        F@_4,
                                                        F@_5,
                                                        F@_6,
                                                        F@_7,
                                                        F@_8,
                                                        F@_9,
                                                        F@_10,
                                                        F@_11,
                                                        F@_12,
                                                        F@_13,
                                                        F@_14,
                                                        F@_15,
                                                        F@_16,
                                                        F@_17,
                                                        F@_18,
                                                        F@_19,
                                                        F@_20,
                                                        F@_21,
                                                        F@_22,
                                                        F@_23,
                                                        F@_24,
                                                        F@_25,
                                                        F@_26,
                                                        F@_27,
                                                        F@_28,
                                                        F@_29,
                                                        F@_30,
                                                        F@_31,
                                                        TrUserData);
        154 ->
            d_pfield_values_timestamp_nanosecond_values(Rest,
                                                        0,
                                                        0,
                                                        0,
                                                        F@_1,
                                                        F@_2,
                                                        F@_3,
                                                        F@_4,
                                                        F@_5,
                                                        F@_6,
                                                        F@_7,
                                                        F@_8,
                                                        F@_9,
                                                        F@_10,
                                                        F@_11,
                                                        F@_12,
                                                        F@_13,
                                                        F@_14,
                                                        F@_15,
                                                        F@_16,
                                                        F@_17,
                                                        F@_18,
                                                        F@_19,
                                                        F@_20,
                                                        F@_21,
                                                        F@_22,
                                                        F@_23,
                                                        F@_24,
                                                        F@_25,
                                                        F@_26,
                                                        F@_27,
                                                        F@_28,
                                                        F@_29,
                                                        F@_30,
                                                        F@_31,
                                                        TrUserData);
        152 ->
            d_field_values_timestamp_nanosecond_values(Rest,
                                                       0,
                                                       0,
                                                       0,
                                                       F@_1,
                                                       F@_2,
                                                       F@_3,
                                                       F@_4,
                                                       F@_5,
                                                       F@_6,
                                                       F@_7,
                                                       F@_8,
                                                       F@_9,
                                                       F@_10,
                                                       F@_11,
                                                       F@_12,
                                                       F@_13,
                                                       F@_14,
                                                       F@_15,
                                                       F@_16,
                                                       F@_17,
                                                       F@_18,
                                                       F@_19,
                                                       F@_20,
                                                       F@_21,
                                                       F@_22,
                                                       F@_23,
                                                       F@_24,
                                                       F@_25,
                                                       F@_26,
                                                       F@_27,
                                                       F@_28,
                                                       F@_29,
                                                       F@_30,
                                                       F@_31,
                                                       TrUserData);
        162 ->
            d_pfield_values_time_second_values(Rest,
                                               0,
                                               0,
                                               0,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
        160 ->
            d_field_values_time_second_values(Rest,
                                              0,
                                              0,
                                              0,
                                              F@_1,
                                              F@_2,
                                              F@_3,
                                              F@_4,
                                              F@_5,
                                              F@_6,
                                              F@_7,
                                              F@_8,
                                              F@_9,
                                              F@_10,
                                              F@_11,
                                              F@_12,
                                              F@_13,
                                              F@_14,
                                              F@_15,
                                              F@_16,
                                              F@_17,
                                              F@_18,
                                              F@_19,
                                              F@_20,
                                              F@_21,
                                              F@_22,
                                              F@_23,
                                              F@_24,
                                              F@_25,
                                              F@_26,
                                              F@_27,
                                              F@_28,
                                              F@_29,
                                              F@_30,
                                              F@_31,
                                              TrUserData);
        170 ->
            d_pfield_values_time_millisecond_values(Rest,
                                                    0,
                                                    0,
                                                    0,
                                                    F@_1,
                                                    F@_2,
                                                    F@_3,
                                                    F@_4,
                                                    F@_5,
                                                    F@_6,
                                                    F@_7,
                                                    F@_8,
                                                    F@_9,
                                                    F@_10,
                                                    F@_11,
                                                    F@_12,
                                                    F@_13,
                                                    F@_14,
                                                    F@_15,
                                                    F@_16,
                                                    F@_17,
                                                    F@_18,
                                                    F@_19,
                                                    F@_20,
                                                    F@_21,
                                                    F@_22,
                                                    F@_23,
                                                    F@_24,
                                                    F@_25,
                                                    F@_26,
                                                    F@_27,
                                                    F@_28,
                                                    F@_29,
                                                    F@_30,
                                                    F@_31,
                                                    TrUserData);
        168 ->
            d_field_values_time_millisecond_values(Rest,
                                                   0,
                                                   0,
                                                   0,
                                                   F@_1,
                                                   F@_2,
                                                   F@_3,
                                                   F@_4,
                                                   F@_5,
                                                   F@_6,
                                                   F@_7,
                                                   F@_8,
                                                   F@_9,
                                                   F@_10,
                                                   F@_11,
                                                   F@_12,
                                                   F@_13,
                                                   F@_14,
                                                   F@_15,
                                                   F@_16,
                                                   F@_17,
                                                   F@_18,
                                                   F@_19,
                                                   F@_20,
                                                   F@_21,
                                                   F@_22,
                                                   F@_23,
                                                   F@_24,
                                                   F@_25,
                                                   F@_26,
                                                   F@_27,
                                                   F@_28,
                                                   F@_29,
                                                   F@_30,
                                                   F@_31,
                                                   TrUserData);
        178 ->
            d_pfield_values_time_microsecond_values(Rest,
                                                    0,
                                                    0,
                                                    0,
                                                    F@_1,
                                                    F@_2,
                                                    F@_3,
                                                    F@_4,
                                                    F@_5,
                                                    F@_6,
                                                    F@_7,
                                                    F@_8,
                                                    F@_9,
                                                    F@_10,
                                                    F@_11,
                                                    F@_12,
                                                    F@_13,
                                                    F@_14,
                                                    F@_15,
                                                    F@_16,
                                                    F@_17,
                                                    F@_18,
                                                    F@_19,
                                                    F@_20,
                                                    F@_21,
                                                    F@_22,
                                                    F@_23,
                                                    F@_24,
                                                    F@_25,
                                                    F@_26,
                                                    F@_27,
                                                    F@_28,
                                                    F@_29,
                                                    F@_30,
                                                    F@_31,
                                                    TrUserData);
        176 ->
            d_field_values_time_microsecond_values(Rest,
                                                   0,
                                                   0,
                                                   0,
                                                   F@_1,
                                                   F@_2,
                                                   F@_3,
                                                   F@_4,
                                                   F@_5,
                                                   F@_6,
                                                   F@_7,
                                                   F@_8,
                                                   F@_9,
                                                   F@_10,
                                                   F@_11,
                                                   F@_12,
                                                   F@_13,
                                                   F@_14,
                                                   F@_15,
                                                   F@_16,
                                                   F@_17,
                                                   F@_18,
                                                   F@_19,
                                                   F@_20,
                                                   F@_21,
                                                   F@_22,
                                                   F@_23,
                                                   F@_24,
                                                   F@_25,
                                                   F@_26,
                                                   F@_27,
                                                   F@_28,
                                                   F@_29,
                                                   F@_30,
                                                   F@_31,
                                                   TrUserData);
        186 ->
            d_pfield_values_time_nanosecond_values(Rest,
                                                   0,
                                                   0,
                                                   0,
                                                   F@_1,
                                                   F@_2,
                                                   F@_3,
                                                   F@_4,
                                                   F@_5,
                                                   F@_6,
                                                   F@_7,
                                                   F@_8,
                                                   F@_9,
                                                   F@_10,
                                                   F@_11,
                                                   F@_12,
                                                   F@_13,
                                                   F@_14,
                                                   F@_15,
                                                   F@_16,
                                                   F@_17,
                                                   F@_18,
                                                   F@_19,
                                                   F@_20,
                                                   F@_21,
                                                   F@_22,
                                                   F@_23,
                                                   F@_24,
                                                   F@_25,
                                                   F@_26,
                                                   F@_27,
                                                   F@_28,
                                                   F@_29,
                                                   F@_30,
                                                   F@_31,
                                                   TrUserData);
        184 ->
            d_field_values_time_nanosecond_values(Rest,
                                                  0,
                                                  0,
                                                  0,
                                                  F@_1,
                                                  F@_2,
                                                  F@_3,
                                                  F@_4,
                                                  F@_5,
                                                  F@_6,
                                                  F@_7,
                                                  F@_8,
                                                  F@_9,
                                                  F@_10,
                                                  F@_11,
                                                  F@_12,
                                                  F@_13,
                                                  F@_14,
                                                  F@_15,
                                                  F@_16,
                                                  F@_17,
                                                  F@_18,
                                                  F@_19,
                                                  F@_20,
                                                  F@_21,
                                                  F@_22,
                                                  F@_23,
                                                  F@_24,
                                                  F@_25,
                                                  F@_26,
                                                  F@_27,
                                                  F@_28,
                                                  F@_29,
                                                  F@_30,
                                                  F@_31,
                                                  TrUserData);
        194 ->
            d_pfield_values_interval_year_month_values(Rest,
                                                       0,
                                                       0,
                                                       0,
                                                       F@_1,
                                                       F@_2,
                                                       F@_3,
                                                       F@_4,
                                                       F@_5,
                                                       F@_6,
                                                       F@_7,
                                                       F@_8,
                                                       F@_9,
                                                       F@_10,
                                                       F@_11,
                                                       F@_12,
                                                       F@_13,
                                                       F@_14,
                                                       F@_15,
                                                       F@_16,
                                                       F@_17,
                                                       F@_18,
                                                       F@_19,
                                                       F@_20,
                                                       F@_21,
                                                       F@_22,
                                                       F@_23,
                                                       F@_24,
                                                       F@_25,
                                                       F@_26,
                                                       F@_27,
                                                       F@_28,
                                                       F@_29,
                                                       F@_30,
                                                       F@_31,
                                                       TrUserData);
        192 ->
            d_field_values_interval_year_month_values(Rest,
                                                      0,
                                                      0,
                                                      0,
                                                      F@_1,
                                                      F@_2,
                                                      F@_3,
                                                      F@_4,
                                                      F@_5,
                                                      F@_6,
                                                      F@_7,
                                                      F@_8,
                                                      F@_9,
                                                      F@_10,
                                                      F@_11,
                                                      F@_12,
                                                      F@_13,
                                                      F@_14,
                                                      F@_15,
                                                      F@_16,
                                                      F@_17,
                                                      F@_18,
                                                      F@_19,
                                                      F@_20,
                                                      F@_21,
                                                      F@_22,
                                                      F@_23,
                                                      F@_24,
                                                      F@_25,
                                                      F@_26,
                                                      F@_27,
                                                      F@_28,
                                                      F@_29,
                                                      F@_30,
                                                      F@_31,
                                                      TrUserData);
        202 ->
            d_pfield_values_interval_day_time_values(Rest,
                                                     0,
                                                     0,
                                                     0,
                                                     F@_1,
                                                     F@_2,
                                                     F@_3,
                                                     F@_4,
                                                     F@_5,
                                                     F@_6,
                                                     F@_7,
                                                     F@_8,
                                                     F@_9,
                                                     F@_10,
                                                     F@_11,
                                                     F@_12,
                                                     F@_13,
                                                     F@_14,
                                                     F@_15,
                                                     F@_16,
                                                     F@_17,
                                                     F@_18,
                                                     F@_19,
                                                     F@_20,
                                                     F@_21,
                                                     F@_22,
                                                     F@_23,
                                                     F@_24,
                                                     F@_25,
                                                     F@_26,
                                                     F@_27,
                                                     F@_28,
                                                     F@_29,
                                                     F@_30,
                                                     F@_31,
                                                     TrUserData);
        200 ->
            d_field_values_interval_day_time_values(Rest,
                                                    0,
                                                    0,
                                                    0,
                                                    F@_1,
                                                    F@_2,
                                                    F@_3,
                                                    F@_4,
                                                    F@_5,
                                                    F@_6,
                                                    F@_7,
                                                    F@_8,
                                                    F@_9,
                                                    F@_10,
                                                    F@_11,
                                                    F@_12,
                                                    F@_13,
                                                    F@_14,
                                                    F@_15,
                                                    F@_16,
                                                    F@_17,
                                                    F@_18,
                                                    F@_19,
                                                    F@_20,
                                                    F@_21,
                                                    F@_22,
                                                    F@_23,
                                                    F@_24,
                                                    F@_25,
                                                    F@_26,
                                                    F@_27,
                                                    F@_28,
                                                    F@_29,
                                                    F@_30,
                                                    F@_31,
                                                    TrUserData);
        210 ->
            d_field_values_interval_month_day_nano_values(Rest,
                                                          0,
                                                          0,
                                                          0,
                                                          F@_1,
                                                          F@_2,
                                                          F@_3,
                                                          F@_4,
                                                          F@_5,
                                                          F@_6,
                                                          F@_7,
                                                          F@_8,
                                                          F@_9,
                                                          F@_10,
                                                          F@_11,
                                                          F@_12,
                                                          F@_13,
                                                          F@_14,
                                                          F@_15,
                                                          F@_16,
                                                          F@_17,
                                                          F@_18,
                                                          F@_19,
                                                          F@_20,
                                                          F@_21,
                                                          F@_22,
                                                          F@_23,
                                                          F@_24,
                                                          F@_25,
                                                          F@_26,
                                                          F@_27,
                                                          F@_28,
                                                          F@_29,
                                                          F@_30,
                                                          F@_31,
                                                          TrUserData);
        218 ->
            d_pfield_values_duration_second_values(Rest,
                                                   0,
                                                   0,
                                                   0,
                                                   F@_1,
                                                   F@_2,
                                                   F@_3,
                                                   F@_4,
                                                   F@_5,
                                                   F@_6,
                                                   F@_7,
                                                   F@_8,
                                                   F@_9,
                                                   F@_10,
                                                   F@_11,
                                                   F@_12,
                                                   F@_13,
                                                   F@_14,
                                                   F@_15,
                                                   F@_16,
                                                   F@_17,
                                                   F@_18,
                                                   F@_19,
                                                   F@_20,
                                                   F@_21,
                                                   F@_22,
                                                   F@_23,
                                                   F@_24,
                                                   F@_25,
                                                   F@_26,
                                                   F@_27,
                                                   F@_28,
                                                   F@_29,
                                                   F@_30,
                                                   F@_31,
                                                   TrUserData);
        216 ->
            d_field_values_duration_second_values(Rest,
                                                  0,
                                                  0,
                                                  0,
                                                  F@_1,
                                                  F@_2,
                                                  F@_3,
                                                  F@_4,
                                                  F@_5,
                                                  F@_6,
                                                  F@_7,
                                                  F@_8,
                                                  F@_9,
                                                  F@_10,
                                                  F@_11,
                                                  F@_12,
                                                  F@_13,
                                                  F@_14,
                                                  F@_15,
                                                  F@_16,
                                                  F@_17,
                                                  F@_18,
                                                  F@_19,
                                                  F@_20,
                                                  F@_21,
                                                  F@_22,
                                                  F@_23,
                                                  F@_24,
                                                  F@_25,
                                                  F@_26,
                                                  F@_27,
                                                  F@_28,
                                                  F@_29,
                                                  F@_30,
                                                  F@_31,
                                                  TrUserData);
        226 ->
            d_pfield_values_duration_millisecond_values(Rest,
                                                        0,
                                                        0,
                                                        0,
                                                        F@_1,
                                                        F@_2,
                                                        F@_3,
                                                        F@_4,
                                                        F@_5,
                                                        F@_6,
                                                        F@_7,
                                                        F@_8,
                                                        F@_9,
                                                        F@_10,
                                                        F@_11,
                                                        F@_12,
                                                        F@_13,
                                                        F@_14,
                                                        F@_15,
                                                        F@_16,
                                                        F@_17,
                                                        F@_18,
                                                        F@_19,
                                                        F@_20,
                                                        F@_21,
                                                        F@_22,
                                                        F@_23,
                                                        F@_24,
                                                        F@_25,
                                                        F@_26,
                                                        F@_27,
                                                        F@_28,
                                                        F@_29,
                                                        F@_30,
                                                        F@_31,
                                                        TrUserData);
        224 ->
            d_field_values_duration_millisecond_values(Rest,
                                                       0,
                                                       0,
                                                       0,
                                                       F@_1,
                                                       F@_2,
                                                       F@_3,
                                                       F@_4,
                                                       F@_5,
                                                       F@_6,
                                                       F@_7,
                                                       F@_8,
                                                       F@_9,
                                                       F@_10,
                                                       F@_11,
                                                       F@_12,
                                                       F@_13,
                                                       F@_14,
                                                       F@_15,
                                                       F@_16,
                                                       F@_17,
                                                       F@_18,
                                                       F@_19,
                                                       F@_20,
                                                       F@_21,
                                                       F@_22,
                                                       F@_23,
                                                       F@_24,
                                                       F@_25,
                                                       F@_26,
                                                       F@_27,
                                                       F@_28,
                                                       F@_29,
                                                       F@_30,
                                                       F@_31,
                                                       TrUserData);
        234 ->
            d_pfield_values_duration_microsecond_values(Rest,
                                                        0,
                                                        0,
                                                        0,
                                                        F@_1,
                                                        F@_2,
                                                        F@_3,
                                                        F@_4,
                                                        F@_5,
                                                        F@_6,
                                                        F@_7,
                                                        F@_8,
                                                        F@_9,
                                                        F@_10,
                                                        F@_11,
                                                        F@_12,
                                                        F@_13,
                                                        F@_14,
                                                        F@_15,
                                                        F@_16,
                                                        F@_17,
                                                        F@_18,
                                                        F@_19,
                                                        F@_20,
                                                        F@_21,
                                                        F@_22,
                                                        F@_23,
                                                        F@_24,
                                                        F@_25,
                                                        F@_26,
                                                        F@_27,
                                                        F@_28,
                                                        F@_29,
                                                        F@_30,
                                                        F@_31,
                                                        TrUserData);
        232 ->
            d_field_values_duration_microsecond_values(Rest,
                                                       0,
                                                       0,
                                                       0,
                                                       F@_1,
                                                       F@_2,
                                                       F@_3,
                                                       F@_4,
                                                       F@_5,
                                                       F@_6,
                                                       F@_7,
                                                       F@_8,
                                                       F@_9,
                                                       F@_10,
                                                       F@_11,
                                                       F@_12,
                                                       F@_13,
                                                       F@_14,
                                                       F@_15,
                                                       F@_16,
                                                       F@_17,
                                                       F@_18,
                                                       F@_19,
                                                       F@_20,
                                                       F@_21,
                                                       F@_22,
                                                       F@_23,
                                                       F@_24,
                                                       F@_25,
                                                       F@_26,
                                                       F@_27,
                                                       F@_28,
                                                       F@_29,
                                                       F@_30,
                                                       F@_31,
                                                       TrUserData);
        242 ->
            d_pfield_values_duration_nanosecond_values(Rest,
                                                       0,
                                                       0,
                                                       0,
                                                       F@_1,
                                                       F@_2,
                                                       F@_3,
                                                       F@_4,
                                                       F@_5,
                                                       F@_6,
                                                       F@_7,
                                                       F@_8,
                                                       F@_9,
                                                       F@_10,
                                                       F@_11,
                                                       F@_12,
                                                       F@_13,
                                                       F@_14,
                                                       F@_15,
                                                       F@_16,
                                                       F@_17,
                                                       F@_18,
                                                       F@_19,
                                                       F@_20,
                                                       F@_21,
                                                       F@_22,
                                                       F@_23,
                                                       F@_24,
                                                       F@_25,
                                                       F@_26,
                                                       F@_27,
                                                       F@_28,
                                                       F@_29,
                                                       F@_30,
                                                       F@_31,
                                                       TrUserData);
        240 ->
            d_field_values_duration_nanosecond_values(Rest,
                                                      0,
                                                      0,
                                                      0,
                                                      F@_1,
                                                      F@_2,
                                                      F@_3,
                                                      F@_4,
                                                      F@_5,
                                                      F@_6,
                                                      F@_7,
                                                      F@_8,
                                                      F@_9,
                                                      F@_10,
                                                      F@_11,
                                                      F@_12,
                                                      F@_13,
                                                      F@_14,
                                                      F@_15,
                                                      F@_16,
                                                      F@_17,
                                                      F@_18,
                                                      F@_19,
                                                      F@_20,
                                                      F@_21,
                                                      F@_22,
                                                      F@_23,
                                                      F@_24,
                                                      F@_25,
                                                      F@_26,
                                                      F@_27,
                                                      F@_28,
                                                      F@_29,
                                                      F@_30,
                                                      F@_31,
                                                      TrUserData);
        250 ->
            d_field_values_decimal128_values(Rest,
                                             0,
                                             0,
                                             0,
                                             F@_1,
                                             F@_2,
                                             F@_3,
                                             F@_4,
                                             F@_5,
                                             F@_6,
                                             F@_7,
                                             F@_8,
                                             F@_9,
                                             F@_10,
                                             F@_11,
                                             F@_12,
                                             F@_13,
                                             F@_14,
                                             F@_15,
                                             F@_16,
                                             F@_17,
                                             F@_18,
                                             F@_19,
                                             F@_20,
                                             F@_21,
                                             F@_22,
                                             F@_23,
                                             F@_24,
                                             F@_25,
                                             F@_26,
                                             F@_27,
                                             F@_28,
                                             F@_29,
                                             F@_30,
                                             F@_31,
                                             TrUserData);
        _ ->
            case Key band 7 of
                0 ->
                    skip_varint_values(Rest,
                                       0,
                                       0,
                                       Key bsr 3,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
                1 ->
                    skip_64_values(Rest,
                                   0,
                                   0,
                                   Key bsr 3,
                                   F@_1,
                                   F@_2,
                                   F@_3,
                                   F@_4,
                                   F@_5,
                                   F@_6,
                                   F@_7,
                                   F@_8,
                                   F@_9,
                                   F@_10,
                                   F@_11,
                                   F@_12,
                                   F@_13,
                                   F@_14,
                                   F@_15,
                                   F@_16,
                                   F@_17,
                                   F@_18,
                                   F@_19,
                                   F@_20,
                                   F@_21,
                                   F@_22,
                                   F@_23,
                                   F@_24,
                                   F@_25,
                                   F@_26,
                                   F@_27,
                                   F@_28,
                                   F@_29,
                                   F@_30,
                                   F@_31,
                                   TrUserData);
                2 ->
                    skip_length_delimited_values(Rest,
                                                 0,
                                                 0,
                                                 Key bsr 3,
                                                 F@_1,
                                                 F@_2,
                                                 F@_3,
                                                 F@_4,
                                                 F@_5,
                                                 F@_6,
                                                 F@_7,
                                                 F@_8,
                                                 F@_9,
                                                 F@_10,
                                                 F@_11,
                                                 F@_12,
                                                 F@_13,
                                                 F@_14,
                                                 F@_15,
                                                 F@_16,
                                                 F@_17,
                                                 F@_18,
                                                 F@_19,
                                                 F@_20,
                                                 F@_21,
                                                 F@_22,
                                                 F@_23,
                                                 F@_24,
                                                 F@_25,
                                                 F@_26,
                                                 F@_27,
                                                 F@_28,
                                                 F@_29,
                                                 F@_30,
                                                 F@_31,
                                                 TrUserData);
                3 ->
                    skip_group_values(Rest,
                                      0,
                                      0,
                                      Key bsr 3,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
                5 ->
                    skip_32_values(Rest,
                                   0,
                                   0,
                                   Key bsr 3,
                                   F@_1,
                                   F@_2,
                                   F@_3,
                                   F@_4,
                                   F@_5,
                                   F@_6,
                                   F@_7,
                                   F@_8,
                                   F@_9,
                                   F@_10,
                                   F@_11,
                                   F@_12,
                                   F@_13,
                                   F@_14,
                                   F@_15,
                                   F@_16,
                                   F@_17,
                                   F@_18,
                                   F@_19,
                                   F@_20,
                                   F@_21,
                                   F@_22,
                                   F@_23,
                                   F@_24,
                                   F@_25,
                                   F@_26,
                                   F@_27,
                                   F@_28,
                                   F@_29,
                                   F@_30,
                                   F@_31,
                                   TrUserData)
            end
    end;
dg_read_field_def_values(<<>>, 0, 0, _, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30, R31, TrUserData) ->
    S1 = #{i8_values => lists_reverse(R1, TrUserData), i16_values => lists_reverse(R2, TrUserData), i32_values => lists_reverse(R3, TrUserData), i64_values => lists_reverse(R4, TrUserData), u8_values => lists_reverse(R5, TrUserData),
           u16_values => lists_reverse(R6, TrUserData), u32_values => lists_reverse(R7, TrUserData), u64_values => lists_reverse(R8, TrUserData), f32_values => lists_reverse(R9, TrUserData), f64_values => lists_reverse(R10, TrUserData),
           bool_values => lists_reverse(R11, TrUserData), binary_values => lists_reverse(R12, TrUserData), string_values => lists_reverse(R13, TrUserData), date_values => lists_reverse(R14, TrUserData), datetime_values => lists_reverse(R15, TrUserData),
           timestamp_second_values => lists_reverse(R16, TrUserData), timestamp_millisecond_values => lists_reverse(R17, TrUserData), timestamp_microsecond_values => lists_reverse(R18, TrUserData), timestamp_nanosecond_values => lists_reverse(R19, TrUserData),
           time_second_values => lists_reverse(R20, TrUserData), time_millisecond_values => lists_reverse(R21, TrUserData), time_microsecond_values => lists_reverse(R22, TrUserData), time_nanosecond_values => lists_reverse(R23, TrUserData),
           interval_year_month_values => lists_reverse(R24, TrUserData), interval_day_time_values => lists_reverse(R25, TrUserData), duration_second_values => lists_reverse(R27, TrUserData), duration_millisecond_values => lists_reverse(R28, TrUserData),
           duration_microsecond_values => lists_reverse(R29, TrUserData), duration_nanosecond_values => lists_reverse(R30, TrUserData)},
    S2 = if R26 == '$undef' -> S1;
            true -> S1#{interval_month_day_nano_values => lists_reverse(R26, TrUserData)}
         end,
    if R31 == '$undef' -> S2;
       true -> S2#{decimal128_values => lists_reverse(R31, TrUserData)}
    end.

d_field_values_i8_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                         F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_i8_values(Rest,
                             N + 7,
                             X bsl N + Acc,
                             F,
                             F@_1,
                             F@_2,
                             F@_3,
                             F@_4,
                             F@_5,
                             F@_6,
                             F@_7,
                             F@_8,
                             F@_9,
                             F@_10,
                             F@_11,
                             F@_12,
                             F@_13,
                             F@_14,
                             F@_15,
                             F@_16,
                             F@_17,
                             F@_18,
                             F@_19,
                             F@_20,
                             F@_21,
                             F@_22,
                             F@_23,
                             F@_24,
                             F@_25,
                             F@_26,
                             F@_27,
                             F@_28,
                             F@_29,
                             F@_30,
                             F@_31,
                             TrUserData);
d_field_values_i8_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, Prev, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                         F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              cons(NewFValue, Prev, TrUserData),
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_i8_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_i8_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_pfield_values_i8_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, E, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_i8_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              NewSeq,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_i8_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_i8_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_i8_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_i8_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_i8_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_i16_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_i16_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_i16_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, Prev, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              cons(NewFValue, Prev, TrUserData),
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_i16_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_i16_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_i16_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, E, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_i16_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              NewSeq,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_i16_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_i16_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_i16_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_i16_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_i16_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_i32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_i32_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_i32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, Prev, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              cons(NewFValue, Prev, TrUserData),
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_i32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_i32_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_i32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, E, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_i32_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              NewSeq,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_i32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_i32_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_i32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_i32_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_i32_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_i64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_i64_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_i64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, Prev, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              cons(NewFValue, Prev, TrUserData),
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_i64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_i64_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_i64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, E, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_i64_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              NewSeq,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_i64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_i64_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_i64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_i64_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_i64_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_u8_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                         F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_u8_values(Rest,
                             N + 7,
                             X bsl N + Acc,
                             F,
                             F@_1,
                             F@_2,
                             F@_3,
                             F@_4,
                             F@_5,
                             F@_6,
                             F@_7,
                             F@_8,
                             F@_9,
                             F@_10,
                             F@_11,
                             F@_12,
                             F@_13,
                             F@_14,
                             F@_15,
                             F@_16,
                             F@_17,
                             F@_18,
                             F@_19,
                             F@_20,
                             F@_21,
                             F@_22,
                             F@_23,
                             F@_24,
                             F@_25,
                             F@_26,
                             F@_27,
                             F@_28,
                             F@_29,
                             F@_30,
                             F@_31,
                             TrUserData);
d_field_values_u8_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, Prev, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                         F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              cons(NewFValue, Prev, TrUserData),
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_u8_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_u8_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_pfield_values_u8_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, E, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_u8_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              NewSeq,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_u8_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_u8_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_u8_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    d_packed_field_values_u8_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_u8_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_u16_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_u16_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_u16_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, Prev, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              cons(NewFValue, Prev, TrUserData),
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_u16_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_u16_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_u16_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, E, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_u16_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              NewSeq,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_u16_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_u16_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_u16_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    d_packed_field_values_u16_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_u16_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_u32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_u32_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_u32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, Prev, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              cons(NewFValue, Prev, TrUserData),
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_u32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_u32_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_u32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, E, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_u32_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              NewSeq,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_u32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_u32_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_u32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    d_packed_field_values_u32_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_u32_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_u64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_u64_values(Rest,
                              N + 7,
                              X bsl N + Acc,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_u64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, Prev, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                          F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 18446744073709551615, TrUserData), Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              cons(NewFValue, Prev, TrUserData),
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_u64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_u64_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_u64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, E, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_u64_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              NewSeq,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_u64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_u64_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_u64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 18446744073709551615, TrUserData), Rest},
    d_packed_field_values_u64_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_u64_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_f32_values(<<0:16, 128, 127, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, Prev, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                          F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              cons(id(infinity, TrUserData), Prev, TrUserData),
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_f32_values(<<0:16, 128, 255, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, Prev, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                          F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              cons(id('-infinity', TrUserData), Prev, TrUserData),
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_f32_values(<<_:16, 1:1, _:7, _:1, 127:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, Prev, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                          F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              cons(id(nan, TrUserData), Prev, TrUserData),
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_f32_values(<<Value:32/little-float, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, Prev, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                          F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              cons(id(Value, TrUserData), Prev, TrUserData),
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_f32_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_f32_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_f32_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, E, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_f32_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              NewSeq,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_f32_values(<<0:16, 128, 127, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f32_values(Rest, Z1, Z2, F, cons(id(infinity, TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f32_values(<<0:16, 128, 255, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f32_values(Rest, Z1, Z2, F, cons(id('-infinity', TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f32_values(<<_:16, 1:1, _:7, _:1, 127:7, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f32_values(Rest, Z1, Z2, F, cons(id(nan, TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f32_values(<<Value:32/little-float, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f32_values(Rest, Z1, Z2, F, cons(id(Value, TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f32_values(<<>>, _, _, _, AccSeq, _) -> AccSeq.

d_field_values_f64_values(<<0:48, 240, 127, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, Prev, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                          F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              cons(id(infinity, TrUserData), Prev, TrUserData),
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_f64_values(<<0:48, 240, 255, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, Prev, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                          F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              cons(id('-infinity', TrUserData), Prev, TrUserData),
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_f64_values(<<_:48, 15:4, _:4, _:1, 127:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, Prev, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                          F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              cons(id(nan, TrUserData), Prev, TrUserData),
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData);
d_field_values_f64_values(<<Value:64/little-float, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, Prev, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                          F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              cons(id(Value, TrUserData), Prev, TrUserData),
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_f64_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_f64_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_pfield_values_f64_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, E, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_f64_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              NewSeq,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_f64_values(<<0:48, 240, 127, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f64_values(Rest, Z1, Z2, F, cons(id(infinity, TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f64_values(<<0:48, 240, 255, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f64_values(Rest, Z1, Z2, F, cons(id('-infinity', TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f64_values(<<_:48, 15:4, _:4, _:1, 127:7, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f64_values(Rest, Z1, Z2, F, cons(id(nan, TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f64_values(<<Value:64/little-float, Rest/binary>>, Z1, Z2, F, AccSeq, TrUserData) -> d_packed_field_values_f64_values(Rest, Z1, Z2, F, cons(id(Value, TrUserData), AccSeq, TrUserData), TrUserData);
d_packed_field_values_f64_values(<<>>, _, _, _, AccSeq, _) -> AccSeq.

d_field_values_bool_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_bool_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_field_values_bool_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, Prev, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {id(X bsl N + Acc =/= 0, TrUserData), Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              cons(NewFValue, Prev, TrUserData),
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_bool_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                            F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_bool_values(Rest,
                                N + 7,
                                X bsl N + Acc,
                                F,
                                F@_1,
                                F@_2,
                                F@_3,
                                F@_4,
                                F@_5,
                                F@_6,
                                F@_7,
                                F@_8,
                                F@_9,
                                F@_10,
                                F@_11,
                                F@_12,
                                F@_13,
                                F@_14,
                                F@_15,
                                F@_16,
                                F@_17,
                                F@_18,
                                F@_19,
                                F@_20,
                                F@_21,
                                F@_22,
                                F@_23,
                                F@_24,
                                F@_25,
                                F@_26,
                                F@_27,
                                F@_28,
                                F@_29,
                                F@_30,
                                F@_31,
                                TrUserData);
d_pfield_values_bool_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, E, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                            F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_bool_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              NewSeq,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_bool_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_bool_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_bool_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {id(X bsl N + Acc =/= 0, TrUserData), Rest},
    d_packed_field_values_bool_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_bool_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_binary_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                             F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_binary_values(Rest,
                                 N + 7,
                                 X bsl N + Acc,
                                 F,
                                 F@_1,
                                 F@_2,
                                 F@_3,
                                 F@_4,
                                 F@_5,
                                 F@_6,
                                 F@_7,
                                 F@_8,
                                 F@_9,
                                 F@_10,
                                 F@_11,
                                 F@_12,
                                 F@_13,
                                 F@_14,
                                 F@_15,
                                 F@_16,
                                 F@_17,
                                 F@_18,
                                 F@_19,
                                 F@_20,
                                 F@_21,
                                 F@_22,
                                 F@_23,
                                 F@_24,
                                 F@_25,
                                 F@_26,
                                 F@_27,
                                 F@_28,
                                 F@_29,
                                 F@_30,
                                 F@_31,
                                 TrUserData);
d_field_values_binary_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, Prev, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                             F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              cons(NewFValue, Prev, TrUserData),
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_field_values_string_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                             F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_string_values(Rest,
                                 N + 7,
                                 X bsl N + Acc,
                                 F,
                                 F@_1,
                                 F@_2,
                                 F@_3,
                                 F@_4,
                                 F@_5,
                                 F@_6,
                                 F@_7,
                                 F@_8,
                                 F@_9,
                                 F@_10,
                                 F@_11,
                                 F@_12,
                                 F@_13,
                                 F@_14,
                                 F@_15,
                                 F@_16,
                                 F@_17,
                                 F@_18,
                                 F@_19,
                                 F@_20,
                                 F@_21,
                                 F@_22,
                                 F@_23,
                                 F@_24,
                                 F@_25,
                                 F@_26,
                                 F@_27,
                                 F@_28,
                                 F@_29,
                                 F@_30,
                                 F@_31,
                                 TrUserData);
d_field_values_string_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, Prev, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                             F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              cons(NewFValue, Prev, TrUserData),
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_field_values_date_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                           F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_date_values(Rest,
                               N + 7,
                               X bsl N + Acc,
                               F,
                               F@_1,
                               F@_2,
                               F@_3,
                               F@_4,
                               F@_5,
                               F@_6,
                               F@_7,
                               F@_8,
                               F@_9,
                               F@_10,
                               F@_11,
                               F@_12,
                               F@_13,
                               F@_14,
                               F@_15,
                               F@_16,
                               F@_17,
                               F@_18,
                               F@_19,
                               F@_20,
                               F@_21,
                               F@_22,
                               F@_23,
                               F@_24,
                               F@_25,
                               F@_26,
                               F@_27,
                               F@_28,
                               F@_29,
                               F@_30,
                               F@_31,
                               TrUserData);
d_field_values_date_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, Prev, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                           F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              cons(NewFValue, Prev, TrUserData),
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_date_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                            F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_date_values(Rest,
                                N + 7,
                                X bsl N + Acc,
                                F,
                                F@_1,
                                F@_2,
                                F@_3,
                                F@_4,
                                F@_5,
                                F@_6,
                                F@_7,
                                F@_8,
                                F@_9,
                                F@_10,
                                F@_11,
                                F@_12,
                                F@_13,
                                F@_14,
                                F@_15,
                                F@_16,
                                F@_17,
                                F@_18,
                                F@_19,
                                F@_20,
                                F@_21,
                                F@_22,
                                F@_23,
                                F@_24,
                                F@_25,
                                F@_26,
                                F@_27,
                                F@_28,
                                F@_29,
                                F@_30,
                                F@_31,
                                TrUserData);
d_pfield_values_date_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, E, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28,
                            F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_date_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              NewSeq,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_date_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_date_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_date_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_date_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_date_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_datetime_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                               F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_datetime_values(Rest,
                                   N + 7,
                                   X bsl N + Acc,
                                   F,
                                   F@_1,
                                   F@_2,
                                   F@_3,
                                   F@_4,
                                   F@_5,
                                   F@_6,
                                   F@_7,
                                   F@_8,
                                   F@_9,
                                   F@_10,
                                   F@_11,
                                   F@_12,
                                   F@_13,
                                   F@_14,
                                   F@_15,
                                   F@_16,
                                   F@_17,
                                   F@_18,
                                   F@_19,
                                   F@_20,
                                   F@_21,
                                   F@_22,
                                   F@_23,
                                   F@_24,
                                   F@_25,
                                   F@_26,
                                   F@_27,
                                   F@_28,
                                   F@_29,
                                   F@_30,
                                   F@_31,
                                   TrUserData);
d_field_values_datetime_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, Prev, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                               F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              cons(NewFValue, Prev, TrUserData),
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_datetime_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                                F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_datetime_values(Rest,
                                    N + 7,
                                    X bsl N + Acc,
                                    F,
                                    F@_1,
                                    F@_2,
                                    F@_3,
                                    F@_4,
                                    F@_5,
                                    F@_6,
                                    F@_7,
                                    F@_8,
                                    F@_9,
                                    F@_10,
                                    F@_11,
                                    F@_12,
                                    F@_13,
                                    F@_14,
                                    F@_15,
                                    F@_16,
                                    F@_17,
                                    F@_18,
                                    F@_19,
                                    F@_20,
                                    F@_21,
                                    F@_22,
                                    F@_23,
                                    F@_24,
                                    F@_25,
                                    F@_26,
                                    F@_27,
                                    F@_28,
                                    F@_29,
                                    F@_30,
                                    F@_31,
                                    TrUserData);
d_pfield_values_datetime_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, E, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                                F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_datetime_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              NewSeq,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_datetime_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_datetime_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_datetime_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_datetime_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_datetime_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_timestamp_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_timestamp_second_values(Rest,
                                           N + 7,
                                           X bsl N + Acc,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
d_field_values_timestamp_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, Prev, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              cons(NewFValue, Prev, TrUserData),
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_timestamp_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_timestamp_second_values(Rest,
                                            N + 7,
                                            X bsl N + Acc,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
d_pfield_values_timestamp_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, E, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_timestamp_second_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              NewSeq,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_timestamp_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_timestamp_second_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_timestamp_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_timestamp_second_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_timestamp_second_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_timestamp_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_timestamp_millisecond_values(Rest,
                                                N + 7,
                                                X bsl N + Acc,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
d_field_values_timestamp_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, Prev, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              cons(NewFValue, Prev, TrUserData),
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_timestamp_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                             F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_timestamp_millisecond_values(Rest,
                                                 N + 7,
                                                 X bsl N + Acc,
                                                 F,
                                                 F@_1,
                                                 F@_2,
                                                 F@_3,
                                                 F@_4,
                                                 F@_5,
                                                 F@_6,
                                                 F@_7,
                                                 F@_8,
                                                 F@_9,
                                                 F@_10,
                                                 F@_11,
                                                 F@_12,
                                                 F@_13,
                                                 F@_14,
                                                 F@_15,
                                                 F@_16,
                                                 F@_17,
                                                 F@_18,
                                                 F@_19,
                                                 F@_20,
                                                 F@_21,
                                                 F@_22,
                                                 F@_23,
                                                 F@_24,
                                                 F@_25,
                                                 F@_26,
                                                 F@_27,
                                                 F@_28,
                                                 F@_29,
                                                 F@_30,
                                                 F@_31,
                                                 TrUserData);
d_pfield_values_timestamp_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, E, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                             F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_timestamp_millisecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              NewSeq,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_timestamp_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_timestamp_millisecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_timestamp_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_timestamp_millisecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_timestamp_millisecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_timestamp_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_timestamp_microsecond_values(Rest,
                                                N + 7,
                                                X bsl N + Acc,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
d_field_values_timestamp_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, Prev, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              cons(NewFValue, Prev, TrUserData),
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_timestamp_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                             F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_timestamp_microsecond_values(Rest,
                                                 N + 7,
                                                 X bsl N + Acc,
                                                 F,
                                                 F@_1,
                                                 F@_2,
                                                 F@_3,
                                                 F@_4,
                                                 F@_5,
                                                 F@_6,
                                                 F@_7,
                                                 F@_8,
                                                 F@_9,
                                                 F@_10,
                                                 F@_11,
                                                 F@_12,
                                                 F@_13,
                                                 F@_14,
                                                 F@_15,
                                                 F@_16,
                                                 F@_17,
                                                 F@_18,
                                                 F@_19,
                                                 F@_20,
                                                 F@_21,
                                                 F@_22,
                                                 F@_23,
                                                 F@_24,
                                                 F@_25,
                                                 F@_26,
                                                 F@_27,
                                                 F@_28,
                                                 F@_29,
                                                 F@_30,
                                                 F@_31,
                                                 TrUserData);
d_pfield_values_timestamp_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, E, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                             F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_timestamp_microsecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              NewSeq,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_timestamp_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_timestamp_microsecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_timestamp_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_timestamp_microsecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_timestamp_microsecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_timestamp_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_timestamp_nanosecond_values(Rest,
                                               N + 7,
                                               X bsl N + Acc,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
d_field_values_timestamp_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, Prev, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              cons(NewFValue, Prev, TrUserData),
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_timestamp_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_timestamp_nanosecond_values(Rest,
                                                N + 7,
                                                X bsl N + Acc,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
d_pfield_values_timestamp_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, E, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                            F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_timestamp_nanosecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              NewSeq,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_timestamp_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_timestamp_nanosecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_timestamp_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_timestamp_nanosecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_timestamp_nanosecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_time_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                  F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_time_second_values(Rest,
                                      N + 7,
                                      X bsl N + Acc,
                                      F,
                                      F@_1,
                                      F@_2,
                                      F@_3,
                                      F@_4,
                                      F@_5,
                                      F@_6,
                                      F@_7,
                                      F@_8,
                                      F@_9,
                                      F@_10,
                                      F@_11,
                                      F@_12,
                                      F@_13,
                                      F@_14,
                                      F@_15,
                                      F@_16,
                                      F@_17,
                                      F@_18,
                                      F@_19,
                                      F@_20,
                                      F@_21,
                                      F@_22,
                                      F@_23,
                                      F@_24,
                                      F@_25,
                                      F@_26,
                                      F@_27,
                                      F@_28,
                                      F@_29,
                                      F@_30,
                                      F@_31,
                                      TrUserData);
d_field_values_time_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, Prev, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                                  F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              cons(NewFValue, Prev, TrUserData),
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_time_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                   F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_time_second_values(Rest,
                                       N + 7,
                                       X bsl N + Acc,
                                       F,
                                       F@_1,
                                       F@_2,
                                       F@_3,
                                       F@_4,
                                       F@_5,
                                       F@_6,
                                       F@_7,
                                       F@_8,
                                       F@_9,
                                       F@_10,
                                       F@_11,
                                       F@_12,
                                       F@_13,
                                       F@_14,
                                       F@_15,
                                       F@_16,
                                       F@_17,
                                       F@_18,
                                       F@_19,
                                       F@_20,
                                       F@_21,
                                       F@_22,
                                       F@_23,
                                       F@_24,
                                       F@_25,
                                       F@_26,
                                       F@_27,
                                       F@_28,
                                       F@_29,
                                       F@_30,
                                       F@_31,
                                       TrUserData);
d_pfield_values_time_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, E, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                                   F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_time_second_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              NewSeq,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_time_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_time_second_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_time_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_time_second_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_time_second_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_time_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_time_millisecond_values(Rest,
                                           N + 7,
                                           X bsl N + Acc,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
d_field_values_time_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, Prev, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              cons(NewFValue, Prev, TrUserData),
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_time_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_time_millisecond_values(Rest,
                                            N + 7,
                                            X bsl N + Acc,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
d_pfield_values_time_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, E, F@_22, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_time_millisecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              NewSeq,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_time_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_time_millisecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_time_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_time_millisecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_time_millisecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_time_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_time_microsecond_values(Rest,
                                           N + 7,
                                           X bsl N + Acc,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
d_field_values_time_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, Prev, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              cons(NewFValue, Prev, TrUserData),
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_time_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_time_microsecond_values(Rest,
                                            N + 7,
                                            X bsl N + Acc,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
d_pfield_values_time_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, E, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_time_microsecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              NewSeq,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_time_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_time_microsecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_time_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_time_microsecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_time_microsecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_time_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                      F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_time_nanosecond_values(Rest,
                                          N + 7,
                                          X bsl N + Acc,
                                          F,
                                          F@_1,
                                          F@_2,
                                          F@_3,
                                          F@_4,
                                          F@_5,
                                          F@_6,
                                          F@_7,
                                          F@_8,
                                          F@_9,
                                          F@_10,
                                          F@_11,
                                          F@_12,
                                          F@_13,
                                          F@_14,
                                          F@_15,
                                          F@_16,
                                          F@_17,
                                          F@_18,
                                          F@_19,
                                          F@_20,
                                          F@_21,
                                          F@_22,
                                          F@_23,
                                          F@_24,
                                          F@_25,
                                          F@_26,
                                          F@_27,
                                          F@_28,
                                          F@_29,
                                          F@_30,
                                          F@_31,
                                          TrUserData);
d_field_values_time_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, Prev, F@_24, F@_25, F@_26,
                                      F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              cons(NewFValue, Prev, TrUserData),
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_time_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_time_nanosecond_values(Rest,
                                           N + 7,
                                           X bsl N + Acc,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
d_pfield_values_time_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, E, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_time_nanosecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              NewSeq,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_time_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_time_nanosecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_time_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_time_nanosecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_time_nanosecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_interval_year_month_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                          F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_interval_year_month_values(Rest,
                                              N + 7,
                                              X bsl N + Acc,
                                              F,
                                              F@_1,
                                              F@_2,
                                              F@_3,
                                              F@_4,
                                              F@_5,
                                              F@_6,
                                              F@_7,
                                              F@_8,
                                              F@_9,
                                              F@_10,
                                              F@_11,
                                              F@_12,
                                              F@_13,
                                              F@_14,
                                              F@_15,
                                              F@_16,
                                              F@_17,
                                              F@_18,
                                              F@_19,
                                              F@_20,
                                              F@_21,
                                              F@_22,
                                              F@_23,
                                              F@_24,
                                              F@_25,
                                              F@_26,
                                              F@_27,
                                              F@_28,
                                              F@_29,
                                              F@_30,
                                              F@_31,
                                              TrUserData);
d_field_values_interval_year_month_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, Prev, F@_25,
                                          F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              cons(NewFValue, Prev, TrUserData),
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_interval_year_month_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_interval_year_month_values(Rest,
                                               N + 7,
                                               X bsl N + Acc,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
d_pfield_values_interval_year_month_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, E, F@_25, F@_26,
                                           F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_interval_year_month_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              NewSeq,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_interval_year_month_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_interval_year_month_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_interval_year_month_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_interval_year_month_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_interval_year_month_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_interval_day_time_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_interval_day_time_values(Rest,
                                            N + 7,
                                            X bsl N + Acc,
                                            F,
                                            F@_1,
                                            F@_2,
                                            F@_3,
                                            F@_4,
                                            F@_5,
                                            F@_6,
                                            F@_7,
                                            F@_8,
                                            F@_9,
                                            F@_10,
                                            F@_11,
                                            F@_12,
                                            F@_13,
                                            F@_14,
                                            F@_15,
                                            F@_16,
                                            F@_17,
                                            F@_18,
                                            F@_19,
                                            F@_20,
                                            F@_21,
                                            F@_22,
                                            F@_23,
                                            F@_24,
                                            F@_25,
                                            F@_26,
                                            F@_27,
                                            F@_28,
                                            F@_29,
                                            F@_30,
                                            F@_31,
                                            TrUserData);
d_field_values_interval_day_time_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, Prev, F@_26,
                                        F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              cons(NewFValue, Prev, TrUserData),
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_interval_day_time_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                         F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_interval_day_time_values(Rest,
                                             N + 7,
                                             X bsl N + Acc,
                                             F,
                                             F@_1,
                                             F@_2,
                                             F@_3,
                                             F@_4,
                                             F@_5,
                                             F@_6,
                                             F@_7,
                                             F@_8,
                                             F@_9,
                                             F@_10,
                                             F@_11,
                                             F@_12,
                                             F@_13,
                                             F@_14,
                                             F@_15,
                                             F@_16,
                                             F@_17,
                                             F@_18,
                                             F@_19,
                                             F@_20,
                                             F@_21,
                                             F@_22,
                                             F@_23,
                                             F@_24,
                                             F@_25,
                                             F@_26,
                                             F@_27,
                                             F@_28,
                                             F@_29,
                                             F@_30,
                                             F@_31,
                                             TrUserData);
d_pfield_values_interval_day_time_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, E, F@_26,
                                         F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_interval_day_time_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              NewSeq,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_interval_day_time_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_interval_day_time_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_interval_day_time_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_interval_day_time_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_interval_day_time_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_interval_month_day_nano_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                              F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_interval_month_day_nano_values(Rest,
                                                  N + 7,
                                                  X bsl N + Acc,
                                                  F,
                                                  F@_1,
                                                  F@_2,
                                                  F@_3,
                                                  F@_4,
                                                  F@_5,
                                                  F@_6,
                                                  F@_7,
                                                  F@_8,
                                                  F@_9,
                                                  F@_10,
                                                  F@_11,
                                                  F@_12,
                                                  F@_13,
                                                  F@_14,
                                                  F@_15,
                                                  F@_16,
                                                  F@_17,
                                                  F@_18,
                                                  F@_19,
                                                  F@_20,
                                                  F@_21,
                                                  F@_22,
                                                  F@_23,
                                                  F@_24,
                                                  F@_25,
                                                  F@_26,
                                                  F@_27,
                                                  F@_28,
                                                  F@_29,
                                                  F@_30,
                                                  F@_31,
                                                  TrUserData);
d_field_values_interval_month_day_nano_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                              Prev, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_interval_month_day_nano(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              cons(NewFValue, Prev, TrUserData),
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_field_values_duration_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                      F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_duration_second_values(Rest,
                                          N + 7,
                                          X bsl N + Acc,
                                          F,
                                          F@_1,
                                          F@_2,
                                          F@_3,
                                          F@_4,
                                          F@_5,
                                          F@_6,
                                          F@_7,
                                          F@_8,
                                          F@_9,
                                          F@_10,
                                          F@_11,
                                          F@_12,
                                          F@_13,
                                          F@_14,
                                          F@_15,
                                          F@_16,
                                          F@_17,
                                          F@_18,
                                          F@_19,
                                          F@_20,
                                          F@_21,
                                          F@_22,
                                          F@_23,
                                          F@_24,
                                          F@_25,
                                          F@_26,
                                          F@_27,
                                          F@_28,
                                          F@_29,
                                          F@_30,
                                          F@_31,
                                          TrUserData);
d_field_values_duration_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                      Prev, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              cons(NewFValue, Prev, TrUserData),
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_duration_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_duration_second_values(Rest,
                                           N + 7,
                                           X bsl N + Acc,
                                           F,
                                           F@_1,
                                           F@_2,
                                           F@_3,
                                           F@_4,
                                           F@_5,
                                           F@_6,
                                           F@_7,
                                           F@_8,
                                           F@_9,
                                           F@_10,
                                           F@_11,
                                           F@_12,
                                           F@_13,
                                           F@_14,
                                           F@_15,
                                           F@_16,
                                           F@_17,
                                           F@_18,
                                           F@_19,
                                           F@_20,
                                           F@_21,
                                           F@_22,
                                           F@_23,
                                           F@_24,
                                           F@_25,
                                           F@_26,
                                           F@_27,
                                           F@_28,
                                           F@_29,
                                           F@_30,
                                           F@_31,
                                           TrUserData);
d_pfield_values_duration_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26,
                                       E, F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_duration_second_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              NewSeq,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_duration_second_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_duration_second_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_duration_second_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_duration_second_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_duration_second_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_duration_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_duration_millisecond_values(Rest,
                                               N + 7,
                                               X bsl N + Acc,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
d_field_values_duration_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, Prev, F@_29, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              cons(NewFValue, Prev, TrUserData),
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_duration_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_duration_millisecond_values(Rest,
                                                N + 7,
                                                X bsl N + Acc,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
d_pfield_values_duration_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, E, F@_29, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_duration_millisecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              NewSeq,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_duration_millisecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_duration_millisecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_duration_millisecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_duration_millisecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_duration_millisecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_duration_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_duration_microsecond_values(Rest,
                                               N + 7,
                                               X bsl N + Acc,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
d_field_values_duration_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, Prev, F@_30, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              cons(NewFValue, Prev, TrUserData),
                              F@_30,
                              F@_31,
                              TrUserData).

d_pfield_values_duration_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_duration_microsecond_values(Rest,
                                                N + 7,
                                                X bsl N + Acc,
                                                F,
                                                F@_1,
                                                F@_2,
                                                F@_3,
                                                F@_4,
                                                F@_5,
                                                F@_6,
                                                F@_7,
                                                F@_8,
                                                F@_9,
                                                F@_10,
                                                F@_11,
                                                F@_12,
                                                F@_13,
                                                F@_14,
                                                F@_15,
                                                F@_16,
                                                F@_17,
                                                F@_18,
                                                F@_19,
                                                F@_20,
                                                F@_21,
                                                F@_22,
                                                F@_23,
                                                F@_24,
                                                F@_25,
                                                F@_26,
                                                F@_27,
                                                F@_28,
                                                F@_29,
                                                F@_30,
                                                F@_31,
                                                TrUserData);
d_pfield_values_duration_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                            F@_26, F@_27, F@_28, E, F@_30, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_duration_microsecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              NewSeq,
                              F@_30,
                              F@_31,
                              TrUserData).

d_packed_field_values_duration_microsecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_duration_microsecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_duration_microsecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_duration_microsecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_duration_microsecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_duration_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                          F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_duration_nanosecond_values(Rest,
                                              N + 7,
                                              X bsl N + Acc,
                                              F,
                                              F@_1,
                                              F@_2,
                                              F@_3,
                                              F@_4,
                                              F@_5,
                                              F@_6,
                                              F@_7,
                                              F@_8,
                                              F@_9,
                                              F@_10,
                                              F@_11,
                                              F@_12,
                                              F@_13,
                                              F@_14,
                                              F@_15,
                                              F@_16,
                                              F@_17,
                                              F@_18,
                                              F@_19,
                                              F@_20,
                                              F@_21,
                                              F@_22,
                                              F@_23,
                                              F@_24,
                                              F@_25,
                                              F@_26,
                                              F@_27,
                                              F@_28,
                                              F@_29,
                                              F@_30,
                                              F@_31,
                                              TrUserData);
d_field_values_duration_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                          F@_26, F@_27, F@_28, F@_29, Prev, F@_31, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              cons(NewFValue, Prev, TrUserData),
                              F@_31,
                              TrUserData).

d_pfield_values_duration_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_pfield_values_duration_nanosecond_values(Rest,
                                               N + 7,
                                               X bsl N + Acc,
                                               F,
                                               F@_1,
                                               F@_2,
                                               F@_3,
                                               F@_4,
                                               F@_5,
                                               F@_6,
                                               F@_7,
                                               F@_8,
                                               F@_9,
                                               F@_10,
                                               F@_11,
                                               F@_12,
                                               F@_13,
                                               F@_14,
                                               F@_15,
                                               F@_16,
                                               F@_17,
                                               F@_18,
                                               F@_19,
                                               F@_20,
                                               F@_21,
                                               F@_22,
                                               F@_23,
                                               F@_24,
                                               F@_25,
                                               F@_26,
                                               F@_27,
                                               F@_28,
                                               F@_29,
                                               F@_30,
                                               F@_31,
                                               TrUserData);
d_pfield_values_duration_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25,
                                           F@_26, F@_27, F@_28, F@_29, E, F@_31, TrUserData) ->
    Len = X bsl N + Acc,
    <<PackedBytes:Len/binary, Rest2/binary>> = Rest,
    NewSeq = d_packed_field_values_duration_nanosecond_values(PackedBytes, 0, 0, F, E, TrUserData),
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              NewSeq,
                              F@_31,
                              TrUserData).

d_packed_field_values_duration_nanosecond_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) when N < 57 -> d_packed_field_values_duration_nanosecond_values(Rest, N + 7, X bsl N + Acc, F, AccSeq, TrUserData);
d_packed_field_values_duration_nanosecond_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, AccSeq, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    d_packed_field_values_duration_nanosecond_values(RestF, 0, 0, F, [NewFValue | AccSeq], TrUserData);
d_packed_field_values_duration_nanosecond_values(<<>>, 0, 0, _, AccSeq, _) -> AccSeq.

d_field_values_decimal128_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                                 F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    d_field_values_decimal128_values(Rest,
                                     N + 7,
                                     X bsl N + Acc,
                                     F,
                                     F@_1,
                                     F@_2,
                                     F@_3,
                                     F@_4,
                                     F@_5,
                                     F@_6,
                                     F@_7,
                                     F@_8,
                                     F@_9,
                                     F@_10,
                                     F@_11,
                                     F@_12,
                                     F@_13,
                                     F@_14,
                                     F@_15,
                                     F@_16,
                                     F@_17,
                                     F@_18,
                                     F@_19,
                                     F@_20,
                                     F@_21,
                                     F@_22,
                                     F@_23,
                                     F@_24,
                                     F@_25,
                                     F@_26,
                                     F@_27,
                                     F@_28,
                                     F@_29,
                                     F@_30,
                                     F@_31,
                                     TrUserData);
d_field_values_decimal128_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                                 F@_28, F@_29, F@_30, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_decimal_128(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_values(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              cons(NewFValue, Prev, TrUserData),
                              TrUserData).

skip_varint_values(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29,
                   F@_30, F@_31, TrUserData) ->
    skip_varint_values(Rest,
                       Z1,
                       Z2,
                       F,
                       F@_1,
                       F@_2,
                       F@_3,
                       F@_4,
                       F@_5,
                       F@_6,
                       F@_7,
                       F@_8,
                       F@_9,
                       F@_10,
                       F@_11,
                       F@_12,
                       F@_13,
                       F@_14,
                       F@_15,
                       F@_16,
                       F@_17,
                       F@_18,
                       F@_19,
                       F@_20,
                       F@_21,
                       F@_22,
                       F@_23,
                       F@_24,
                       F@_25,
                       F@_26,
                       F@_27,
                       F@_28,
                       F@_29,
                       F@_30,
                       F@_31,
                       TrUserData);
skip_varint_values(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29,
                   F@_30, F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

skip_length_delimited_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                             F@_28, F@_29, F@_30, F@_31, TrUserData)
    when N < 57 ->
    skip_length_delimited_values(Rest,
                                 N + 7,
                                 X bsl N + Acc,
                                 F,
                                 F@_1,
                                 F@_2,
                                 F@_3,
                                 F@_4,
                                 F@_5,
                                 F@_6,
                                 F@_7,
                                 F@_8,
                                 F@_9,
                                 F@_10,
                                 F@_11,
                                 F@_12,
                                 F@_13,
                                 F@_14,
                                 F@_15,
                                 F@_16,
                                 F@_17,
                                 F@_18,
                                 F@_19,
                                 F@_20,
                                 F@_21,
                                 F@_22,
                                 F@_23,
                                 F@_24,
                                 F@_25,
                                 F@_26,
                                 F@_27,
                                 F@_28,
                                 F@_29,
                                 F@_30,
                                 F@_31,
                                 TrUserData);
skip_length_delimited_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27,
                             F@_28, F@_29, F@_30, F@_31, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_values(Rest2,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

skip_group_values(Bin, _, Z2, FNum, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29, F@_30, F@_31,
                  TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_values(Rest,
                              0,
                              Z2,
                              FNum,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

skip_32_values(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29, F@_30,
               F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

skip_64_values(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, F@_7, F@_8, F@_9, F@_10, F@_11, F@_12, F@_13, F@_14, F@_15, F@_16, F@_17, F@_18, F@_19, F@_20, F@_21, F@_22, F@_23, F@_24, F@_25, F@_26, F@_27, F@_28, F@_29, F@_30,
               F@_31, TrUserData) ->
    dfp_read_field_def_values(Rest,
                              Z1,
                              Z2,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              F@_6,
                              F@_7,
                              F@_8,
                              F@_9,
                              F@_10,
                              F@_11,
                              F@_12,
                              F@_13,
                              F@_14,
                              F@_15,
                              F@_16,
                              F@_17,
                              F@_18,
                              F@_19,
                              F@_20,
                              F@_21,
                              F@_22,
                              F@_23,
                              F@_24,
                              F@_25,
                              F@_26,
                              F@_27,
                              F@_28,
                              F@_29,
                              F@_30,
                              F@_31,
                              TrUserData).

decode_msg_column(Bin, TrUserData) -> dfp_read_field_def_column(Bin, 0, 0, 0, id(<<>>, TrUserData), id('TAG', TrUserData), id('$undef', TrUserData), id(<<>>, TrUserData), id('BOOLEAN', TrUserData), id('$undef', TrUserData), TrUserData).

dfp_read_field_def_column(<<10, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> d_field_column_column_name(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dfp_read_field_def_column(<<16, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> d_field_column_semantic_type(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dfp_read_field_def_column(<<26, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> d_field_column_values(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dfp_read_field_def_column(<<34, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> d_field_column_null_mask(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dfp_read_field_def_column(<<40, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> d_field_column_datatype(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dfp_read_field_def_column(<<50, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> d_field_column_datatype_extension(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dfp_read_field_def_column(<<>>, 0, 0, _, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, _) ->
    S1 = #{column_name => F@_1, semantic_type => F@_2, null_mask => F@_4, datatype => F@_5},
    S2 = if F@_3 == '$undef' -> S1;
            true -> S1#{values => F@_3}
         end,
    if F@_6 == '$undef' -> S2;
       true -> S2#{datatype_extension => F@_6}
    end;
dfp_read_field_def_column(Other, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> dg_read_field_def_column(Other, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

dg_read_field_def_column(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 32 - 7 -> dg_read_field_def_column(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
dg_read_field_def_column(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_column_column_name(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
        16 -> d_field_column_semantic_type(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
        26 -> d_field_column_values(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
        34 -> d_field_column_null_mask(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
        40 -> d_field_column_datatype(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
        50 -> d_field_column_datatype_extension(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_column(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
                1 -> skip_64_column(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
                2 -> skip_length_delimited_column(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
                3 -> skip_group_column(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
                5 -> skip_32_column(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData)
            end
    end;
dg_read_field_def_column(<<>>, 0, 0, _, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, _) ->
    S1 = #{column_name => F@_1, semantic_type => F@_2, null_mask => F@_4, datatype => F@_5},
    S2 = if F@_3 == '$undef' -> S1;
            true -> S1#{values => F@_3}
         end,
    if F@_6 == '$undef' -> S2;
       true -> S2#{datatype_extension => F@_6}
    end.

d_field_column_column_name(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> d_field_column_column_name(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
d_field_column_column_name(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_column(RestF, 0, 0, F, NewFValue, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

d_field_column_semantic_type(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> d_field_column_semantic_type(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
d_field_column_semantic_type(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, F@_3, F@_4, F@_5, F@_6, TrUserData) ->
    {NewFValue, RestF} = {id('d_enum_greptime.v1.SemanticType'(begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end), TrUserData), Rest},
    dfp_read_field_def_column(RestF, 0, 0, F, F@_1, NewFValue, F@_3, F@_4, F@_5, F@_6, TrUserData).

d_field_column_values(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> d_field_column_values(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
d_field_column_values(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, Prev, F@_4, F@_5, F@_6, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_values(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_column(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              if Prev == '$undef' -> NewFValue;
                                 true -> merge_msg_values(Prev, NewFValue, TrUserData)
                              end,
                              F@_4,
                              F@_5,
                              F@_6,
                              TrUserData).

d_field_column_null_mask(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> d_field_column_null_mask(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
d_field_column_null_mask(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, _, F@_5, F@_6, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_column(RestF, 0, 0, F, F@_1, F@_2, F@_3, NewFValue, F@_5, F@_6, TrUserData).

d_field_column_datatype(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> d_field_column_datatype(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
d_field_column_datatype(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, _, F@_6, TrUserData) ->
    {NewFValue, RestF} = {id('d_enum_greptime.v1.ColumnDataType'(begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end), TrUserData), Rest},
    dfp_read_field_def_column(RestF, 0, 0, F, F@_1, F@_2, F@_3, F@_4, NewFValue, F@_6, TrUserData).

d_field_column_datatype_extension(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> d_field_column_datatype_extension(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
d_field_column_datatype_extension(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_column_data_type_extension(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_column(RestF,
                              0,
                              0,
                              F,
                              F@_1,
                              F@_2,
                              F@_3,
                              F@_4,
                              F@_5,
                              if Prev == '$undef' -> NewFValue;
                                 true -> merge_msg_column_data_type_extension(Prev, NewFValue, TrUserData)
                              end,
                              TrUserData).

skip_varint_column(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> skip_varint_column(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
skip_varint_column(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> dfp_read_field_def_column(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

skip_length_delimited_column(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) when N < 57 -> skip_length_delimited_column(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData);
skip_length_delimited_column(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_column(Rest2, 0, 0, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

skip_group_column(Bin, _, Z2, FNum, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_column(Rest, 0, Z2, FNum, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

skip_32_column(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> dfp_read_field_def_column(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

skip_64_column(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData) -> dfp_read_field_def_column(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, F@_6, TrUserData).

decode_msg_request_header(Bin, TrUserData) ->
    dfp_read_field_def_request_header(Bin, 0, 0, 0, id(<<>>, TrUserData), id(<<>>, TrUserData), id('$undef', TrUserData), id(<<>>, TrUserData), 'tr_decode_init_default_request_header.tracing_context'([], TrUserData), TrUserData).

dfp_read_field_def_request_header(<<10, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> d_field_request_header_catalog(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
dfp_read_field_def_request_header(<<18, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> d_field_request_header_schema(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
dfp_read_field_def_request_header(<<26, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> d_field_request_header_authorization(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
dfp_read_field_def_request_header(<<34, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> d_field_request_header_dbname(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
dfp_read_field_def_request_header(<<42, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> d_field_request_header_tracing_context(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
dfp_read_field_def_request_header(<<>>, 0, 0, _, F@_1, F@_2, F@_3, F@_4, R1, TrUserData) ->
    S1 = #{catalog => F@_1, schema => F@_2, dbname => F@_4, tracing_context => 'tr_decode_repeated_finalize_request_header.tracing_context'(R1, TrUserData)},
    if F@_3 == '$undef' -> S1;
       true -> S1#{authorization => F@_3}
    end;
dfp_read_field_def_request_header(Other, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> dg_read_field_def_request_header(Other, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData).

dg_read_field_def_request_header(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 32 - 7 -> dg_read_field_def_request_header(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
dg_read_field_def_request_header(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_request_header_catalog(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
        18 -> d_field_request_header_schema(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
        26 -> d_field_request_header_authorization(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
        34 -> d_field_request_header_dbname(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
        42 -> d_field_request_header_tracing_context(Rest, 0, 0, 0, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_request_header(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
                1 -> skip_64_request_header(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
                2 -> skip_length_delimited_request_header(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
                3 -> skip_group_request_header(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
                5 -> skip_32_request_header(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData)
            end
    end;
dg_read_field_def_request_header(<<>>, 0, 0, _, F@_1, F@_2, F@_3, F@_4, R1, TrUserData) ->
    S1 = #{catalog => F@_1, schema => F@_2, dbname => F@_4, tracing_context => 'tr_decode_repeated_finalize_request_header.tracing_context'(R1, TrUserData)},
    if F@_3 == '$undef' -> S1;
       true -> S1#{authorization => F@_3}
    end.

d_field_request_header_catalog(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 57 -> d_field_request_header_catalog(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
d_field_request_header_catalog(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, F@_3, F@_4, F@_5, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_request_header(RestF, 0, 0, F, NewFValue, F@_2, F@_3, F@_4, F@_5, TrUserData).

d_field_request_header_schema(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 57 -> d_field_request_header_schema(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
d_field_request_header_schema(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, F@_3, F@_4, F@_5, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_request_header(RestF, 0, 0, F, F@_1, NewFValue, F@_3, F@_4, F@_5, TrUserData).

d_field_request_header_authorization(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 57 -> d_field_request_header_authorization(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
d_field_request_header_authorization(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, Prev, F@_4, F@_5, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_auth_header(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_request_header(RestF,
                                      0,
                                      0,
                                      F,
                                      F@_1,
                                      F@_2,
                                      if Prev == '$undef' -> NewFValue;
                                         true -> merge_msg_auth_header(Prev, NewFValue, TrUserData)
                                      end,
                                      F@_4,
                                      F@_5,
                                      TrUserData).

d_field_request_header_dbname(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 57 -> d_field_request_header_dbname(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
d_field_request_header_dbname(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, _, F@_5, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_request_header(RestF, 0, 0, F, F@_1, F@_2, F@_3, NewFValue, F@_5, TrUserData).

d_field_request_header_tracing_context(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 57 -> d_field_request_header_tracing_context(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
d_field_request_header_tracing_context(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id('decode_msg_map<string,string>'(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_request_header(RestF, 0, 0, F, F@_1, F@_2, F@_3, F@_4, 'tr_decode_repeated_add_elem_request_header.tracing_context'(NewFValue, Prev, TrUserData), TrUserData).

skip_varint_request_header(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> skip_varint_request_header(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
skip_varint_request_header(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> dfp_read_field_def_request_header(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData).

skip_length_delimited_request_header(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) when N < 57 -> skip_length_delimited_request_header(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData);
skip_length_delimited_request_header(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_request_header(Rest2, 0, 0, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData).

skip_group_request_header(Bin, _, Z2, FNum, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_request_header(Rest, 0, Z2, FNum, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData).

skip_32_request_header(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> dfp_read_field_def_request_header(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData).

skip_64_request_header(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData) -> dfp_read_field_def_request_header(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, F@_4, F@_5, TrUserData).

decode_msg_response_header(Bin, TrUserData) -> dfp_read_field_def_response_header(Bin, 0, 0, 0, id('$undef', TrUserData), TrUserData).

dfp_read_field_def_response_header(<<10, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_response_header_status(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_response_header(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{status => F@_1}
    end;
dfp_read_field_def_response_header(Other, Z1, Z2, F, F@_1, TrUserData) -> dg_read_field_def_response_header(Other, Z1, Z2, F, F@_1, TrUserData).

dg_read_field_def_response_header(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 32 - 7 -> dg_read_field_def_response_header(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
dg_read_field_def_response_header(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_response_header_status(Rest, 0, 0, 0, F@_1, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_response_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                1 -> skip_64_response_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                2 -> skip_length_delimited_response_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                3 -> skip_group_response_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                5 -> skip_32_response_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData)
            end
    end;
dg_read_field_def_response_header(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{status => F@_1}
    end.

d_field_response_header_status(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_response_header_status(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_response_header_status(<<0:1, X:7, Rest/binary>>, N, Acc, F, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_status(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_response_header(RestF,
                                       0,
                                       0,
                                       F,
                                       if Prev == '$undef' -> NewFValue;
                                          true -> merge_msg_status(Prev, NewFValue, TrUserData)
                                       end,
                                       TrUserData).

skip_varint_response_header(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> skip_varint_response_header(Rest, Z1, Z2, F, F@_1, TrUserData);
skip_varint_response_header(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_response_header(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_length_delimited_response_header(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> skip_length_delimited_response_header(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
skip_length_delimited_response_header(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_response_header(Rest2, 0, 0, F, F@_1, TrUserData).

skip_group_response_header(Bin, _, Z2, FNum, F@_1, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_response_header(Rest, 0, Z2, FNum, F@_1, TrUserData).

skip_32_response_header(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_response_header(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_64_response_header(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_response_header(Rest, Z1, Z2, F, F@_1, TrUserData).

decode_msg_status(Bin, TrUserData) -> dfp_read_field_def_status(Bin, 0, 0, 0, id(0, TrUserData), id(<<>>, TrUserData), TrUserData).

dfp_read_field_def_status(<<8, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_status_status_code(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_status(<<18, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_status_err_msg(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_status(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{status_code => F@_1, err_msg => F@_2};
dfp_read_field_def_status(Other, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dg_read_field_def_status(Other, Z1, Z2, F, F@_1, F@_2, TrUserData).

dg_read_field_def_status(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 32 - 7 -> dg_read_field_def_status(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
dg_read_field_def_status(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        8 -> d_field_status_status_code(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        18 -> d_field_status_err_msg(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_status(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                1 -> skip_64_status(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                2 -> skip_length_delimited_status(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                3 -> skip_group_status(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                5 -> skip_32_status(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData)
            end
    end;
dg_read_field_def_status(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{status_code => F@_1, err_msg => F@_2}.

d_field_status_status_code(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_status_status_code(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_status_status_code(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    dfp_read_field_def_status(RestF, 0, 0, F, NewFValue, F@_2, TrUserData).

d_field_status_err_msg(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_status_err_msg(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_status_err_msg(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_status(RestF, 0, 0, F, F@_1, NewFValue, TrUserData).

skip_varint_status(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> skip_varint_status(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
skip_varint_status(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_status(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_length_delimited_status(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> skip_length_delimited_status(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
skip_length_delimited_status(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_status(Rest2, 0, 0, F, F@_1, F@_2, TrUserData).

skip_group_status(Bin, _, Z2, FNum, F@_1, F@_2, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_status(Rest, 0, Z2, FNum, F@_1, F@_2, TrUserData).

skip_32_status(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_status(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_64_status(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_status(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

decode_msg_auth_header(Bin, TrUserData) -> dfp_read_field_def_auth_header(Bin, 0, 0, 0, id('$undef', TrUserData), TrUserData).

dfp_read_field_def_auth_header(<<10, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_auth_header_basic(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_auth_header(<<18, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_auth_header_token(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_auth_header(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{auth_scheme => F@_1}
    end;
dfp_read_field_def_auth_header(Other, Z1, Z2, F, F@_1, TrUserData) -> dg_read_field_def_auth_header(Other, Z1, Z2, F, F@_1, TrUserData).

dg_read_field_def_auth_header(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 32 - 7 -> dg_read_field_def_auth_header(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
dg_read_field_def_auth_header(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_auth_header_basic(Rest, 0, 0, 0, F@_1, TrUserData);
        18 -> d_field_auth_header_token(Rest, 0, 0, 0, F@_1, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_auth_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                1 -> skip_64_auth_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                2 -> skip_length_delimited_auth_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                3 -> skip_group_auth_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                5 -> skip_32_auth_header(Rest, 0, 0, Key bsr 3, F@_1, TrUserData)
            end
    end;
dg_read_field_def_auth_header(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{auth_scheme => F@_1}
    end.

d_field_auth_header_basic(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_auth_header_basic(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_auth_header_basic(<<0:1, X:7, Rest/binary>>, N, Acc, F, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_basic(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_auth_header(RestF,
                                   0,
                                   0,
                                   F,
                                   case Prev of
                                       '$undef' -> id({basic, NewFValue}, TrUserData);
                                       {basic, MVPrev} -> id({basic, merge_msg_basic(MVPrev, NewFValue, TrUserData)}, TrUserData);
                                       _ -> id({basic, NewFValue}, TrUserData)
                                   end,
                                   TrUserData).

d_field_auth_header_token(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_auth_header_token(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_auth_header_token(<<0:1, X:7, Rest/binary>>, N, Acc, F, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_token(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_auth_header(RestF,
                                   0,
                                   0,
                                   F,
                                   case Prev of
                                       '$undef' -> id({token, NewFValue}, TrUserData);
                                       {token, MVPrev} -> id({token, merge_msg_token(MVPrev, NewFValue, TrUserData)}, TrUserData);
                                       _ -> id({token, NewFValue}, TrUserData)
                                   end,
                                   TrUserData).

skip_varint_auth_header(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> skip_varint_auth_header(Rest, Z1, Z2, F, F@_1, TrUserData);
skip_varint_auth_header(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_auth_header(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_length_delimited_auth_header(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> skip_length_delimited_auth_header(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
skip_length_delimited_auth_header(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_auth_header(Rest2, 0, 0, F, F@_1, TrUserData).

skip_group_auth_header(Bin, _, Z2, FNum, F@_1, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_auth_header(Rest, 0, Z2, FNum, F@_1, TrUserData).

skip_32_auth_header(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_auth_header(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_64_auth_header(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_auth_header(Rest, Z1, Z2, F, F@_1, TrUserData).

decode_msg_basic(Bin, TrUserData) -> dfp_read_field_def_basic(Bin, 0, 0, 0, id(<<>>, TrUserData), id(<<>>, TrUserData), TrUserData).

dfp_read_field_def_basic(<<10, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_basic_username(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_basic(<<18, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_basic_password(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_basic(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{username => F@_1, password => F@_2};
dfp_read_field_def_basic(Other, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dg_read_field_def_basic(Other, Z1, Z2, F, F@_1, F@_2, TrUserData).

dg_read_field_def_basic(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 32 - 7 -> dg_read_field_def_basic(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
dg_read_field_def_basic(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_basic_username(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        18 -> d_field_basic_password(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_basic(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                1 -> skip_64_basic(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                2 -> skip_length_delimited_basic(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                3 -> skip_group_basic(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                5 -> skip_32_basic(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData)
            end
    end;
dg_read_field_def_basic(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{username => F@_1, password => F@_2}.

d_field_basic_username(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_basic_username(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_basic_username(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_basic(RestF, 0, 0, F, NewFValue, F@_2, TrUserData).

d_field_basic_password(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_basic_password(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_basic_password(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_basic(RestF, 0, 0, F, F@_1, NewFValue, TrUserData).

skip_varint_basic(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> skip_varint_basic(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
skip_varint_basic(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_basic(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_length_delimited_basic(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> skip_length_delimited_basic(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
skip_length_delimited_basic(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_basic(Rest2, 0, 0, F, F@_1, F@_2, TrUserData).

skip_group_basic(Bin, _, Z2, FNum, F@_1, F@_2, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_basic(Rest, 0, Z2, FNum, F@_1, F@_2, TrUserData).

skip_32_basic(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_basic(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_64_basic(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_basic(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

decode_msg_token(Bin, TrUserData) -> dfp_read_field_def_token(Bin, 0, 0, 0, id(<<>>, TrUserData), TrUserData).

dfp_read_field_def_token(<<10, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_token_token(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_token(<<>>, 0, 0, _, F@_1, _) -> #{token => F@_1};
dfp_read_field_def_token(Other, Z1, Z2, F, F@_1, TrUserData) -> dg_read_field_def_token(Other, Z1, Z2, F, F@_1, TrUserData).

dg_read_field_def_token(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 32 - 7 -> dg_read_field_def_token(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
dg_read_field_def_token(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_token_token(Rest, 0, 0, 0, F@_1, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_token(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                1 -> skip_64_token(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                2 -> skip_length_delimited_token(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                3 -> skip_group_token(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                5 -> skip_32_token(Rest, 0, 0, Key bsr 3, F@_1, TrUserData)
            end
    end;
dg_read_field_def_token(<<>>, 0, 0, _, F@_1, _) -> #{token => F@_1}.

d_field_token_token(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_token_token(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_token_token(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    dfp_read_field_def_token(RestF, 0, 0, F, NewFValue, TrUserData).

skip_varint_token(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> skip_varint_token(Rest, Z1, Z2, F, F@_1, TrUserData);
skip_varint_token(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_token(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_length_delimited_token(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> skip_length_delimited_token(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
skip_length_delimited_token(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_token(Rest2, 0, 0, F, F@_1, TrUserData).

skip_group_token(Bin, _, Z2, FNum, F@_1, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_token(Rest, 0, Z2, FNum, F@_1, TrUserData).

skip_32_token(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_token(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_64_token(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_token(Rest, Z1, Z2, F, F@_1, TrUserData).

decode_msg_affected_rows(Bin, TrUserData) -> dfp_read_field_def_affected_rows(Bin, 0, 0, 0, id(0, TrUserData), TrUserData).

dfp_read_field_def_affected_rows(<<8, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_affected_rows_value(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_affected_rows(<<>>, 0, 0, _, F@_1, _) -> #{value => F@_1};
dfp_read_field_def_affected_rows(Other, Z1, Z2, F, F@_1, TrUserData) -> dg_read_field_def_affected_rows(Other, Z1, Z2, F, F@_1, TrUserData).

dg_read_field_def_affected_rows(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 32 - 7 -> dg_read_field_def_affected_rows(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
dg_read_field_def_affected_rows(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        8 -> d_field_affected_rows_value(Rest, 0, 0, 0, F@_1, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_affected_rows(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                1 -> skip_64_affected_rows(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                2 -> skip_length_delimited_affected_rows(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                3 -> skip_group_affected_rows(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                5 -> skip_32_affected_rows(Rest, 0, 0, Key bsr 3, F@_1, TrUserData)
            end
    end;
dg_read_field_def_affected_rows(<<>>, 0, 0, _, F@_1, _) -> #{value => F@_1}.

d_field_affected_rows_value(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_affected_rows_value(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_affected_rows_value(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, TrUserData) ->
    {NewFValue, RestF} = {id((X bsl N + Acc) band 4294967295, TrUserData), Rest},
    dfp_read_field_def_affected_rows(RestF, 0, 0, F, NewFValue, TrUserData).

skip_varint_affected_rows(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> skip_varint_affected_rows(Rest, Z1, Z2, F, F@_1, TrUserData);
skip_varint_affected_rows(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_affected_rows(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_length_delimited_affected_rows(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> skip_length_delimited_affected_rows(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
skip_length_delimited_affected_rows(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_affected_rows(Rest2, 0, 0, F, F@_1, TrUserData).

skip_group_affected_rows(Bin, _, Z2, FNum, F@_1, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_affected_rows(Rest, 0, Z2, FNum, F@_1, TrUserData).

skip_32_affected_rows(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_affected_rows(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_64_affected_rows(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_affected_rows(Rest, Z1, Z2, F, F@_1, TrUserData).

decode_msg_flight_metadata(Bin, TrUserData) -> dfp_read_field_def_flight_metadata(Bin, 0, 0, 0, id('$undef', TrUserData), TrUserData).

dfp_read_field_def_flight_metadata(<<10, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_flight_metadata_affected_rows(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_flight_metadata(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{affected_rows => F@_1}
    end;
dfp_read_field_def_flight_metadata(Other, Z1, Z2, F, F@_1, TrUserData) -> dg_read_field_def_flight_metadata(Other, Z1, Z2, F, F@_1, TrUserData).

dg_read_field_def_flight_metadata(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 32 - 7 -> dg_read_field_def_flight_metadata(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
dg_read_field_def_flight_metadata(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_flight_metadata_affected_rows(Rest, 0, 0, 0, F@_1, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_flight_metadata(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                1 -> skip_64_flight_metadata(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                2 -> skip_length_delimited_flight_metadata(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                3 -> skip_group_flight_metadata(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                5 -> skip_32_flight_metadata(Rest, 0, 0, Key bsr 3, F@_1, TrUserData)
            end
    end;
dg_read_field_def_flight_metadata(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{affected_rows => F@_1}
    end.

d_field_flight_metadata_affected_rows(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_flight_metadata_affected_rows(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_flight_metadata_affected_rows(<<0:1, X:7, Rest/binary>>, N, Acc, F, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_affected_rows(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_flight_metadata(RestF,
                                       0,
                                       0,
                                       F,
                                       if Prev == '$undef' -> NewFValue;
                                          true -> merge_msg_affected_rows(Prev, NewFValue, TrUserData)
                                       end,
                                       TrUserData).

skip_varint_flight_metadata(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> skip_varint_flight_metadata(Rest, Z1, Z2, F, F@_1, TrUserData);
skip_varint_flight_metadata(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_flight_metadata(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_length_delimited_flight_metadata(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> skip_length_delimited_flight_metadata(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
skip_length_delimited_flight_metadata(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_flight_metadata(Rest2, 0, 0, F, F@_1, TrUserData).

skip_group_flight_metadata(Bin, _, Z2, FNum, F@_1, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_flight_metadata(Rest, 0, Z2, FNum, F@_1, TrUserData).

skip_32_flight_metadata(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_flight_metadata(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_64_flight_metadata(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_flight_metadata(Rest, Z1, Z2, F, F@_1, TrUserData).

decode_msg_interval_month_day_nano(Bin, TrUserData) -> dfp_read_field_def_interval_month_day_nano(Bin, 0, 0, 0, id(0, TrUserData), id(0, TrUserData), id(0, TrUserData), TrUserData).

dfp_read_field_def_interval_month_day_nano(<<8, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> d_field_interval_month_day_nano_months(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData);
dfp_read_field_def_interval_month_day_nano(<<16, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> d_field_interval_month_day_nano_days(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData);
dfp_read_field_def_interval_month_day_nano(<<24, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> d_field_interval_month_day_nano_nanoseconds(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData);
dfp_read_field_def_interval_month_day_nano(<<>>, 0, 0, _, F@_1, F@_2, F@_3, _) -> #{months => F@_1, days => F@_2, nanoseconds => F@_3};
dfp_read_field_def_interval_month_day_nano(Other, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> dg_read_field_def_interval_month_day_nano(Other, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData).

dg_read_field_def_interval_month_day_nano(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, TrUserData) when N < 32 - 7 -> dg_read_field_def_interval_month_day_nano(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, TrUserData);
dg_read_field_def_interval_month_day_nano(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, F@_3, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        8 -> d_field_interval_month_day_nano_months(Rest, 0, 0, 0, F@_1, F@_2, F@_3, TrUserData);
        16 -> d_field_interval_month_day_nano_days(Rest, 0, 0, 0, F@_1, F@_2, F@_3, TrUserData);
        24 -> d_field_interval_month_day_nano_nanoseconds(Rest, 0, 0, 0, F@_1, F@_2, F@_3, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_interval_month_day_nano(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, TrUserData);
                1 -> skip_64_interval_month_day_nano(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, TrUserData);
                2 -> skip_length_delimited_interval_month_day_nano(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, TrUserData);
                3 -> skip_group_interval_month_day_nano(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, TrUserData);
                5 -> skip_32_interval_month_day_nano(Rest, 0, 0, Key bsr 3, F@_1, F@_2, F@_3, TrUserData)
            end
    end;
dg_read_field_def_interval_month_day_nano(<<>>, 0, 0, _, F@_1, F@_2, F@_3, _) -> #{months => F@_1, days => F@_2, nanoseconds => F@_3}.

d_field_interval_month_day_nano_months(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, TrUserData) when N < 57 -> d_field_interval_month_day_nano_months(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, TrUserData);
d_field_interval_month_day_nano_months(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, F@_3, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_interval_month_day_nano(RestF, 0, 0, F, NewFValue, F@_2, F@_3, TrUserData).

d_field_interval_month_day_nano_days(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, TrUserData) when N < 57 -> d_field_interval_month_day_nano_days(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, TrUserData);
d_field_interval_month_day_nano_days(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, F@_3, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_interval_month_day_nano(RestF, 0, 0, F, F@_1, NewFValue, F@_3, TrUserData).

d_field_interval_month_day_nano_nanoseconds(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, TrUserData) when N < 57 -> d_field_interval_month_day_nano_nanoseconds(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, TrUserData);
d_field_interval_month_day_nano_nanoseconds(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, _, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_interval_month_day_nano(RestF, 0, 0, F, F@_1, F@_2, NewFValue, TrUserData).

skip_varint_interval_month_day_nano(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> skip_varint_interval_month_day_nano(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData);
skip_varint_interval_month_day_nano(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> dfp_read_field_def_interval_month_day_nano(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData).

skip_length_delimited_interval_month_day_nano(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, TrUserData) when N < 57 -> skip_length_delimited_interval_month_day_nano(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, F@_3, TrUserData);
skip_length_delimited_interval_month_day_nano(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, F@_3, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_interval_month_day_nano(Rest2, 0, 0, F, F@_1, F@_2, F@_3, TrUserData).

skip_group_interval_month_day_nano(Bin, _, Z2, FNum, F@_1, F@_2, F@_3, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_interval_month_day_nano(Rest, 0, Z2, FNum, F@_1, F@_2, F@_3, TrUserData).

skip_32_interval_month_day_nano(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> dfp_read_field_def_interval_month_day_nano(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData).

skip_64_interval_month_day_nano(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData) -> dfp_read_field_def_interval_month_day_nano(Rest, Z1, Z2, F, F@_1, F@_2, F@_3, TrUserData).

decode_msg_decimal_128(Bin, TrUserData) -> dfp_read_field_def_decimal_128(Bin, 0, 0, 0, id(0, TrUserData), id(0, TrUserData), TrUserData).

dfp_read_field_def_decimal_128(<<8, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_decimal_128_hi(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_decimal_128(<<16, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_decimal_128_lo(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_decimal_128(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{hi => F@_1, lo => F@_2};
dfp_read_field_def_decimal_128(Other, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dg_read_field_def_decimal_128(Other, Z1, Z2, F, F@_1, F@_2, TrUserData).

dg_read_field_def_decimal_128(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 32 - 7 -> dg_read_field_def_decimal_128(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
dg_read_field_def_decimal_128(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        8 -> d_field_decimal_128_hi(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        16 -> d_field_decimal_128_lo(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_decimal_128(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                1 -> skip_64_decimal_128(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                2 -> skip_length_delimited_decimal_128(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                3 -> skip_group_decimal_128(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                5 -> skip_32_decimal_128(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData)
            end
    end;
dg_read_field_def_decimal_128(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{hi => F@_1, lo => F@_2}.

d_field_decimal_128_hi(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_decimal_128_hi(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_decimal_128_hi(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_decimal_128(RestF, 0, 0, F, NewFValue, F@_2, TrUserData).

d_field_decimal_128_lo(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_decimal_128_lo(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_decimal_128_lo(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:64/signed-native>> = <<(X bsl N + Acc):64/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_decimal_128(RestF, 0, 0, F, F@_1, NewFValue, TrUserData).

skip_varint_decimal_128(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> skip_varint_decimal_128(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
skip_varint_decimal_128(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_decimal_128(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_length_delimited_decimal_128(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> skip_length_delimited_decimal_128(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
skip_length_delimited_decimal_128(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_decimal_128(Rest2, 0, 0, F, F@_1, F@_2, TrUserData).

skip_group_decimal_128(Bin, _, Z2, FNum, F@_1, F@_2, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_decimal_128(Rest, 0, Z2, FNum, F@_1, F@_2, TrUserData).

skip_32_decimal_128(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_decimal_128(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_64_decimal_128(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_decimal_128(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

decode_msg_column_data_type_extension(Bin, TrUserData) -> dfp_read_field_def_column_data_type_extension(Bin, 0, 0, 0, id('$undef', TrUserData), TrUserData).

dfp_read_field_def_column_data_type_extension(<<10, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> d_field_column_data_type_extension_decimal_type(Rest, Z1, Z2, F, F@_1, TrUserData);
dfp_read_field_def_column_data_type_extension(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{type_ext => F@_1}
    end;
dfp_read_field_def_column_data_type_extension(Other, Z1, Z2, F, F@_1, TrUserData) -> dg_read_field_def_column_data_type_extension(Other, Z1, Z2, F, F@_1, TrUserData).

dg_read_field_def_column_data_type_extension(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 32 - 7 -> dg_read_field_def_column_data_type_extension(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
dg_read_field_def_column_data_type_extension(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> d_field_column_data_type_extension_decimal_type(Rest, 0, 0, 0, F@_1, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_column_data_type_extension(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                1 -> skip_64_column_data_type_extension(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                2 -> skip_length_delimited_column_data_type_extension(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                3 -> skip_group_column_data_type_extension(Rest, 0, 0, Key bsr 3, F@_1, TrUserData);
                5 -> skip_32_column_data_type_extension(Rest, 0, 0, Key bsr 3, F@_1, TrUserData)
            end
    end;
dg_read_field_def_column_data_type_extension(<<>>, 0, 0, _, F@_1, _) ->
    S1 = #{},
    if F@_1 == '$undef' -> S1;
       true -> S1#{type_ext => F@_1}
    end.

d_field_column_data_type_extension_decimal_type(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> d_field_column_data_type_extension_decimal_type(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
d_field_column_data_type_extension_decimal_type(<<0:1, X:7, Rest/binary>>, N, Acc, F, Prev, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bs:Len/binary, Rest2/binary>> = Rest, {id(decode_msg_decimal_type_extension(Bs, TrUserData), TrUserData), Rest2} end,
    dfp_read_field_def_column_data_type_extension(RestF,
                                                  0,
                                                  0,
                                                  F,
                                                  case Prev of
                                                      '$undef' -> id({decimal_type, NewFValue}, TrUserData);
                                                      {decimal_type, MVPrev} -> id({decimal_type, merge_msg_decimal_type_extension(MVPrev, NewFValue, TrUserData)}, TrUserData);
                                                      _ -> id({decimal_type, NewFValue}, TrUserData)
                                                  end,
                                                  TrUserData).

skip_varint_column_data_type_extension(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> skip_varint_column_data_type_extension(Rest, Z1, Z2, F, F@_1, TrUserData);
skip_varint_column_data_type_extension(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_column_data_type_extension(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_length_delimited_column_data_type_extension(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) when N < 57 -> skip_length_delimited_column_data_type_extension(Rest, N + 7, X bsl N + Acc, F, F@_1, TrUserData);
skip_length_delimited_column_data_type_extension(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_column_data_type_extension(Rest2, 0, 0, F, F@_1, TrUserData).

skip_group_column_data_type_extension(Bin, _, Z2, FNum, F@_1, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_column_data_type_extension(Rest, 0, Z2, FNum, F@_1, TrUserData).

skip_32_column_data_type_extension(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_column_data_type_extension(Rest, Z1, Z2, F, F@_1, TrUserData).

skip_64_column_data_type_extension(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, TrUserData) -> dfp_read_field_def_column_data_type_extension(Rest, Z1, Z2, F, F@_1, TrUserData).

decode_msg_decimal_type_extension(Bin, TrUserData) -> dfp_read_field_def_decimal_type_extension(Bin, 0, 0, 0, id(0, TrUserData), id(0, TrUserData), TrUserData).

dfp_read_field_def_decimal_type_extension(<<8, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_decimal_type_extension_precision(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_decimal_type_extension(<<16, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> d_field_decimal_type_extension_scale(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
dfp_read_field_def_decimal_type_extension(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{precision => F@_1, scale => F@_2};
dfp_read_field_def_decimal_type_extension(Other, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dg_read_field_def_decimal_type_extension(Other, Z1, Z2, F, F@_1, F@_2, TrUserData).

dg_read_field_def_decimal_type_extension(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 32 - 7 -> dg_read_field_def_decimal_type_extension(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
dg_read_field_def_decimal_type_extension(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        8 -> d_field_decimal_type_extension_precision(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        16 -> d_field_decimal_type_extension_scale(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        _ ->
            case Key band 7 of
                0 -> skip_varint_decimal_type_extension(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                1 -> skip_64_decimal_type_extension(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                2 -> skip_length_delimited_decimal_type_extension(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                3 -> skip_group_decimal_type_extension(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                5 -> skip_32_decimal_type_extension(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData)
            end
    end;
dg_read_field_def_decimal_type_extension(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{precision => F@_1, scale => F@_2}.

d_field_decimal_type_extension_precision(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_decimal_type_extension_precision(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_decimal_type_extension_precision(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_decimal_type_extension(RestF, 0, 0, F, NewFValue, F@_2, TrUserData).

d_field_decimal_type_extension_scale(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> d_field_decimal_type_extension_scale(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
d_field_decimal_type_extension_scale(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, TrUserData) ->
    {NewFValue, RestF} = {begin <<Res:32/signed-native>> = <<(X bsl N + Acc):32/unsigned-native>>, id(Res, TrUserData) end, Rest},
    dfp_read_field_def_decimal_type_extension(RestF, 0, 0, F, F@_1, NewFValue, TrUserData).

skip_varint_decimal_type_extension(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> skip_varint_decimal_type_extension(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
skip_varint_decimal_type_extension(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_decimal_type_extension(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_length_delimited_decimal_type_extension(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> skip_length_delimited_decimal_type_extension(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
skip_length_delimited_decimal_type_extension(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_decimal_type_extension(Rest2, 0, 0, F, F@_1, F@_2, TrUserData).

skip_group_decimal_type_extension(Bin, _, Z2, FNum, F@_1, F@_2, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_decimal_type_extension(Rest, 0, Z2, FNum, F@_1, F@_2, TrUserData).

skip_32_decimal_type_extension(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_decimal_type_extension(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

skip_64_decimal_type_extension(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> dfp_read_field_def_decimal_type_extension(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

'decode_msg_map<string,string>'(Bin, TrUserData) -> 'dfp_read_field_def_map<string,string>'(Bin, 0, 0, 0, id(<<>>, TrUserData), id(<<>>, TrUserData), TrUserData).

'dfp_read_field_def_map<string,string>'(<<10, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'd_field_map<string,string>_key'(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
'dfp_read_field_def_map<string,string>'(<<18, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'd_field_map<string,string>_value'(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
'dfp_read_field_def_map<string,string>'(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{key => F@_1, value => F@_2};
'dfp_read_field_def_map<string,string>'(Other, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'dg_read_field_def_map<string,string>'(Other, Z1, Z2, F, F@_1, F@_2, TrUserData).

'dg_read_field_def_map<string,string>'(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 32 - 7 -> 'dg_read_field_def_map<string,string>'(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
'dg_read_field_def_map<string,string>'(<<0:1, X:7, Rest/binary>>, N, Acc, _, F@_1, F@_2, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
        10 -> 'd_field_map<string,string>_key'(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        18 -> 'd_field_map<string,string>_value'(Rest, 0, 0, 0, F@_1, F@_2, TrUserData);
        _ ->
            case Key band 7 of
                0 -> 'skip_varint_map<string,string>'(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                1 -> 'skip_64_map<string,string>'(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                2 -> 'skip_length_delimited_map<string,string>'(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                3 -> 'skip_group_map<string,string>'(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData);
                5 -> 'skip_32_map<string,string>'(Rest, 0, 0, Key bsr 3, F@_1, F@_2, TrUserData)
            end
    end;
'dg_read_field_def_map<string,string>'(<<>>, 0, 0, _, F@_1, F@_2, _) -> #{key => F@_1, value => F@_2}.

'd_field_map<string,string>_key'(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> 'd_field_map<string,string>_key'(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
'd_field_map<string,string>_key'(<<0:1, X:7, Rest/binary>>, N, Acc, F, _, F@_2, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    'dfp_read_field_def_map<string,string>'(RestF, 0, 0, F, NewFValue, F@_2, TrUserData).

'd_field_map<string,string>_value'(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> 'd_field_map<string,string>_value'(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
'd_field_map<string,string>_value'(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, _, TrUserData) ->
    {NewFValue, RestF} = begin Len = X bsl N + Acc, <<Bytes:Len/binary, Rest2/binary>> = Rest, Bytes2 = binary:copy(Bytes), {id(Bytes2, TrUserData), Rest2} end,
    'dfp_read_field_def_map<string,string>'(RestF, 0, 0, F, F@_1, NewFValue, TrUserData).

'skip_varint_map<string,string>'(<<1:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'skip_varint_map<string,string>'(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData);
'skip_varint_map<string,string>'(<<0:1, _:7, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'dfp_read_field_def_map<string,string>'(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

'skip_length_delimited_map<string,string>'(<<1:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) when N < 57 -> 'skip_length_delimited_map<string,string>'(Rest, N + 7, X bsl N + Acc, F, F@_1, F@_2, TrUserData);
'skip_length_delimited_map<string,string>'(<<0:1, X:7, Rest/binary>>, N, Acc, F, F@_1, F@_2, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    'dfp_read_field_def_map<string,string>'(Rest2, 0, 0, F, F@_1, F@_2, TrUserData).

'skip_group_map<string,string>'(Bin, _, Z2, FNum, F@_1, F@_2, TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    'dfp_read_field_def_map<string,string>'(Rest, 0, Z2, FNum, F@_1, F@_2, TrUserData).

'skip_32_map<string,string>'(<<_:32, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'dfp_read_field_def_map<string,string>'(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

'skip_64_map<string,string>'(<<_:64, Rest/binary>>, Z1, Z2, F, F@_1, F@_2, TrUserData) -> 'dfp_read_field_def_map<string,string>'(Rest, Z1, Z2, F, F@_1, F@_2, TrUserData).

'd_enum_greptime.v1.SemanticType'(0) -> 'TAG';
'd_enum_greptime.v1.SemanticType'(1) -> 'FIELD';
'd_enum_greptime.v1.SemanticType'(2) -> 'TIMESTAMP';
'd_enum_greptime.v1.SemanticType'(V) -> V.

'd_enum_greptime.v1.ColumnDataType'(0) -> 'BOOLEAN';
'd_enum_greptime.v1.ColumnDataType'(1) -> 'INT8';
'd_enum_greptime.v1.ColumnDataType'(2) -> 'INT16';
'd_enum_greptime.v1.ColumnDataType'(3) -> 'INT32';
'd_enum_greptime.v1.ColumnDataType'(4) -> 'INT64';
'd_enum_greptime.v1.ColumnDataType'(5) -> 'UINT8';
'd_enum_greptime.v1.ColumnDataType'(6) -> 'UINT16';
'd_enum_greptime.v1.ColumnDataType'(7) -> 'UINT32';
'd_enum_greptime.v1.ColumnDataType'(8) -> 'UINT64';
'd_enum_greptime.v1.ColumnDataType'(9) -> 'FLOAT32';
'd_enum_greptime.v1.ColumnDataType'(10) -> 'FLOAT64';
'd_enum_greptime.v1.ColumnDataType'(11) -> 'BINARY';
'd_enum_greptime.v1.ColumnDataType'(12) -> 'STRING';
'd_enum_greptime.v1.ColumnDataType'(13) -> 'DATE';
'd_enum_greptime.v1.ColumnDataType'(14) -> 'DATETIME';
'd_enum_greptime.v1.ColumnDataType'(15) -> 'TIMESTAMP_SECOND';
'd_enum_greptime.v1.ColumnDataType'(16) -> 'TIMESTAMP_MILLISECOND';
'd_enum_greptime.v1.ColumnDataType'(17) -> 'TIMESTAMP_MICROSECOND';
'd_enum_greptime.v1.ColumnDataType'(18) -> 'TIMESTAMP_NANOSECOND';
'd_enum_greptime.v1.ColumnDataType'(19) -> 'TIME_SECOND';
'd_enum_greptime.v1.ColumnDataType'(20) -> 'TIME_MILLISECOND';
'd_enum_greptime.v1.ColumnDataType'(21) -> 'TIME_MICROSECOND';
'd_enum_greptime.v1.ColumnDataType'(22) -> 'TIME_NANOSECOND';
'd_enum_greptime.v1.ColumnDataType'(23) -> 'INTERVAL_YEAR_MONTH';
'd_enum_greptime.v1.ColumnDataType'(24) -> 'INTERVAL_DAY_TIME';
'd_enum_greptime.v1.ColumnDataType'(25) -> 'INTERVAL_MONTH_DAY_NANO';
'd_enum_greptime.v1.ColumnDataType'(26) -> 'DURATION_SECOND';
'd_enum_greptime.v1.ColumnDataType'(27) -> 'DURATION_MILLISECOND';
'd_enum_greptime.v1.ColumnDataType'(28) -> 'DURATION_MICROSECOND';
'd_enum_greptime.v1.ColumnDataType'(29) -> 'DURATION_NANOSECOND';
'd_enum_greptime.v1.ColumnDataType'(30) -> 'DECIMAL128';
'd_enum_greptime.v1.ColumnDataType'(V) -> V.

read_group(Bin, FieldNum) ->
    {NumBytes, EndTagLen} = read_gr_b(Bin, 0, 0, 0, 0, FieldNum),
    <<Group:NumBytes/binary, _:EndTagLen/binary, Rest/binary>> = Bin,
    {Group, Rest}.

%% Like skipping over fields, but record the total length,
%% Each field is <(FieldNum bsl 3) bor FieldType> ++ <FieldValue>
%% Record the length because varints may be non-optimally encoded.
%%
%% Groups can be nested, but assume the same FieldNum cannot be nested
%% because group field numbers are shared with the rest of the fields
%% numbers. Thus we can search just for an group-end with the same
%% field number.
%%
%% (The only time the same group field number could occur would
%% be in a nested sub message, but then it would be inside a
%% length-delimited entry, which we skip-read by length.)
read_gr_b(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum)
  when N < (32-7) ->
    read_gr_b(Tl, N+7, X bsl N + Acc, NumBytes, TagLen+1, FieldNum);
read_gr_b(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen,
          FieldNum) ->
    Key = X bsl N + Acc,
    TagLen1 = TagLen + 1,
    case {Key bsr 3, Key band 7} of
        {FieldNum, 4} -> % 4 = group_end
            {NumBytes, TagLen1};
        {_, 0} -> % 0 = varint
            read_gr_vi(Tl, 0, NumBytes + TagLen1, FieldNum);
        {_, 1} -> % 1 = bits64
            <<_:64, Tl2/binary>> = Tl,
            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 8, 0, FieldNum);
        {_, 2} -> % 2 = length_delimited
            read_gr_ld(Tl, 0, 0, NumBytes + TagLen1, FieldNum);
        {_, 3} -> % 3 = group_start
            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);
        {_, 4} -> % 4 = group_end
            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);
        {_, 5} -> % 5 = bits32
            <<_:32, Tl2/binary>> = Tl,
            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 4, 0, FieldNum)
    end.

read_gr_vi(<<1:1, _:7, Tl/binary>>, N, NumBytes, FieldNum)
  when N < (64-7) ->
    read_gr_vi(Tl, N+7, NumBytes+1, FieldNum);
read_gr_vi(<<0:1, _:7, Tl/binary>>, _, NumBytes, FieldNum) ->
    read_gr_b(Tl, 0, 0, NumBytes+1, 0, FieldNum).

read_gr_ld(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum)
  when N < (64-7) ->
    read_gr_ld(Tl, N+7, X bsl N + Acc, NumBytes+1, FieldNum);
read_gr_ld(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum) ->
    Len = X bsl N + Acc,
    NumBytes1 = NumBytes + 1,
    <<_:Len/binary, Tl2/binary>> = Tl,
    read_gr_b(Tl2, 0, 0, NumBytes1 + Len, 0, FieldNum).

merge_msgs(Prev, New, MsgName) when is_atom(MsgName) -> merge_msgs(Prev, New, MsgName, []).

merge_msgs(Prev, New, MsgName, Opts) ->
    TrUserData = proplists:get_value(user_data, Opts),
    case MsgName of
        values -> merge_msg_values(Prev, New, TrUserData);
        column -> merge_msg_column(Prev, New, TrUserData);
        request_header -> merge_msg_request_header(Prev, New, TrUserData);
        response_header -> merge_msg_response_header(Prev, New, TrUserData);
        status -> merge_msg_status(Prev, New, TrUserData);
        auth_header -> merge_msg_auth_header(Prev, New, TrUserData);
        basic -> merge_msg_basic(Prev, New, TrUserData);
        token -> merge_msg_token(Prev, New, TrUserData);
        affected_rows -> merge_msg_affected_rows(Prev, New, TrUserData);
        flight_metadata -> merge_msg_flight_metadata(Prev, New, TrUserData);
        interval_month_day_nano -> merge_msg_interval_month_day_nano(Prev, New, TrUserData);
        decimal_128 -> merge_msg_decimal_128(Prev, New, TrUserData);
        column_data_type_extension -> merge_msg_column_data_type_extension(Prev, New, TrUserData);
        decimal_type_extension -> merge_msg_decimal_type_extension(Prev, New, TrUserData)
    end.

-compile({nowarn_unused_function,merge_msg_values/3}).
merge_msg_values(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {#{i8_values := PFi8_values}, #{i8_values := NFi8_values}} -> S1#{i8_values => 'erlang_++'(PFi8_values, NFi8_values, TrUserData)};
             {_, #{i8_values := NFi8_values}} -> S1#{i8_values => NFi8_values};
             {#{i8_values := PFi8_values}, _} -> S1#{i8_values => PFi8_values};
             {_, _} -> S1
         end,
    S3 = case {PMsg, NMsg} of
             {#{i16_values := PFi16_values}, #{i16_values := NFi16_values}} -> S2#{i16_values => 'erlang_++'(PFi16_values, NFi16_values, TrUserData)};
             {_, #{i16_values := NFi16_values}} -> S2#{i16_values => NFi16_values};
             {#{i16_values := PFi16_values}, _} -> S2#{i16_values => PFi16_values};
             {_, _} -> S2
         end,
    S4 = case {PMsg, NMsg} of
             {#{i32_values := PFi32_values}, #{i32_values := NFi32_values}} -> S3#{i32_values => 'erlang_++'(PFi32_values, NFi32_values, TrUserData)};
             {_, #{i32_values := NFi32_values}} -> S3#{i32_values => NFi32_values};
             {#{i32_values := PFi32_values}, _} -> S3#{i32_values => PFi32_values};
             {_, _} -> S3
         end,
    S5 = case {PMsg, NMsg} of
             {#{i64_values := PFi64_values}, #{i64_values := NFi64_values}} -> S4#{i64_values => 'erlang_++'(PFi64_values, NFi64_values, TrUserData)};
             {_, #{i64_values := NFi64_values}} -> S4#{i64_values => NFi64_values};
             {#{i64_values := PFi64_values}, _} -> S4#{i64_values => PFi64_values};
             {_, _} -> S4
         end,
    S6 = case {PMsg, NMsg} of
             {#{u8_values := PFu8_values}, #{u8_values := NFu8_values}} -> S5#{u8_values => 'erlang_++'(PFu8_values, NFu8_values, TrUserData)};
             {_, #{u8_values := NFu8_values}} -> S5#{u8_values => NFu8_values};
             {#{u8_values := PFu8_values}, _} -> S5#{u8_values => PFu8_values};
             {_, _} -> S5
         end,
    S7 = case {PMsg, NMsg} of
             {#{u16_values := PFu16_values}, #{u16_values := NFu16_values}} -> S6#{u16_values => 'erlang_++'(PFu16_values, NFu16_values, TrUserData)};
             {_, #{u16_values := NFu16_values}} -> S6#{u16_values => NFu16_values};
             {#{u16_values := PFu16_values}, _} -> S6#{u16_values => PFu16_values};
             {_, _} -> S6
         end,
    S8 = case {PMsg, NMsg} of
             {#{u32_values := PFu32_values}, #{u32_values := NFu32_values}} -> S7#{u32_values => 'erlang_++'(PFu32_values, NFu32_values, TrUserData)};
             {_, #{u32_values := NFu32_values}} -> S7#{u32_values => NFu32_values};
             {#{u32_values := PFu32_values}, _} -> S7#{u32_values => PFu32_values};
             {_, _} -> S7
         end,
    S9 = case {PMsg, NMsg} of
             {#{u64_values := PFu64_values}, #{u64_values := NFu64_values}} -> S8#{u64_values => 'erlang_++'(PFu64_values, NFu64_values, TrUserData)};
             {_, #{u64_values := NFu64_values}} -> S8#{u64_values => NFu64_values};
             {#{u64_values := PFu64_values}, _} -> S8#{u64_values => PFu64_values};
             {_, _} -> S8
         end,
    S10 = case {PMsg, NMsg} of
              {#{f32_values := PFf32_values}, #{f32_values := NFf32_values}} -> S9#{f32_values => 'erlang_++'(PFf32_values, NFf32_values, TrUserData)};
              {_, #{f32_values := NFf32_values}} -> S9#{f32_values => NFf32_values};
              {#{f32_values := PFf32_values}, _} -> S9#{f32_values => PFf32_values};
              {_, _} -> S9
          end,
    S11 = case {PMsg, NMsg} of
              {#{f64_values := PFf64_values}, #{f64_values := NFf64_values}} -> S10#{f64_values => 'erlang_++'(PFf64_values, NFf64_values, TrUserData)};
              {_, #{f64_values := NFf64_values}} -> S10#{f64_values => NFf64_values};
              {#{f64_values := PFf64_values}, _} -> S10#{f64_values => PFf64_values};
              {_, _} -> S10
          end,
    S12 = case {PMsg, NMsg} of
              {#{bool_values := PFbool_values}, #{bool_values := NFbool_values}} -> S11#{bool_values => 'erlang_++'(PFbool_values, NFbool_values, TrUserData)};
              {_, #{bool_values := NFbool_values}} -> S11#{bool_values => NFbool_values};
              {#{bool_values := PFbool_values}, _} -> S11#{bool_values => PFbool_values};
              {_, _} -> S11
          end,
    S13 = case {PMsg, NMsg} of
              {#{binary_values := PFbinary_values}, #{binary_values := NFbinary_values}} -> S12#{binary_values => 'erlang_++'(PFbinary_values, NFbinary_values, TrUserData)};
              {_, #{binary_values := NFbinary_values}} -> S12#{binary_values => NFbinary_values};
              {#{binary_values := PFbinary_values}, _} -> S12#{binary_values => PFbinary_values};
              {_, _} -> S12
          end,
    S14 = case {PMsg, NMsg} of
              {#{string_values := PFstring_values}, #{string_values := NFstring_values}} -> S13#{string_values => 'erlang_++'(PFstring_values, NFstring_values, TrUserData)};
              {_, #{string_values := NFstring_values}} -> S13#{string_values => NFstring_values};
              {#{string_values := PFstring_values}, _} -> S13#{string_values => PFstring_values};
              {_, _} -> S13
          end,
    S15 = case {PMsg, NMsg} of
              {#{date_values := PFdate_values}, #{date_values := NFdate_values}} -> S14#{date_values => 'erlang_++'(PFdate_values, NFdate_values, TrUserData)};
              {_, #{date_values := NFdate_values}} -> S14#{date_values => NFdate_values};
              {#{date_values := PFdate_values}, _} -> S14#{date_values => PFdate_values};
              {_, _} -> S14
          end,
    S16 = case {PMsg, NMsg} of
              {#{datetime_values := PFdatetime_values}, #{datetime_values := NFdatetime_values}} -> S15#{datetime_values => 'erlang_++'(PFdatetime_values, NFdatetime_values, TrUserData)};
              {_, #{datetime_values := NFdatetime_values}} -> S15#{datetime_values => NFdatetime_values};
              {#{datetime_values := PFdatetime_values}, _} -> S15#{datetime_values => PFdatetime_values};
              {_, _} -> S15
          end,
    S17 = case {PMsg, NMsg} of
              {#{timestamp_second_values := PFtimestamp_second_values}, #{timestamp_second_values := NFtimestamp_second_values}} -> S16#{timestamp_second_values => 'erlang_++'(PFtimestamp_second_values, NFtimestamp_second_values, TrUserData)};
              {_, #{timestamp_second_values := NFtimestamp_second_values}} -> S16#{timestamp_second_values => NFtimestamp_second_values};
              {#{timestamp_second_values := PFtimestamp_second_values}, _} -> S16#{timestamp_second_values => PFtimestamp_second_values};
              {_, _} -> S16
          end,
    S18 = case {PMsg, NMsg} of
              {#{timestamp_millisecond_values := PFtimestamp_millisecond_values}, #{timestamp_millisecond_values := NFtimestamp_millisecond_values}} ->
                  S17#{timestamp_millisecond_values => 'erlang_++'(PFtimestamp_millisecond_values, NFtimestamp_millisecond_values, TrUserData)};
              {_, #{timestamp_millisecond_values := NFtimestamp_millisecond_values}} -> S17#{timestamp_millisecond_values => NFtimestamp_millisecond_values};
              {#{timestamp_millisecond_values := PFtimestamp_millisecond_values}, _} -> S17#{timestamp_millisecond_values => PFtimestamp_millisecond_values};
              {_, _} -> S17
          end,
    S19 = case {PMsg, NMsg} of
              {#{timestamp_microsecond_values := PFtimestamp_microsecond_values}, #{timestamp_microsecond_values := NFtimestamp_microsecond_values}} ->
                  S18#{timestamp_microsecond_values => 'erlang_++'(PFtimestamp_microsecond_values, NFtimestamp_microsecond_values, TrUserData)};
              {_, #{timestamp_microsecond_values := NFtimestamp_microsecond_values}} -> S18#{timestamp_microsecond_values => NFtimestamp_microsecond_values};
              {#{timestamp_microsecond_values := PFtimestamp_microsecond_values}, _} -> S18#{timestamp_microsecond_values => PFtimestamp_microsecond_values};
              {_, _} -> S18
          end,
    S20 = case {PMsg, NMsg} of
              {#{timestamp_nanosecond_values := PFtimestamp_nanosecond_values}, #{timestamp_nanosecond_values := NFtimestamp_nanosecond_values}} ->
                  S19#{timestamp_nanosecond_values => 'erlang_++'(PFtimestamp_nanosecond_values, NFtimestamp_nanosecond_values, TrUserData)};
              {_, #{timestamp_nanosecond_values := NFtimestamp_nanosecond_values}} -> S19#{timestamp_nanosecond_values => NFtimestamp_nanosecond_values};
              {#{timestamp_nanosecond_values := PFtimestamp_nanosecond_values}, _} -> S19#{timestamp_nanosecond_values => PFtimestamp_nanosecond_values};
              {_, _} -> S19
          end,
    S21 = case {PMsg, NMsg} of
              {#{time_second_values := PFtime_second_values}, #{time_second_values := NFtime_second_values}} -> S20#{time_second_values => 'erlang_++'(PFtime_second_values, NFtime_second_values, TrUserData)};
              {_, #{time_second_values := NFtime_second_values}} -> S20#{time_second_values => NFtime_second_values};
              {#{time_second_values := PFtime_second_values}, _} -> S20#{time_second_values => PFtime_second_values};
              {_, _} -> S20
          end,
    S22 = case {PMsg, NMsg} of
              {#{time_millisecond_values := PFtime_millisecond_values}, #{time_millisecond_values := NFtime_millisecond_values}} -> S21#{time_millisecond_values => 'erlang_++'(PFtime_millisecond_values, NFtime_millisecond_values, TrUserData)};
              {_, #{time_millisecond_values := NFtime_millisecond_values}} -> S21#{time_millisecond_values => NFtime_millisecond_values};
              {#{time_millisecond_values := PFtime_millisecond_values}, _} -> S21#{time_millisecond_values => PFtime_millisecond_values};
              {_, _} -> S21
          end,
    S23 = case {PMsg, NMsg} of
              {#{time_microsecond_values := PFtime_microsecond_values}, #{time_microsecond_values := NFtime_microsecond_values}} -> S22#{time_microsecond_values => 'erlang_++'(PFtime_microsecond_values, NFtime_microsecond_values, TrUserData)};
              {_, #{time_microsecond_values := NFtime_microsecond_values}} -> S22#{time_microsecond_values => NFtime_microsecond_values};
              {#{time_microsecond_values := PFtime_microsecond_values}, _} -> S22#{time_microsecond_values => PFtime_microsecond_values};
              {_, _} -> S22
          end,
    S24 = case {PMsg, NMsg} of
              {#{time_nanosecond_values := PFtime_nanosecond_values}, #{time_nanosecond_values := NFtime_nanosecond_values}} -> S23#{time_nanosecond_values => 'erlang_++'(PFtime_nanosecond_values, NFtime_nanosecond_values, TrUserData)};
              {_, #{time_nanosecond_values := NFtime_nanosecond_values}} -> S23#{time_nanosecond_values => NFtime_nanosecond_values};
              {#{time_nanosecond_values := PFtime_nanosecond_values}, _} -> S23#{time_nanosecond_values => PFtime_nanosecond_values};
              {_, _} -> S23
          end,
    S25 = case {PMsg, NMsg} of
              {#{interval_year_month_values := PFinterval_year_month_values}, #{interval_year_month_values := NFinterval_year_month_values}} -> S24#{interval_year_month_values => 'erlang_++'(PFinterval_year_month_values, NFinterval_year_month_values, TrUserData)};
              {_, #{interval_year_month_values := NFinterval_year_month_values}} -> S24#{interval_year_month_values => NFinterval_year_month_values};
              {#{interval_year_month_values := PFinterval_year_month_values}, _} -> S24#{interval_year_month_values => PFinterval_year_month_values};
              {_, _} -> S24
          end,
    S26 = case {PMsg, NMsg} of
              {#{interval_day_time_values := PFinterval_day_time_values}, #{interval_day_time_values := NFinterval_day_time_values}} -> S25#{interval_day_time_values => 'erlang_++'(PFinterval_day_time_values, NFinterval_day_time_values, TrUserData)};
              {_, #{interval_day_time_values := NFinterval_day_time_values}} -> S25#{interval_day_time_values => NFinterval_day_time_values};
              {#{interval_day_time_values := PFinterval_day_time_values}, _} -> S25#{interval_day_time_values => PFinterval_day_time_values};
              {_, _} -> S25
          end,
    S27 = case {PMsg, NMsg} of
              {#{interval_month_day_nano_values := PFinterval_month_day_nano_values}, #{interval_month_day_nano_values := NFinterval_month_day_nano_values}} ->
                  S26#{interval_month_day_nano_values => 'erlang_++'(PFinterval_month_day_nano_values, NFinterval_month_day_nano_values, TrUserData)};
              {_, #{interval_month_day_nano_values := NFinterval_month_day_nano_values}} -> S26#{interval_month_day_nano_values => NFinterval_month_day_nano_values};
              {#{interval_month_day_nano_values := PFinterval_month_day_nano_values}, _} -> S26#{interval_month_day_nano_values => PFinterval_month_day_nano_values};
              {_, _} -> S26
          end,
    S28 = case {PMsg, NMsg} of
              {#{duration_second_values := PFduration_second_values}, #{duration_second_values := NFduration_second_values}} -> S27#{duration_second_values => 'erlang_++'(PFduration_second_values, NFduration_second_values, TrUserData)};
              {_, #{duration_second_values := NFduration_second_values}} -> S27#{duration_second_values => NFduration_second_values};
              {#{duration_second_values := PFduration_second_values}, _} -> S27#{duration_second_values => PFduration_second_values};
              {_, _} -> S27
          end,
    S29 = case {PMsg, NMsg} of
              {#{duration_millisecond_values := PFduration_millisecond_values}, #{duration_millisecond_values := NFduration_millisecond_values}} ->
                  S28#{duration_millisecond_values => 'erlang_++'(PFduration_millisecond_values, NFduration_millisecond_values, TrUserData)};
              {_, #{duration_millisecond_values := NFduration_millisecond_values}} -> S28#{duration_millisecond_values => NFduration_millisecond_values};
              {#{duration_millisecond_values := PFduration_millisecond_values}, _} -> S28#{duration_millisecond_values => PFduration_millisecond_values};
              {_, _} -> S28
          end,
    S30 = case {PMsg, NMsg} of
              {#{duration_microsecond_values := PFduration_microsecond_values}, #{duration_microsecond_values := NFduration_microsecond_values}} ->
                  S29#{duration_microsecond_values => 'erlang_++'(PFduration_microsecond_values, NFduration_microsecond_values, TrUserData)};
              {_, #{duration_microsecond_values := NFduration_microsecond_values}} -> S29#{duration_microsecond_values => NFduration_microsecond_values};
              {#{duration_microsecond_values := PFduration_microsecond_values}, _} -> S29#{duration_microsecond_values => PFduration_microsecond_values};
              {_, _} -> S29
          end,
    S31 = case {PMsg, NMsg} of
              {#{duration_nanosecond_values := PFduration_nanosecond_values}, #{duration_nanosecond_values := NFduration_nanosecond_values}} -> S30#{duration_nanosecond_values => 'erlang_++'(PFduration_nanosecond_values, NFduration_nanosecond_values, TrUserData)};
              {_, #{duration_nanosecond_values := NFduration_nanosecond_values}} -> S30#{duration_nanosecond_values => NFduration_nanosecond_values};
              {#{duration_nanosecond_values := PFduration_nanosecond_values}, _} -> S30#{duration_nanosecond_values => PFduration_nanosecond_values};
              {_, _} -> S30
          end,
    case {PMsg, NMsg} of
        {#{decimal128_values := PFdecimal128_values}, #{decimal128_values := NFdecimal128_values}} -> S31#{decimal128_values => 'erlang_++'(PFdecimal128_values, NFdecimal128_values, TrUserData)};
        {_, #{decimal128_values := NFdecimal128_values}} -> S31#{decimal128_values => NFdecimal128_values};
        {#{decimal128_values := PFdecimal128_values}, _} -> S31#{decimal128_values => PFdecimal128_values};
        {_, _} -> S31
    end.

-compile({nowarn_unused_function,merge_msg_column/3}).
merge_msg_column(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{column_name := NFcolumn_name}} -> S1#{column_name => NFcolumn_name};
             {#{column_name := PFcolumn_name}, _} -> S1#{column_name => PFcolumn_name};
             _ -> S1
         end,
    S3 = case {PMsg, NMsg} of
             {_, #{semantic_type := NFsemantic_type}} -> S2#{semantic_type => NFsemantic_type};
             {#{semantic_type := PFsemantic_type}, _} -> S2#{semantic_type => PFsemantic_type};
             _ -> S2
         end,
    S4 = case {PMsg, NMsg} of
             {#{values := PFvalues}, #{values := NFvalues}} -> S3#{values => merge_msg_values(PFvalues, NFvalues, TrUserData)};
             {_, #{values := NFvalues}} -> S3#{values => NFvalues};
             {#{values := PFvalues}, _} -> S3#{values => PFvalues};
             {_, _} -> S3
         end,
    S5 = case {PMsg, NMsg} of
             {_, #{null_mask := NFnull_mask}} -> S4#{null_mask => NFnull_mask};
             {#{null_mask := PFnull_mask}, _} -> S4#{null_mask => PFnull_mask};
             _ -> S4
         end,
    S6 = case {PMsg, NMsg} of
             {_, #{datatype := NFdatatype}} -> S5#{datatype => NFdatatype};
             {#{datatype := PFdatatype}, _} -> S5#{datatype => PFdatatype};
             _ -> S5
         end,
    case {PMsg, NMsg} of
        {#{datatype_extension := PFdatatype_extension}, #{datatype_extension := NFdatatype_extension}} -> S6#{datatype_extension => merge_msg_column_data_type_extension(PFdatatype_extension, NFdatatype_extension, TrUserData)};
        {_, #{datatype_extension := NFdatatype_extension}} -> S6#{datatype_extension => NFdatatype_extension};
        {#{datatype_extension := PFdatatype_extension}, _} -> S6#{datatype_extension => PFdatatype_extension};
        {_, _} -> S6
    end.

-compile({nowarn_unused_function,merge_msg_request_header/3}).
merge_msg_request_header(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{catalog := NFcatalog}} -> S1#{catalog => NFcatalog};
             {#{catalog := PFcatalog}, _} -> S1#{catalog => PFcatalog};
             _ -> S1
         end,
    S3 = case {PMsg, NMsg} of
             {_, #{schema := NFschema}} -> S2#{schema => NFschema};
             {#{schema := PFschema}, _} -> S2#{schema => PFschema};
             _ -> S2
         end,
    S4 = case {PMsg, NMsg} of
             {#{authorization := PFauthorization}, #{authorization := NFauthorization}} -> S3#{authorization => merge_msg_auth_header(PFauthorization, NFauthorization, TrUserData)};
             {_, #{authorization := NFauthorization}} -> S3#{authorization => NFauthorization};
             {#{authorization := PFauthorization}, _} -> S3#{authorization => PFauthorization};
             {_, _} -> S3
         end,
    S5 = case {PMsg, NMsg} of
             {_, #{dbname := NFdbname}} -> S4#{dbname => NFdbname};
             {#{dbname := PFdbname}, _} -> S4#{dbname => PFdbname};
             _ -> S4
         end,
    case {PMsg, NMsg} of
        {#{tracing_context := PFtracing_context}, #{tracing_context := NFtracing_context}} -> S5#{tracing_context => 'tr_merge_request_header.tracing_context'(PFtracing_context, NFtracing_context, TrUserData)};
        {_, #{tracing_context := NFtracing_context}} -> S5#{tracing_context => NFtracing_context};
        {#{tracing_context := PFtracing_context}, _} -> S5#{tracing_context => PFtracing_context};
        {_, _} -> S5
    end.

-compile({nowarn_unused_function,merge_msg_response_header/3}).
merge_msg_response_header(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    case {PMsg, NMsg} of
        {#{status := PFstatus}, #{status := NFstatus}} -> S1#{status => merge_msg_status(PFstatus, NFstatus, TrUserData)};
        {_, #{status := NFstatus}} -> S1#{status => NFstatus};
        {#{status := PFstatus}, _} -> S1#{status => PFstatus};
        {_, _} -> S1
    end.

-compile({nowarn_unused_function,merge_msg_status/3}).
merge_msg_status(PMsg, NMsg, _) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{status_code := NFstatus_code}} -> S1#{status_code => NFstatus_code};
             {#{status_code := PFstatus_code}, _} -> S1#{status_code => PFstatus_code};
             _ -> S1
         end,
    case {PMsg, NMsg} of
        {_, #{err_msg := NFerr_msg}} -> S2#{err_msg => NFerr_msg};
        {#{err_msg := PFerr_msg}, _} -> S2#{err_msg => PFerr_msg};
        _ -> S2
    end.

-compile({nowarn_unused_function,merge_msg_auth_header/3}).
merge_msg_auth_header(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    case {PMsg, NMsg} of
        {#{auth_scheme := {basic, OPFauth_scheme}}, #{auth_scheme := {basic, ONFauth_scheme}}} -> S1#{auth_scheme => {basic, merge_msg_basic(OPFauth_scheme, ONFauth_scheme, TrUserData)}};
        {#{auth_scheme := {token, OPFauth_scheme}}, #{auth_scheme := {token, ONFauth_scheme}}} -> S1#{auth_scheme => {token, merge_msg_token(OPFauth_scheme, ONFauth_scheme, TrUserData)}};
        {_, #{auth_scheme := NFauth_scheme}} -> S1#{auth_scheme => NFauth_scheme};
        {#{auth_scheme := PFauth_scheme}, _} -> S1#{auth_scheme => PFauth_scheme};
        {_, _} -> S1
    end.

-compile({nowarn_unused_function,merge_msg_basic/3}).
merge_msg_basic(PMsg, NMsg, _) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{username := NFusername}} -> S1#{username => NFusername};
             {#{username := PFusername}, _} -> S1#{username => PFusername};
             _ -> S1
         end,
    case {PMsg, NMsg} of
        {_, #{password := NFpassword}} -> S2#{password => NFpassword};
        {#{password := PFpassword}, _} -> S2#{password => PFpassword};
        _ -> S2
    end.

-compile({nowarn_unused_function,merge_msg_token/3}).
merge_msg_token(PMsg, NMsg, _) ->
    S1 = #{},
    case {PMsg, NMsg} of
        {_, #{token := NFtoken}} -> S1#{token => NFtoken};
        {#{token := PFtoken}, _} -> S1#{token => PFtoken};
        _ -> S1
    end.

-compile({nowarn_unused_function,merge_msg_affected_rows/3}).
merge_msg_affected_rows(PMsg, NMsg, _) ->
    S1 = #{},
    case {PMsg, NMsg} of
        {_, #{value := NFvalue}} -> S1#{value => NFvalue};
        {#{value := PFvalue}, _} -> S1#{value => PFvalue};
        _ -> S1
    end.

-compile({nowarn_unused_function,merge_msg_flight_metadata/3}).
merge_msg_flight_metadata(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    case {PMsg, NMsg} of
        {#{affected_rows := PFaffected_rows}, #{affected_rows := NFaffected_rows}} -> S1#{affected_rows => merge_msg_affected_rows(PFaffected_rows, NFaffected_rows, TrUserData)};
        {_, #{affected_rows := NFaffected_rows}} -> S1#{affected_rows => NFaffected_rows};
        {#{affected_rows := PFaffected_rows}, _} -> S1#{affected_rows => PFaffected_rows};
        {_, _} -> S1
    end.

-compile({nowarn_unused_function,merge_msg_interval_month_day_nano/3}).
merge_msg_interval_month_day_nano(PMsg, NMsg, _) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{months := NFmonths}} -> S1#{months => NFmonths};
             {#{months := PFmonths}, _} -> S1#{months => PFmonths};
             _ -> S1
         end,
    S3 = case {PMsg, NMsg} of
             {_, #{days := NFdays}} -> S2#{days => NFdays};
             {#{days := PFdays}, _} -> S2#{days => PFdays};
             _ -> S2
         end,
    case {PMsg, NMsg} of
        {_, #{nanoseconds := NFnanoseconds}} -> S3#{nanoseconds => NFnanoseconds};
        {#{nanoseconds := PFnanoseconds}, _} -> S3#{nanoseconds => PFnanoseconds};
        _ -> S3
    end.

-compile({nowarn_unused_function,merge_msg_decimal_128/3}).
merge_msg_decimal_128(PMsg, NMsg, _) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{hi := NFhi}} -> S1#{hi => NFhi};
             {#{hi := PFhi}, _} -> S1#{hi => PFhi};
             _ -> S1
         end,
    case {PMsg, NMsg} of
        {_, #{lo := NFlo}} -> S2#{lo => NFlo};
        {#{lo := PFlo}, _} -> S2#{lo => PFlo};
        _ -> S2
    end.

-compile({nowarn_unused_function,merge_msg_column_data_type_extension/3}).
merge_msg_column_data_type_extension(PMsg, NMsg, TrUserData) ->
    S1 = #{},
    case {PMsg, NMsg} of
        {#{type_ext := {decimal_type, OPFtype_ext}}, #{type_ext := {decimal_type, ONFtype_ext}}} -> S1#{type_ext => {decimal_type, merge_msg_decimal_type_extension(OPFtype_ext, ONFtype_ext, TrUserData)}};
        {_, #{type_ext := NFtype_ext}} -> S1#{type_ext => NFtype_ext};
        {#{type_ext := PFtype_ext}, _} -> S1#{type_ext => PFtype_ext};
        {_, _} -> S1
    end.

-compile({nowarn_unused_function,merge_msg_decimal_type_extension/3}).
merge_msg_decimal_type_extension(PMsg, NMsg, _) ->
    S1 = #{},
    S2 = case {PMsg, NMsg} of
             {_, #{precision := NFprecision}} -> S1#{precision => NFprecision};
             {#{precision := PFprecision}, _} -> S1#{precision => PFprecision};
             _ -> S1
         end,
    case {PMsg, NMsg} of
        {_, #{scale := NFscale}} -> S2#{scale => NFscale};
        {#{scale := PFscale}, _} -> S2#{scale => PFscale};
        _ -> S2
    end.


verify_msg(Msg, MsgName) when is_atom(MsgName) -> verify_msg(Msg, MsgName, []).

verify_msg(Msg, MsgName, Opts) ->
    TrUserData = proplists:get_value(user_data, Opts),
    case MsgName of
        values -> v_msg_values(Msg, [MsgName], TrUserData);
        column -> v_msg_column(Msg, [MsgName], TrUserData);
        request_header -> v_msg_request_header(Msg, [MsgName], TrUserData);
        response_header -> v_msg_response_header(Msg, [MsgName], TrUserData);
        status -> v_msg_status(Msg, [MsgName], TrUserData);
        auth_header -> v_msg_auth_header(Msg, [MsgName], TrUserData);
        basic -> v_msg_basic(Msg, [MsgName], TrUserData);
        token -> v_msg_token(Msg, [MsgName], TrUserData);
        affected_rows -> v_msg_affected_rows(Msg, [MsgName], TrUserData);
        flight_metadata -> v_msg_flight_metadata(Msg, [MsgName], TrUserData);
        interval_month_day_nano -> v_msg_interval_month_day_nano(Msg, [MsgName], TrUserData);
        decimal_128 -> v_msg_decimal_128(Msg, [MsgName], TrUserData);
        column_data_type_extension -> v_msg_column_data_type_extension(Msg, [MsgName], TrUserData);
        decimal_type_extension -> v_msg_decimal_type_extension(Msg, [MsgName], TrUserData);
        _ -> mk_type_error(not_a_known_message, Msg, [])
    end.


-compile({nowarn_unused_function,v_submsg_values/3}).
-dialyzer({nowarn_function,v_submsg_values/3}).
v_submsg_values(Msg, Path, TrUserData) -> v_msg_values(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_values/3}).
-dialyzer({nowarn_function,v_msg_values/3}).
v_msg_values(#{} = M, Path, TrUserData) ->
    case M of
        #{i8_values := F1} ->
            if is_list(F1) ->
                   _ = [v_type_int32(Elem, [i8_values | Path], TrUserData) || Elem <- F1],
                   ok;
               true -> mk_type_error({invalid_list_of, int32}, F1, [i8_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{i16_values := F2} ->
            if is_list(F2) ->
                   _ = [v_type_int32(Elem, [i16_values | Path], TrUserData) || Elem <- F2],
                   ok;
               true -> mk_type_error({invalid_list_of, int32}, F2, [i16_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{i32_values := F3} ->
            if is_list(F3) ->
                   _ = [v_type_int32(Elem, [i32_values | Path], TrUserData) || Elem <- F3],
                   ok;
               true -> mk_type_error({invalid_list_of, int32}, F3, [i32_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{i64_values := F4} ->
            if is_list(F4) ->
                   _ = [v_type_int64(Elem, [i64_values | Path], TrUserData) || Elem <- F4],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F4, [i64_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{u8_values := F5} ->
            if is_list(F5) ->
                   _ = [v_type_uint32(Elem, [u8_values | Path], TrUserData) || Elem <- F5],
                   ok;
               true -> mk_type_error({invalid_list_of, uint32}, F5, [u8_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{u16_values := F6} ->
            if is_list(F6) ->
                   _ = [v_type_uint32(Elem, [u16_values | Path], TrUserData) || Elem <- F6],
                   ok;
               true -> mk_type_error({invalid_list_of, uint32}, F6, [u16_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{u32_values := F7} ->
            if is_list(F7) ->
                   _ = [v_type_uint32(Elem, [u32_values | Path], TrUserData) || Elem <- F7],
                   ok;
               true -> mk_type_error({invalid_list_of, uint32}, F7, [u32_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{u64_values := F8} ->
            if is_list(F8) ->
                   _ = [v_type_uint64(Elem, [u64_values | Path], TrUserData) || Elem <- F8],
                   ok;
               true -> mk_type_error({invalid_list_of, uint64}, F8, [u64_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{f32_values := F9} ->
            if is_list(F9) ->
                   _ = [v_type_float(Elem, [f32_values | Path], TrUserData) || Elem <- F9],
                   ok;
               true -> mk_type_error({invalid_list_of, float}, F9, [f32_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{f64_values := F10} ->
            if is_list(F10) ->
                   _ = [v_type_double(Elem, [f64_values | Path], TrUserData) || Elem <- F10],
                   ok;
               true -> mk_type_error({invalid_list_of, double}, F10, [f64_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{bool_values := F11} ->
            if is_list(F11) ->
                   _ = [v_type_bool(Elem, [bool_values | Path], TrUserData) || Elem <- F11],
                   ok;
               true -> mk_type_error({invalid_list_of, bool}, F11, [bool_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{binary_values := F12} ->
            if is_list(F12) ->
                   _ = [v_type_bytes(Elem, [binary_values | Path], TrUserData) || Elem <- F12],
                   ok;
               true -> mk_type_error({invalid_list_of, bytes}, F12, [binary_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{string_values := F13} ->
            if is_list(F13) ->
                   _ = [v_type_string(Elem, [string_values | Path], TrUserData) || Elem <- F13],
                   ok;
               true -> mk_type_error({invalid_list_of, string}, F13, [string_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{date_values := F14} ->
            if is_list(F14) ->
                   _ = [v_type_int32(Elem, [date_values | Path], TrUserData) || Elem <- F14],
                   ok;
               true -> mk_type_error({invalid_list_of, int32}, F14, [date_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{datetime_values := F15} ->
            if is_list(F15) ->
                   _ = [v_type_int64(Elem, [datetime_values | Path], TrUserData) || Elem <- F15],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F15, [datetime_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{timestamp_second_values := F16} ->
            if is_list(F16) ->
                   _ = [v_type_int64(Elem, [timestamp_second_values | Path], TrUserData) || Elem <- F16],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F16, [timestamp_second_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{timestamp_millisecond_values := F17} ->
            if is_list(F17) ->
                   _ = [v_type_int64(Elem, [timestamp_millisecond_values | Path], TrUserData) || Elem <- F17],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F17, [timestamp_millisecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{timestamp_microsecond_values := F18} ->
            if is_list(F18) ->
                   _ = [v_type_int64(Elem, [timestamp_microsecond_values | Path], TrUserData) || Elem <- F18],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F18, [timestamp_microsecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{timestamp_nanosecond_values := F19} ->
            if is_list(F19) ->
                   _ = [v_type_int64(Elem, [timestamp_nanosecond_values | Path], TrUserData) || Elem <- F19],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F19, [timestamp_nanosecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{time_second_values := F20} ->
            if is_list(F20) ->
                   _ = [v_type_int64(Elem, [time_second_values | Path], TrUserData) || Elem <- F20],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F20, [time_second_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{time_millisecond_values := F21} ->
            if is_list(F21) ->
                   _ = [v_type_int64(Elem, [time_millisecond_values | Path], TrUserData) || Elem <- F21],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F21, [time_millisecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{time_microsecond_values := F22} ->
            if is_list(F22) ->
                   _ = [v_type_int64(Elem, [time_microsecond_values | Path], TrUserData) || Elem <- F22],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F22, [time_microsecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{time_nanosecond_values := F23} ->
            if is_list(F23) ->
                   _ = [v_type_int64(Elem, [time_nanosecond_values | Path], TrUserData) || Elem <- F23],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F23, [time_nanosecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{interval_year_month_values := F24} ->
            if is_list(F24) ->
                   _ = [v_type_int32(Elem, [interval_year_month_values | Path], TrUserData) || Elem <- F24],
                   ok;
               true -> mk_type_error({invalid_list_of, int32}, F24, [interval_year_month_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{interval_day_time_values := F25} ->
            if is_list(F25) ->
                   _ = [v_type_int64(Elem, [interval_day_time_values | Path], TrUserData) || Elem <- F25],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F25, [interval_day_time_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{interval_month_day_nano_values := F26} ->
            if is_list(F26) ->
                   _ = [v_submsg_interval_month_day_nano(Elem, [interval_month_day_nano_values | Path], TrUserData) || Elem <- F26],
                   ok;
               true -> mk_type_error({invalid_list_of, {msg, interval_month_day_nano}}, F26, [interval_month_day_nano_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{duration_second_values := F27} ->
            if is_list(F27) ->
                   _ = [v_type_int64(Elem, [duration_second_values | Path], TrUserData) || Elem <- F27],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F27, [duration_second_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{duration_millisecond_values := F28} ->
            if is_list(F28) ->
                   _ = [v_type_int64(Elem, [duration_millisecond_values | Path], TrUserData) || Elem <- F28],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F28, [duration_millisecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{duration_microsecond_values := F29} ->
            if is_list(F29) ->
                   _ = [v_type_int64(Elem, [duration_microsecond_values | Path], TrUserData) || Elem <- F29],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F29, [duration_microsecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{duration_nanosecond_values := F30} ->
            if is_list(F30) ->
                   _ = [v_type_int64(Elem, [duration_nanosecond_values | Path], TrUserData) || Elem <- F30],
                   ok;
               true -> mk_type_error({invalid_list_of, int64}, F30, [duration_nanosecond_values | Path])
            end;
        _ -> ok
    end,
    case M of
        #{decimal128_values := F31} ->
            if is_list(F31) ->
                   _ = [v_submsg_decimal_128(Elem, [decimal128_values | Path], TrUserData) || Elem <- F31],
                   ok;
               true -> mk_type_error({invalid_list_of, {msg, decimal_128}}, F31, [decimal128_values | Path])
            end;
        _ -> ok
    end,
    lists:foreach(fun (i8_values) -> ok;
                      (i16_values) -> ok;
                      (i32_values) -> ok;
                      (i64_values) -> ok;
                      (u8_values) -> ok;
                      (u16_values) -> ok;
                      (u32_values) -> ok;
                      (u64_values) -> ok;
                      (f32_values) -> ok;
                      (f64_values) -> ok;
                      (bool_values) -> ok;
                      (binary_values) -> ok;
                      (string_values) -> ok;
                      (date_values) -> ok;
                      (datetime_values) -> ok;
                      (timestamp_second_values) -> ok;
                      (timestamp_millisecond_values) -> ok;
                      (timestamp_microsecond_values) -> ok;
                      (timestamp_nanosecond_values) -> ok;
                      (time_second_values) -> ok;
                      (time_millisecond_values) -> ok;
                      (time_microsecond_values) -> ok;
                      (time_nanosecond_values) -> ok;
                      (interval_year_month_values) -> ok;
                      (interval_day_time_values) -> ok;
                      (interval_month_day_nano_values) -> ok;
                      (duration_second_values) -> ok;
                      (duration_millisecond_values) -> ok;
                      (duration_microsecond_values) -> ok;
                      (duration_nanosecond_values) -> ok;
                      (decimal128_values) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_values(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), values}, M, Path);
v_msg_values(X, Path, _TrUserData) -> mk_type_error({expected_msg, values}, X, Path).

-compile({nowarn_unused_function,v_msg_column/3}).
-dialyzer({nowarn_function,v_msg_column/3}).
v_msg_column(#{} = M, Path, TrUserData) ->
    case M of
        #{column_name := F1} -> v_type_string(F1, [column_name | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{semantic_type := F2} -> 'v_enum_greptime.v1.SemanticType'(F2, [semantic_type | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{values := F3} -> v_submsg_values(F3, [values | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{null_mask := F4} -> v_type_bytes(F4, [null_mask | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{datatype := F5} -> 'v_enum_greptime.v1.ColumnDataType'(F5, [datatype | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{datatype_extension := F6} -> v_submsg_column_data_type_extension(F6, [datatype_extension | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (column_name) -> ok;
                      (semantic_type) -> ok;
                      (values) -> ok;
                      (null_mask) -> ok;
                      (datatype) -> ok;
                      (datatype_extension) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_column(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), column}, M, Path);
v_msg_column(X, Path, _TrUserData) -> mk_type_error({expected_msg, column}, X, Path).

-compile({nowarn_unused_function,v_msg_request_header/3}).
-dialyzer({nowarn_function,v_msg_request_header/3}).
v_msg_request_header(#{} = M, Path, TrUserData) ->
    case M of
        #{catalog := F1} -> v_type_string(F1, [catalog | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{schema := F2} -> v_type_string(F2, [schema | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{authorization := F3} -> v_submsg_auth_header(F3, [authorization | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{dbname := F4} -> v_type_string(F4, [dbname | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{tracing_context := F5} -> 'v_map<string,string>'(F5, [tracing_context | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (catalog) -> ok;
                      (schema) -> ok;
                      (authorization) -> ok;
                      (dbname) -> ok;
                      (tracing_context) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_request_header(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), request_header}, M, Path);
v_msg_request_header(X, Path, _TrUserData) -> mk_type_error({expected_msg, request_header}, X, Path).

-compile({nowarn_unused_function,v_msg_response_header/3}).
-dialyzer({nowarn_function,v_msg_response_header/3}).
v_msg_response_header(#{} = M, Path, TrUserData) ->
    case M of
        #{status := F1} -> v_submsg_status(F1, [status | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (status) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_response_header(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), response_header}, M, Path);
v_msg_response_header(X, Path, _TrUserData) -> mk_type_error({expected_msg, response_header}, X, Path).

-compile({nowarn_unused_function,v_submsg_status/3}).
-dialyzer({nowarn_function,v_submsg_status/3}).
v_submsg_status(Msg, Path, TrUserData) -> v_msg_status(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_status/3}).
-dialyzer({nowarn_function,v_msg_status/3}).
v_msg_status(#{} = M, Path, TrUserData) ->
    case M of
        #{status_code := F1} -> v_type_uint32(F1, [status_code | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{err_msg := F2} -> v_type_string(F2, [err_msg | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (status_code) -> ok;
                      (err_msg) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_status(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), status}, M, Path);
v_msg_status(X, Path, _TrUserData) -> mk_type_error({expected_msg, status}, X, Path).

-compile({nowarn_unused_function,v_submsg_auth_header/3}).
-dialyzer({nowarn_function,v_submsg_auth_header/3}).
v_submsg_auth_header(Msg, Path, TrUserData) -> v_msg_auth_header(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_auth_header/3}).
-dialyzer({nowarn_function,v_msg_auth_header/3}).
v_msg_auth_header(#{} = M, Path, TrUserData) ->
    case M of
        #{auth_scheme := {basic, OF1}} -> v_submsg_basic(OF1, [basic, auth_scheme | Path], TrUserData);
        #{auth_scheme := {token, OF1}} -> v_submsg_token(OF1, [token, auth_scheme | Path], TrUserData);
        #{auth_scheme := F1} -> mk_type_error(invalid_oneof, F1, [auth_scheme | Path]);
        _ -> ok
    end,
    lists:foreach(fun (auth_scheme) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_auth_header(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), auth_header}, M, Path);
v_msg_auth_header(X, Path, _TrUserData) -> mk_type_error({expected_msg, auth_header}, X, Path).

-compile({nowarn_unused_function,v_submsg_basic/3}).
-dialyzer({nowarn_function,v_submsg_basic/3}).
v_submsg_basic(Msg, Path, TrUserData) -> v_msg_basic(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_basic/3}).
-dialyzer({nowarn_function,v_msg_basic/3}).
v_msg_basic(#{} = M, Path, TrUserData) ->
    case M of
        #{username := F1} -> v_type_string(F1, [username | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{password := F2} -> v_type_string(F2, [password | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (username) -> ok;
                      (password) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_basic(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), basic}, M, Path);
v_msg_basic(X, Path, _TrUserData) -> mk_type_error({expected_msg, basic}, X, Path).

-compile({nowarn_unused_function,v_submsg_token/3}).
-dialyzer({nowarn_function,v_submsg_token/3}).
v_submsg_token(Msg, Path, TrUserData) -> v_msg_token(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_token/3}).
-dialyzer({nowarn_function,v_msg_token/3}).
v_msg_token(#{} = M, Path, TrUserData) ->
    case M of
        #{token := F1} -> v_type_string(F1, [token | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (token) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_token(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), token}, M, Path);
v_msg_token(X, Path, _TrUserData) -> mk_type_error({expected_msg, token}, X, Path).

-compile({nowarn_unused_function,v_submsg_affected_rows/3}).
-dialyzer({nowarn_function,v_submsg_affected_rows/3}).
v_submsg_affected_rows(Msg, Path, TrUserData) -> v_msg_affected_rows(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_affected_rows/3}).
-dialyzer({nowarn_function,v_msg_affected_rows/3}).
v_msg_affected_rows(#{} = M, Path, TrUserData) ->
    case M of
        #{value := F1} -> v_type_uint32(F1, [value | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (value) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_affected_rows(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), affected_rows}, M, Path);
v_msg_affected_rows(X, Path, _TrUserData) -> mk_type_error({expected_msg, affected_rows}, X, Path).

-compile({nowarn_unused_function,v_msg_flight_metadata/3}).
-dialyzer({nowarn_function,v_msg_flight_metadata/3}).
v_msg_flight_metadata(#{} = M, Path, TrUserData) ->
    case M of
        #{affected_rows := F1} -> v_submsg_affected_rows(F1, [affected_rows | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (affected_rows) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_flight_metadata(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), flight_metadata}, M, Path);
v_msg_flight_metadata(X, Path, _TrUserData) -> mk_type_error({expected_msg, flight_metadata}, X, Path).

-compile({nowarn_unused_function,v_submsg_interval_month_day_nano/3}).
-dialyzer({nowarn_function,v_submsg_interval_month_day_nano/3}).
v_submsg_interval_month_day_nano(Msg, Path, TrUserData) -> v_msg_interval_month_day_nano(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_interval_month_day_nano/3}).
-dialyzer({nowarn_function,v_msg_interval_month_day_nano/3}).
v_msg_interval_month_day_nano(#{} = M, Path, TrUserData) ->
    case M of
        #{months := F1} -> v_type_int32(F1, [months | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{days := F2} -> v_type_int32(F2, [days | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{nanoseconds := F3} -> v_type_int64(F3, [nanoseconds | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (months) -> ok;
                      (days) -> ok;
                      (nanoseconds) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_interval_month_day_nano(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), interval_month_day_nano}, M, Path);
v_msg_interval_month_day_nano(X, Path, _TrUserData) -> mk_type_error({expected_msg, interval_month_day_nano}, X, Path).

-compile({nowarn_unused_function,v_submsg_decimal_128/3}).
-dialyzer({nowarn_function,v_submsg_decimal_128/3}).
v_submsg_decimal_128(Msg, Path, TrUserData) -> v_msg_decimal_128(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_decimal_128/3}).
-dialyzer({nowarn_function,v_msg_decimal_128/3}).
v_msg_decimal_128(#{} = M, Path, TrUserData) ->
    case M of
        #{hi := F1} -> v_type_int64(F1, [hi | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{lo := F2} -> v_type_int64(F2, [lo | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (hi) -> ok;
                      (lo) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_decimal_128(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), decimal_128}, M, Path);
v_msg_decimal_128(X, Path, _TrUserData) -> mk_type_error({expected_msg, decimal_128}, X, Path).

-compile({nowarn_unused_function,v_submsg_column_data_type_extension/3}).
-dialyzer({nowarn_function,v_submsg_column_data_type_extension/3}).
v_submsg_column_data_type_extension(Msg, Path, TrUserData) -> v_msg_column_data_type_extension(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_column_data_type_extension/3}).
-dialyzer({nowarn_function,v_msg_column_data_type_extension/3}).
v_msg_column_data_type_extension(#{} = M, Path, TrUserData) ->
    case M of
        #{type_ext := {decimal_type, OF1}} -> v_submsg_decimal_type_extension(OF1, [decimal_type, type_ext | Path], TrUserData);
        #{type_ext := F1} -> mk_type_error(invalid_oneof, F1, [type_ext | Path]);
        _ -> ok
    end,
    lists:foreach(fun (type_ext) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_column_data_type_extension(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), column_data_type_extension}, M, Path);
v_msg_column_data_type_extension(X, Path, _TrUserData) -> mk_type_error({expected_msg, column_data_type_extension}, X, Path).

-compile({nowarn_unused_function,v_submsg_decimal_type_extension/3}).
-dialyzer({nowarn_function,v_submsg_decimal_type_extension/3}).
v_submsg_decimal_type_extension(Msg, Path, TrUserData) -> v_msg_decimal_type_extension(Msg, Path, TrUserData).

-compile({nowarn_unused_function,v_msg_decimal_type_extension/3}).
-dialyzer({nowarn_function,v_msg_decimal_type_extension/3}).
v_msg_decimal_type_extension(#{} = M, Path, TrUserData) ->
    case M of
        #{precision := F1} -> v_type_int32(F1, [precision | Path], TrUserData);
        _ -> ok
    end,
    case M of
        #{scale := F2} -> v_type_int32(F2, [scale | Path], TrUserData);
        _ -> ok
    end,
    lists:foreach(fun (precision) -> ok;
                      (scale) -> ok;
                      (OtherKey) -> mk_type_error({extraneous_key, OtherKey}, M, Path)
                  end,
                  maps:keys(M)),
    ok;
v_msg_decimal_type_extension(M, Path, _TrUserData) when is_map(M) -> mk_type_error({missing_fields, [] -- maps:keys(M), decimal_type_extension}, M, Path);
v_msg_decimal_type_extension(X, Path, _TrUserData) -> mk_type_error({expected_msg, decimal_type_extension}, X, Path).

-compile({nowarn_unused_function,'v_enum_greptime.v1.SemanticType'/3}).
-dialyzer({nowarn_function,'v_enum_greptime.v1.SemanticType'/3}).
'v_enum_greptime.v1.SemanticType'('TAG', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.SemanticType'('FIELD', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.SemanticType'('TIMESTAMP', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.SemanticType'(V, _Path, _TrUserData) when -2147483648 =< V, V =< 2147483647, is_integer(V) -> ok;
'v_enum_greptime.v1.SemanticType'(X, Path, _TrUserData) -> mk_type_error({invalid_enum, 'greptime.v1.SemanticType'}, X, Path).

-compile({nowarn_unused_function,'v_enum_greptime.v1.ColumnDataType'/3}).
-dialyzer({nowarn_function,'v_enum_greptime.v1.ColumnDataType'/3}).
'v_enum_greptime.v1.ColumnDataType'('BOOLEAN', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INT8', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INT16', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INT32', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INT64', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('UINT8', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('UINT16', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('UINT32', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('UINT64', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('FLOAT32', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('FLOAT64', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('BINARY', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('STRING', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DATE', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DATETIME', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIMESTAMP_SECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIMESTAMP_MILLISECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIMESTAMP_MICROSECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIMESTAMP_NANOSECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIME_SECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIME_MILLISECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIME_MICROSECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('TIME_NANOSECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INTERVAL_YEAR_MONTH', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INTERVAL_DAY_TIME', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('INTERVAL_MONTH_DAY_NANO', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DURATION_SECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DURATION_MILLISECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DURATION_MICROSECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DURATION_NANOSECOND', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'('DECIMAL128', _Path, _TrUserData) -> ok;
'v_enum_greptime.v1.ColumnDataType'(V, _Path, _TrUserData) when -2147483648 =< V, V =< 2147483647, is_integer(V) -> ok;
'v_enum_greptime.v1.ColumnDataType'(X, Path, _TrUserData) -> mk_type_error({invalid_enum, 'greptime.v1.ColumnDataType'}, X, Path).

-compile({nowarn_unused_function,v_type_int32/3}).
-dialyzer({nowarn_function,v_type_int32/3}).
v_type_int32(N, _Path, _TrUserData) when is_integer(N), -2147483648 =< N, N =< 2147483647 -> ok;
v_type_int32(N, Path, _TrUserData) when is_integer(N) -> mk_type_error({value_out_of_range, int32, signed, 32}, N, Path);
v_type_int32(X, Path, _TrUserData) -> mk_type_error({bad_integer, int32, signed, 32}, X, Path).

-compile({nowarn_unused_function,v_type_int64/3}).
-dialyzer({nowarn_function,v_type_int64/3}).
v_type_int64(N, _Path, _TrUserData) when is_integer(N), -9223372036854775808 =< N, N =< 9223372036854775807 -> ok;
v_type_int64(N, Path, _TrUserData) when is_integer(N) -> mk_type_error({value_out_of_range, int64, signed, 64}, N, Path);
v_type_int64(X, Path, _TrUserData) -> mk_type_error({bad_integer, int64, signed, 64}, X, Path).

-compile({nowarn_unused_function,v_type_uint32/3}).
-dialyzer({nowarn_function,v_type_uint32/3}).
v_type_uint32(N, _Path, _TrUserData) when is_integer(N), 0 =< N, N =< 4294967295 -> ok;
v_type_uint32(N, Path, _TrUserData) when is_integer(N) -> mk_type_error({value_out_of_range, uint32, unsigned, 32}, N, Path);
v_type_uint32(X, Path, _TrUserData) -> mk_type_error({bad_integer, uint32, unsigned, 32}, X, Path).

-compile({nowarn_unused_function,v_type_uint64/3}).
-dialyzer({nowarn_function,v_type_uint64/3}).
v_type_uint64(N, _Path, _TrUserData) when is_integer(N), 0 =< N, N =< 18446744073709551615 -> ok;
v_type_uint64(N, Path, _TrUserData) when is_integer(N) -> mk_type_error({value_out_of_range, uint64, unsigned, 64}, N, Path);
v_type_uint64(X, Path, _TrUserData) -> mk_type_error({bad_integer, uint64, unsigned, 64}, X, Path).

-compile({nowarn_unused_function,v_type_bool/3}).
-dialyzer({nowarn_function,v_type_bool/3}).
v_type_bool(false, _Path, _TrUserData) -> ok;
v_type_bool(true, _Path, _TrUserData) -> ok;
v_type_bool(0, _Path, _TrUserData) -> ok;
v_type_bool(1, _Path, _TrUserData) -> ok;
v_type_bool(X, Path, _TrUserData) -> mk_type_error(bad_boolean_value, X, Path).

-compile({nowarn_unused_function,v_type_float/3}).
-dialyzer({nowarn_function,v_type_float/3}).
v_type_float(N, _Path, _TrUserData) when is_float(N) -> ok;
v_type_float(N, _Path, _TrUserData) when is_integer(N) -> ok;
v_type_float(infinity, _Path, _TrUserData) -> ok;
v_type_float('-infinity', _Path, _TrUserData) -> ok;
v_type_float(nan, _Path, _TrUserData) -> ok;
v_type_float(X, Path, _TrUserData) -> mk_type_error(bad_float_value, X, Path).

-compile({nowarn_unused_function,v_type_double/3}).
-dialyzer({nowarn_function,v_type_double/3}).
v_type_double(N, _Path, _TrUserData) when is_float(N) -> ok;
v_type_double(N, _Path, _TrUserData) when is_integer(N) -> ok;
v_type_double(infinity, _Path, _TrUserData) -> ok;
v_type_double('-infinity', _Path, _TrUserData) -> ok;
v_type_double(nan, _Path, _TrUserData) -> ok;
v_type_double(X, Path, _TrUserData) -> mk_type_error(bad_double_value, X, Path).

-compile({nowarn_unused_function,v_type_string/3}).
-dialyzer({nowarn_function,v_type_string/3}).
v_type_string(S, Path, _TrUserData) when is_list(S); is_binary(S) ->
    try unicode:characters_to_binary(S) of
        B when is_binary(B) -> ok;
        {error, _, _} -> mk_type_error(bad_unicode_string, S, Path)
    catch
        error:badarg -> mk_type_error(bad_unicode_string, S, Path)
    end;
v_type_string(X, Path, _TrUserData) -> mk_type_error(bad_unicode_string, X, Path).

-compile({nowarn_unused_function,v_type_bytes/3}).
-dialyzer({nowarn_function,v_type_bytes/3}).
v_type_bytes(B, _Path, _TrUserData) when is_binary(B) -> ok;
v_type_bytes(B, _Path, _TrUserData) when is_list(B) -> ok;
v_type_bytes(X, Path, _TrUserData) -> mk_type_error(bad_binary_value, X, Path).

-compile({nowarn_unused_function,'v_map<string,string>'/3}).
-dialyzer({nowarn_function,'v_map<string,string>'/3}).
'v_map<string,string>'(M, Path, TrUserData) when is_map(M) ->
    [begin v_type_string(Key, [key | Path], TrUserData), v_type_string(Value, [value | Path], TrUserData) end || {Key, Value} <- maps:to_list(M)],
    ok;
'v_map<string,string>'(X, Path, _TrUserData) -> mk_type_error(invalid_map, X, Path).

-compile({nowarn_unused_function,mk_type_error/3}).
-spec mk_type_error(_, _, list()) -> no_return().
mk_type_error(Error, ValueSeen, Path) ->
    Path2 = prettify_path(Path),
    erlang:error({gpb_type_error, {Error, [{value, ValueSeen}, {path, Path2}]}}).


-compile({nowarn_unused_function,prettify_path/1}).
-dialyzer({nowarn_function,prettify_path/1}).
prettify_path([]) -> top_level;
prettify_path(PathR) -> lists:append(lists:join(".", lists:map(fun atom_to_list/1, lists:reverse(PathR)))).


-compile({nowarn_unused_function,id/2}).
-compile({inline,id/2}).
id(X, _TrUserData) -> X.

-compile({nowarn_unused_function,v_ok/3}).
-compile({inline,v_ok/3}).
v_ok(_Value, _Path, _TrUserData) -> ok.

-compile({nowarn_unused_function,m_overwrite/3}).
-compile({inline,m_overwrite/3}).
m_overwrite(_Prev, New, _TrUserData) -> New.

-compile({nowarn_unused_function,cons/3}).
-compile({inline,cons/3}).
cons(Elem, Acc, _TrUserData) -> [Elem | Acc].

-compile({nowarn_unused_function,lists_reverse/2}).
-compile({inline,lists_reverse/2}).
'lists_reverse'(L, _TrUserData) -> lists:reverse(L).
-compile({nowarn_unused_function,'erlang_++'/3}).
-compile({inline,'erlang_++'/3}).
'erlang_++'(A, B, _TrUserData) -> A ++ B.
-compile({inline,'tr_encode_request_header.tracing_context[x]'/2}).
'tr_encode_request_header.tracing_context[x]'(X, _) -> mt_maptuple_to_pseudomsg_m(X).

-compile({inline,'tr_decode_init_default_request_header.tracing_context'/2}).
'tr_decode_init_default_request_header.tracing_context'(_, _) -> mt_empty_map_m().

-compile({inline,'tr_merge_request_header.tracing_context'/3}).
'tr_merge_request_header.tracing_context'(X1, X2, _) -> mt_merge_maps_m(X1, X2).

-compile({inline,'tr_decode_repeated_finalize_request_header.tracing_context'/2}).
'tr_decode_repeated_finalize_request_header.tracing_context'(L, TrUserData) -> id(L, TrUserData).

-compile({inline,'tr_encode_request_header.tracing_context'/2}).
'tr_encode_request_header.tracing_context'(X, _) -> mt_map_to_list_m(X).

-compile({inline,'tr_decode_repeated_add_elem_request_header.tracing_context'/3}).
'tr_decode_repeated_add_elem_request_header.tracing_context'(Elem, L, _) -> mt_add_item_m(Elem, L).

-compile({inline,mt_maptuple_to_pseudomsg_m/1}).
mt_maptuple_to_pseudomsg_m({K, V}) -> #{key => K, value => V}.


-compile({inline,mt_map_to_list_m/1}).
mt_map_to_list_m(M) -> maps:to_list(M).


-compile({inline,mt_empty_map_m/0}).
mt_empty_map_m() -> #{}.


-compile({inline,mt_add_item_m/2}).
mt_add_item_m(#{key := K, value := V}, M) -> M#{K => V}.


-compile({inline,mt_merge_maps_m/2}).
mt_merge_maps_m(M1, M2) -> maps:merge(M1, M2).




get_msg_defs() ->
    [{{enum, 'greptime.v1.SemanticType'}, [{'TAG', 0}, {'FIELD', 1}, {'TIMESTAMP', 2}]},
     {{enum, 'greptime.v1.ColumnDataType'},
      [{'BOOLEAN', 0},
       {'INT8', 1},
       {'INT16', 2},
       {'INT32', 3},
       {'INT64', 4},
       {'UINT8', 5},
       {'UINT16', 6},
       {'UINT32', 7},
       {'UINT64', 8},
       {'FLOAT32', 9},
       {'FLOAT64', 10},
       {'BINARY', 11},
       {'STRING', 12},
       {'DATE', 13},
       {'DATETIME', 14},
       {'TIMESTAMP_SECOND', 15},
       {'TIMESTAMP_MILLISECOND', 16},
       {'TIMESTAMP_MICROSECOND', 17},
       {'TIMESTAMP_NANOSECOND', 18},
       {'TIME_SECOND', 19},
       {'TIME_MILLISECOND', 20},
       {'TIME_MICROSECOND', 21},
       {'TIME_NANOSECOND', 22},
       {'INTERVAL_YEAR_MONTH', 23},
       {'INTERVAL_DAY_TIME', 24},
       {'INTERVAL_MONTH_DAY_NANO', 25},
       {'DURATION_SECOND', 26},
       {'DURATION_MILLISECOND', 27},
       {'DURATION_MICROSECOND', 28},
       {'DURATION_NANOSECOND', 29},
       {'DECIMAL128', 30}]},
     {{msg, values},
      [#{name => i8_values, fnum => 1, rnum => 2, type => int32, occurrence => repeated, opts => [packed]},
       #{name => i16_values, fnum => 2, rnum => 3, type => int32, occurrence => repeated, opts => [packed]},
       #{name => i32_values, fnum => 3, rnum => 4, type => int32, occurrence => repeated, opts => [packed]},
       #{name => i64_values, fnum => 4, rnum => 5, type => int64, occurrence => repeated, opts => [packed]},
       #{name => u8_values, fnum => 5, rnum => 6, type => uint32, occurrence => repeated, opts => [packed]},
       #{name => u16_values, fnum => 6, rnum => 7, type => uint32, occurrence => repeated, opts => [packed]},
       #{name => u32_values, fnum => 7, rnum => 8, type => uint32, occurrence => repeated, opts => [packed]},
       #{name => u64_values, fnum => 8, rnum => 9, type => uint64, occurrence => repeated, opts => [packed]},
       #{name => f32_values, fnum => 9, rnum => 10, type => float, occurrence => repeated, opts => [packed]},
       #{name => f64_values, fnum => 10, rnum => 11, type => double, occurrence => repeated, opts => [packed]},
       #{name => bool_values, fnum => 11, rnum => 12, type => bool, occurrence => repeated, opts => [packed]},
       #{name => binary_values, fnum => 12, rnum => 13, type => bytes, occurrence => repeated, opts => []},
       #{name => string_values, fnum => 13, rnum => 14, type => string, occurrence => repeated, opts => []},
       #{name => date_values, fnum => 14, rnum => 15, type => int32, occurrence => repeated, opts => [packed]},
       #{name => datetime_values, fnum => 15, rnum => 16, type => int64, occurrence => repeated, opts => [packed]},
       #{name => timestamp_second_values, fnum => 16, rnum => 17, type => int64, occurrence => repeated, opts => [packed]},
       #{name => timestamp_millisecond_values, fnum => 17, rnum => 18, type => int64, occurrence => repeated, opts => [packed]},
       #{name => timestamp_microsecond_values, fnum => 18, rnum => 19, type => int64, occurrence => repeated, opts => [packed]},
       #{name => timestamp_nanosecond_values, fnum => 19, rnum => 20, type => int64, occurrence => repeated, opts => [packed]},
       #{name => time_second_values, fnum => 20, rnum => 21, type => int64, occurrence => repeated, opts => [packed]},
       #{name => time_millisecond_values, fnum => 21, rnum => 22, type => int64, occurrence => repeated, opts => [packed]},
       #{name => time_microsecond_values, fnum => 22, rnum => 23, type => int64, occurrence => repeated, opts => [packed]},
       #{name => time_nanosecond_values, fnum => 23, rnum => 24, type => int64, occurrence => repeated, opts => [packed]},
       #{name => interval_year_month_values, fnum => 24, rnum => 25, type => int32, occurrence => repeated, opts => [packed]},
       #{name => interval_day_time_values, fnum => 25, rnum => 26, type => int64, occurrence => repeated, opts => [packed]},
       #{name => interval_month_day_nano_values, fnum => 26, rnum => 27, type => {msg, interval_month_day_nano}, occurrence => repeated, opts => []},
       #{name => duration_second_values, fnum => 27, rnum => 28, type => int64, occurrence => repeated, opts => [packed]},
       #{name => duration_millisecond_values, fnum => 28, rnum => 29, type => int64, occurrence => repeated, opts => [packed]},
       #{name => duration_microsecond_values, fnum => 29, rnum => 30, type => int64, occurrence => repeated, opts => [packed]},
       #{name => duration_nanosecond_values, fnum => 30, rnum => 31, type => int64, occurrence => repeated, opts => [packed]},
       #{name => decimal128_values, fnum => 31, rnum => 32, type => {msg, decimal_128}, occurrence => repeated, opts => []}]},
     {{msg, column},
      [#{name => column_name, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []},
       #{name => semantic_type, fnum => 2, rnum => 3, type => {enum, 'greptime.v1.SemanticType'}, occurrence => optional, opts => []},
       #{name => values, fnum => 3, rnum => 4, type => {msg, values}, occurrence => optional, opts => []},
       #{name => null_mask, fnum => 4, rnum => 5, type => bytes, occurrence => optional, opts => []},
       #{name => datatype, fnum => 5, rnum => 6, type => {enum, 'greptime.v1.ColumnDataType'}, occurrence => optional, opts => []},
       #{name => datatype_extension, fnum => 6, rnum => 7, type => {msg, column_data_type_extension}, occurrence => optional, opts => []}]},
     {{msg, request_header},
      [#{name => catalog, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []},
       #{name => schema, fnum => 2, rnum => 3, type => string, occurrence => optional, opts => []},
       #{name => authorization, fnum => 3, rnum => 4, type => {msg, auth_header}, occurrence => optional, opts => []},
       #{name => dbname, fnum => 4, rnum => 5, type => string, occurrence => optional, opts => []},
       #{name => tracing_context, fnum => 5, rnum => 6, type => {map, string, string}, occurrence => repeated, opts => []}]},
     {{msg, response_header}, [#{name => status, fnum => 1, rnum => 2, type => {msg, status}, occurrence => optional, opts => []}]},
     {{msg, status}, [#{name => status_code, fnum => 1, rnum => 2, type => uint32, occurrence => optional, opts => []}, #{name => err_msg, fnum => 2, rnum => 3, type => string, occurrence => optional, opts => []}]},
     {{msg, auth_header},
      [#{name => auth_scheme, rnum => 2, fields => [#{name => basic, fnum => 1, rnum => 2, type => {msg, basic}, occurrence => optional, opts => []}, #{name => token, fnum => 2, rnum => 2, type => {msg, token}, occurrence => optional, opts => []}],
         opts => []}]},
     {{msg, basic}, [#{name => username, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []}, #{name => password, fnum => 2, rnum => 3, type => string, occurrence => optional, opts => []}]},
     {{msg, token}, [#{name => token, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []}]},
     {{msg, affected_rows}, [#{name => value, fnum => 1, rnum => 2, type => uint32, occurrence => optional, opts => []}]},
     {{msg, flight_metadata}, [#{name => affected_rows, fnum => 1, rnum => 2, type => {msg, affected_rows}, occurrence => optional, opts => []}]},
     {{msg, interval_month_day_nano},
      [#{name => months, fnum => 1, rnum => 2, type => int32, occurrence => optional, opts => []},
       #{name => days, fnum => 2, rnum => 3, type => int32, occurrence => optional, opts => []},
       #{name => nanoseconds, fnum => 3, rnum => 4, type => int64, occurrence => optional, opts => []}]},
     {{msg, decimal_128}, [#{name => hi, fnum => 1, rnum => 2, type => int64, occurrence => optional, opts => []}, #{name => lo, fnum => 2, rnum => 3, type => int64, occurrence => optional, opts => []}]},
     {{msg, column_data_type_extension}, [#{name => type_ext, rnum => 2, fields => [#{name => decimal_type, fnum => 1, rnum => 2, type => {msg, decimal_type_extension}, occurrence => optional, opts => []}], opts => []}]},
     {{msg, decimal_type_extension}, [#{name => precision, fnum => 1, rnum => 2, type => int32, occurrence => optional, opts => []}, #{name => scale, fnum => 2, rnum => 3, type => int32, occurrence => optional, opts => []}]}].


get_msg_names() -> [values, column, request_header, response_header, status, auth_header, basic, token, affected_rows, flight_metadata, interval_month_day_nano, decimal_128, column_data_type_extension, decimal_type_extension].


get_group_names() -> [].


get_msg_or_group_names() -> [values, column, request_header, response_header, status, auth_header, basic, token, affected_rows, flight_metadata, interval_month_day_nano, decimal_128, column_data_type_extension, decimal_type_extension].


get_enum_names() -> ['greptime.v1.SemanticType', 'greptime.v1.ColumnDataType'].


fetch_msg_def(MsgName) ->
    case find_msg_def(MsgName) of
        Fs when is_list(Fs) -> Fs;
        error -> erlang:error({no_such_msg, MsgName})
    end.


fetch_enum_def(EnumName) ->
    case find_enum_def(EnumName) of
        Es when is_list(Es) -> Es;
        error -> erlang:error({no_such_enum, EnumName})
    end.


find_msg_def(values) ->
    [#{name => i8_values, fnum => 1, rnum => 2, type => int32, occurrence => repeated, opts => [packed]},
     #{name => i16_values, fnum => 2, rnum => 3, type => int32, occurrence => repeated, opts => [packed]},
     #{name => i32_values, fnum => 3, rnum => 4, type => int32, occurrence => repeated, opts => [packed]},
     #{name => i64_values, fnum => 4, rnum => 5, type => int64, occurrence => repeated, opts => [packed]},
     #{name => u8_values, fnum => 5, rnum => 6, type => uint32, occurrence => repeated, opts => [packed]},
     #{name => u16_values, fnum => 6, rnum => 7, type => uint32, occurrence => repeated, opts => [packed]},
     #{name => u32_values, fnum => 7, rnum => 8, type => uint32, occurrence => repeated, opts => [packed]},
     #{name => u64_values, fnum => 8, rnum => 9, type => uint64, occurrence => repeated, opts => [packed]},
     #{name => f32_values, fnum => 9, rnum => 10, type => float, occurrence => repeated, opts => [packed]},
     #{name => f64_values, fnum => 10, rnum => 11, type => double, occurrence => repeated, opts => [packed]},
     #{name => bool_values, fnum => 11, rnum => 12, type => bool, occurrence => repeated, opts => [packed]},
     #{name => binary_values, fnum => 12, rnum => 13, type => bytes, occurrence => repeated, opts => []},
     #{name => string_values, fnum => 13, rnum => 14, type => string, occurrence => repeated, opts => []},
     #{name => date_values, fnum => 14, rnum => 15, type => int32, occurrence => repeated, opts => [packed]},
     #{name => datetime_values, fnum => 15, rnum => 16, type => int64, occurrence => repeated, opts => [packed]},
     #{name => timestamp_second_values, fnum => 16, rnum => 17, type => int64, occurrence => repeated, opts => [packed]},
     #{name => timestamp_millisecond_values, fnum => 17, rnum => 18, type => int64, occurrence => repeated, opts => [packed]},
     #{name => timestamp_microsecond_values, fnum => 18, rnum => 19, type => int64, occurrence => repeated, opts => [packed]},
     #{name => timestamp_nanosecond_values, fnum => 19, rnum => 20, type => int64, occurrence => repeated, opts => [packed]},
     #{name => time_second_values, fnum => 20, rnum => 21, type => int64, occurrence => repeated, opts => [packed]},
     #{name => time_millisecond_values, fnum => 21, rnum => 22, type => int64, occurrence => repeated, opts => [packed]},
     #{name => time_microsecond_values, fnum => 22, rnum => 23, type => int64, occurrence => repeated, opts => [packed]},
     #{name => time_nanosecond_values, fnum => 23, rnum => 24, type => int64, occurrence => repeated, opts => [packed]},
     #{name => interval_year_month_values, fnum => 24, rnum => 25, type => int32, occurrence => repeated, opts => [packed]},
     #{name => interval_day_time_values, fnum => 25, rnum => 26, type => int64, occurrence => repeated, opts => [packed]},
     #{name => interval_month_day_nano_values, fnum => 26, rnum => 27, type => {msg, interval_month_day_nano}, occurrence => repeated, opts => []},
     #{name => duration_second_values, fnum => 27, rnum => 28, type => int64, occurrence => repeated, opts => [packed]},
     #{name => duration_millisecond_values, fnum => 28, rnum => 29, type => int64, occurrence => repeated, opts => [packed]},
     #{name => duration_microsecond_values, fnum => 29, rnum => 30, type => int64, occurrence => repeated, opts => [packed]},
     #{name => duration_nanosecond_values, fnum => 30, rnum => 31, type => int64, occurrence => repeated, opts => [packed]},
     #{name => decimal128_values, fnum => 31, rnum => 32, type => {msg, decimal_128}, occurrence => repeated, opts => []}];
find_msg_def(column) ->
    [#{name => column_name, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []},
     #{name => semantic_type, fnum => 2, rnum => 3, type => {enum, 'greptime.v1.SemanticType'}, occurrence => optional, opts => []},
     #{name => values, fnum => 3, rnum => 4, type => {msg, values}, occurrence => optional, opts => []},
     #{name => null_mask, fnum => 4, rnum => 5, type => bytes, occurrence => optional, opts => []},
     #{name => datatype, fnum => 5, rnum => 6, type => {enum, 'greptime.v1.ColumnDataType'}, occurrence => optional, opts => []},
     #{name => datatype_extension, fnum => 6, rnum => 7, type => {msg, column_data_type_extension}, occurrence => optional, opts => []}];
find_msg_def(request_header) ->
    [#{name => catalog, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []},
     #{name => schema, fnum => 2, rnum => 3, type => string, occurrence => optional, opts => []},
     #{name => authorization, fnum => 3, rnum => 4, type => {msg, auth_header}, occurrence => optional, opts => []},
     #{name => dbname, fnum => 4, rnum => 5, type => string, occurrence => optional, opts => []},
     #{name => tracing_context, fnum => 5, rnum => 6, type => {map, string, string}, occurrence => repeated, opts => []}];
find_msg_def(response_header) -> [#{name => status, fnum => 1, rnum => 2, type => {msg, status}, occurrence => optional, opts => []}];
find_msg_def(status) -> [#{name => status_code, fnum => 1, rnum => 2, type => uint32, occurrence => optional, opts => []}, #{name => err_msg, fnum => 2, rnum => 3, type => string, occurrence => optional, opts => []}];
find_msg_def(auth_header) ->
    [#{name => auth_scheme, rnum => 2, fields => [#{name => basic, fnum => 1, rnum => 2, type => {msg, basic}, occurrence => optional, opts => []}, #{name => token, fnum => 2, rnum => 2, type => {msg, token}, occurrence => optional, opts => []}],
       opts => []}];
find_msg_def(basic) -> [#{name => username, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []}, #{name => password, fnum => 2, rnum => 3, type => string, occurrence => optional, opts => []}];
find_msg_def(token) -> [#{name => token, fnum => 1, rnum => 2, type => string, occurrence => optional, opts => []}];
find_msg_def(affected_rows) -> [#{name => value, fnum => 1, rnum => 2, type => uint32, occurrence => optional, opts => []}];
find_msg_def(flight_metadata) -> [#{name => affected_rows, fnum => 1, rnum => 2, type => {msg, affected_rows}, occurrence => optional, opts => []}];
find_msg_def(interval_month_day_nano) ->
    [#{name => months, fnum => 1, rnum => 2, type => int32, occurrence => optional, opts => []},
     #{name => days, fnum => 2, rnum => 3, type => int32, occurrence => optional, opts => []},
     #{name => nanoseconds, fnum => 3, rnum => 4, type => int64, occurrence => optional, opts => []}];
find_msg_def(decimal_128) -> [#{name => hi, fnum => 1, rnum => 2, type => int64, occurrence => optional, opts => []}, #{name => lo, fnum => 2, rnum => 3, type => int64, occurrence => optional, opts => []}];
find_msg_def(column_data_type_extension) -> [#{name => type_ext, rnum => 2, fields => [#{name => decimal_type, fnum => 1, rnum => 2, type => {msg, decimal_type_extension}, occurrence => optional, opts => []}], opts => []}];
find_msg_def(decimal_type_extension) -> [#{name => precision, fnum => 1, rnum => 2, type => int32, occurrence => optional, opts => []}, #{name => scale, fnum => 2, rnum => 3, type => int32, occurrence => optional, opts => []}];
find_msg_def(_) -> error.


find_enum_def('greptime.v1.SemanticType') -> [{'TAG', 0}, {'FIELD', 1}, {'TIMESTAMP', 2}];
find_enum_def('greptime.v1.ColumnDataType') ->
    [{'BOOLEAN', 0},
     {'INT8', 1},
     {'INT16', 2},
     {'INT32', 3},
     {'INT64', 4},
     {'UINT8', 5},
     {'UINT16', 6},
     {'UINT32', 7},
     {'UINT64', 8},
     {'FLOAT32', 9},
     {'FLOAT64', 10},
     {'BINARY', 11},
     {'STRING', 12},
     {'DATE', 13},
     {'DATETIME', 14},
     {'TIMESTAMP_SECOND', 15},
     {'TIMESTAMP_MILLISECOND', 16},
     {'TIMESTAMP_MICROSECOND', 17},
     {'TIMESTAMP_NANOSECOND', 18},
     {'TIME_SECOND', 19},
     {'TIME_MILLISECOND', 20},
     {'TIME_MICROSECOND', 21},
     {'TIME_NANOSECOND', 22},
     {'INTERVAL_YEAR_MONTH', 23},
     {'INTERVAL_DAY_TIME', 24},
     {'INTERVAL_MONTH_DAY_NANO', 25},
     {'DURATION_SECOND', 26},
     {'DURATION_MILLISECOND', 27},
     {'DURATION_MICROSECOND', 28},
     {'DURATION_NANOSECOND', 29},
     {'DECIMAL128', 30}];
find_enum_def(_) -> error.


enum_symbol_by_value('greptime.v1.SemanticType', Value) -> 'enum_symbol_by_value_greptime.v1.SemanticType'(Value);
enum_symbol_by_value('greptime.v1.ColumnDataType', Value) -> 'enum_symbol_by_value_greptime.v1.ColumnDataType'(Value).


enum_value_by_symbol('greptime.v1.SemanticType', Sym) -> 'enum_value_by_symbol_greptime.v1.SemanticType'(Sym);
enum_value_by_symbol('greptime.v1.ColumnDataType', Sym) -> 'enum_value_by_symbol_greptime.v1.ColumnDataType'(Sym).


'enum_symbol_by_value_greptime.v1.SemanticType'(0) -> 'TAG';
'enum_symbol_by_value_greptime.v1.SemanticType'(1) -> 'FIELD';
'enum_symbol_by_value_greptime.v1.SemanticType'(2) -> 'TIMESTAMP'.


'enum_value_by_symbol_greptime.v1.SemanticType'('TAG') -> 0;
'enum_value_by_symbol_greptime.v1.SemanticType'('FIELD') -> 1;
'enum_value_by_symbol_greptime.v1.SemanticType'('TIMESTAMP') -> 2.

'enum_symbol_by_value_greptime.v1.ColumnDataType'(0) -> 'BOOLEAN';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(1) -> 'INT8';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(2) -> 'INT16';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(3) -> 'INT32';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(4) -> 'INT64';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(5) -> 'UINT8';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(6) -> 'UINT16';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(7) -> 'UINT32';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(8) -> 'UINT64';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(9) -> 'FLOAT32';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(10) -> 'FLOAT64';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(11) -> 'BINARY';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(12) -> 'STRING';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(13) -> 'DATE';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(14) -> 'DATETIME';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(15) -> 'TIMESTAMP_SECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(16) -> 'TIMESTAMP_MILLISECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(17) -> 'TIMESTAMP_MICROSECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(18) -> 'TIMESTAMP_NANOSECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(19) -> 'TIME_SECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(20) -> 'TIME_MILLISECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(21) -> 'TIME_MICROSECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(22) -> 'TIME_NANOSECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(23) -> 'INTERVAL_YEAR_MONTH';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(24) -> 'INTERVAL_DAY_TIME';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(25) -> 'INTERVAL_MONTH_DAY_NANO';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(26) -> 'DURATION_SECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(27) -> 'DURATION_MILLISECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(28) -> 'DURATION_MICROSECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(29) -> 'DURATION_NANOSECOND';
'enum_symbol_by_value_greptime.v1.ColumnDataType'(30) -> 'DECIMAL128'.


'enum_value_by_symbol_greptime.v1.ColumnDataType'('BOOLEAN') -> 0;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INT8') -> 1;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INT16') -> 2;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INT32') -> 3;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INT64') -> 4;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('UINT8') -> 5;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('UINT16') -> 6;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('UINT32') -> 7;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('UINT64') -> 8;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('FLOAT32') -> 9;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('FLOAT64') -> 10;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('BINARY') -> 11;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('STRING') -> 12;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DATE') -> 13;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DATETIME') -> 14;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIMESTAMP_SECOND') -> 15;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIMESTAMP_MILLISECOND') -> 16;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIMESTAMP_MICROSECOND') -> 17;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIMESTAMP_NANOSECOND') -> 18;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIME_SECOND') -> 19;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIME_MILLISECOND') -> 20;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIME_MICROSECOND') -> 21;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('TIME_NANOSECOND') -> 22;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INTERVAL_YEAR_MONTH') -> 23;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INTERVAL_DAY_TIME') -> 24;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('INTERVAL_MONTH_DAY_NANO') -> 25;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DURATION_SECOND') -> 26;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DURATION_MILLISECOND') -> 27;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DURATION_MICROSECOND') -> 28;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DURATION_NANOSECOND') -> 29;
'enum_value_by_symbol_greptime.v1.ColumnDataType'('DECIMAL128') -> 30.


get_service_names() -> [].


get_service_def(_) -> error.


get_rpc_names(_) -> error.


find_rpc_def(_, _) -> error.



-spec fetch_rpc_def(_, _) -> no_return().
fetch_rpc_def(ServiceName, RpcName) -> erlang:error({no_such_rpc, ServiceName, RpcName}).


%% Convert a a fully qualified (ie with package name) service name
%% as a binary to a service name as an atom.
-spec fqbin_to_service_name(_) -> no_return().
fqbin_to_service_name(X) -> error({gpb_error, {badservice, X}}).


%% Convert a service name as an atom to a fully qualified
%% (ie with package name) name as a binary.
-spec service_name_to_fqbin(_) -> no_return().
service_name_to_fqbin(X) -> error({gpb_error, {badservice, X}}).


%% Convert a a fully qualified (ie with package name) service name
%% and an rpc name, both as binaries to a service name and an rpc
%% name, as atoms.
-spec fqbins_to_service_and_rpc_name(_, _) -> no_return().
fqbins_to_service_and_rpc_name(S, R) -> error({gpb_error, {badservice_or_rpc, {S, R}}}).


%% Convert a service name and an rpc name, both as atoms,
%% to a fully qualified (ie with package name) service name and
%% an rpc name as binaries.
-spec service_and_rpc_name_to_fqbins(_, _) -> no_return().
service_and_rpc_name_to_fqbins(S, R) -> error({gpb_error, {badservice_or_rpc, {S, R}}}).


fqbin_to_msg_name(<<"greptime.v1.Column.Values">>) -> values;
fqbin_to_msg_name(<<"greptime.v1.Column">>) -> column;
fqbin_to_msg_name(<<"greptime.v1.RequestHeader">>) -> request_header;
fqbin_to_msg_name(<<"greptime.v1.ResponseHeader">>) -> response_header;
fqbin_to_msg_name(<<"greptime.v1.Status">>) -> status;
fqbin_to_msg_name(<<"greptime.v1.AuthHeader">>) -> auth_header;
fqbin_to_msg_name(<<"greptime.v1.Basic">>) -> basic;
fqbin_to_msg_name(<<"greptime.v1.Token">>) -> token;
fqbin_to_msg_name(<<"greptime.v1.AffectedRows">>) -> affected_rows;
fqbin_to_msg_name(<<"greptime.v1.FlightMetadata">>) -> flight_metadata;
fqbin_to_msg_name(<<"greptime.v1.IntervalMonthDayNano">>) -> interval_month_day_nano;
fqbin_to_msg_name(<<"greptime.v1.Decimal128">>) -> decimal_128;
fqbin_to_msg_name(<<"greptime.v1.ColumnDataTypeExtension">>) -> column_data_type_extension;
fqbin_to_msg_name(<<"greptime.v1.DecimalTypeExtension">>) -> decimal_type_extension;
fqbin_to_msg_name(E) -> error({gpb_error, {badmsg, E}}).


msg_name_to_fqbin(values) -> <<"greptime.v1.Column.Values">>;
msg_name_to_fqbin(column) -> <<"greptime.v1.Column">>;
msg_name_to_fqbin(request_header) -> <<"greptime.v1.RequestHeader">>;
msg_name_to_fqbin(response_header) -> <<"greptime.v1.ResponseHeader">>;
msg_name_to_fqbin(status) -> <<"greptime.v1.Status">>;
msg_name_to_fqbin(auth_header) -> <<"greptime.v1.AuthHeader">>;
msg_name_to_fqbin(basic) -> <<"greptime.v1.Basic">>;
msg_name_to_fqbin(token) -> <<"greptime.v1.Token">>;
msg_name_to_fqbin(affected_rows) -> <<"greptime.v1.AffectedRows">>;
msg_name_to_fqbin(flight_metadata) -> <<"greptime.v1.FlightMetadata">>;
msg_name_to_fqbin(interval_month_day_nano) -> <<"greptime.v1.IntervalMonthDayNano">>;
msg_name_to_fqbin(decimal_128) -> <<"greptime.v1.Decimal128">>;
msg_name_to_fqbin(column_data_type_extension) -> <<"greptime.v1.ColumnDataTypeExtension">>;
msg_name_to_fqbin(decimal_type_extension) -> <<"greptime.v1.DecimalTypeExtension">>;
msg_name_to_fqbin(E) -> error({gpb_error, {badmsg, E}}).


fqbin_to_enum_name(<<"greptime.v1.SemanticType">>) -> 'greptime.v1.SemanticType';
fqbin_to_enum_name(<<"greptime.v1.ColumnDataType">>) -> 'greptime.v1.ColumnDataType';
fqbin_to_enum_name(E) -> error({gpb_error, {badenum, E}}).


enum_name_to_fqbin('greptime.v1.SemanticType') -> <<"greptime.v1.SemanticType">>;
enum_name_to_fqbin('greptime.v1.ColumnDataType') -> <<"greptime.v1.ColumnDataType">>;
enum_name_to_fqbin(E) -> error({gpb_error, {badenum, E}}).


get_package_name() -> 'greptime.v1'.


%% Whether or not the message names
%% are prepended with package name or not.
uses_packages() -> true.


source_basename() -> "column.proto".


%% Retrieve all proto file names, also imported ones.
%% The order is top-down. The first element is always the main
%% source file. The files are returned with extension,
%% see get_all_proto_names/0 for a version that returns
%% the basenames sans extension
get_all_source_basenames() -> ["column.proto", "common.proto"].


%% Retrieve all proto file names, also imported ones.
%% The order is top-down. The first element is always the main
%% source file. The files are returned sans .proto extension,
%% to make it easier to use them with the various get_xyz_containment
%% functions.
get_all_proto_names() -> ["column", "common"].


get_msg_containment("column") -> [column, values];
get_msg_containment("common") -> [affected_rows, auth_header, basic, column_data_type_extension, decimal_128, decimal_type_extension, flight_metadata, interval_month_day_nano, request_header, response_header, status, token];
get_msg_containment(P) -> error({gpb_error, {badproto, P}}).


get_pkg_containment("column") -> 'greptime.v1';
get_pkg_containment("common") -> 'greptime.v1';
get_pkg_containment(P) -> error({gpb_error, {badproto, P}}).


get_service_containment("column") -> [];
get_service_containment("common") -> [];
get_service_containment(P) -> error({gpb_error, {badproto, P}}).


get_rpc_containment("column") -> [];
get_rpc_containment("common") -> [];
get_rpc_containment(P) -> error({gpb_error, {badproto, P}}).


get_enum_containment("column") -> [];
get_enum_containment("common") -> ['greptime.v1.ColumnDataType', 'greptime.v1.SemanticType'];
get_enum_containment(P) -> error({gpb_error, {badproto, P}}).


get_proto_by_msg_name_as_fqbin(<<"greptime.v1.FlightMetadata">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.ResponseHeader">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.RequestHeader">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.AuthHeader">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Status">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Basic">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.AffectedRows">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Column.Values">>) -> "column";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Decimal128">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Token">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.DecimalTypeExtension">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.ColumnDataTypeExtension">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Column">>) -> "column";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.IntervalMonthDayNano">>) -> "common";
get_proto_by_msg_name_as_fqbin(E) -> error({gpb_error, {badmsg, E}}).


-spec get_proto_by_service_name_as_fqbin(_) -> no_return().
get_proto_by_service_name_as_fqbin(E) -> error({gpb_error, {badservice, E}}).


get_proto_by_enum_name_as_fqbin(<<"greptime.v1.SemanticType">>) -> "common";
get_proto_by_enum_name_as_fqbin(<<"greptime.v1.ColumnDataType">>) -> "common";
get_proto_by_enum_name_as_fqbin(E) -> error({gpb_error, {badenum, E}}).


get_protos_by_pkg_name_as_fqbin(<<"greptime.v1">>) -> ["column", "common"];
get_protos_by_pkg_name_as_fqbin(E) -> error({gpb_error, {badpkg, E}}).



gpb_version_as_string() ->
    "4.20.0".

gpb_version_as_list() ->
    [4,20,0].

gpb_version_source() ->
    "file".
