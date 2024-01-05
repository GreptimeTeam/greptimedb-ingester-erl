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

-module(greptimedb_common_pb).

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

-export_type(['request_header'/0, 'response_header'/0, 'status'/0, 'auth_header'/0, 'basic'/0, 'token'/0, 'affected_rows'/0, 'flight_metadata'/0, 'interval_month_day_nano'/0, 'decimal_128'/0, 'column_data_type_extension'/0, 'decimal_type_extension'/0]).
-type '$msg_name'() :: request_header | response_header | status | auth_header | basic | token | affected_rows | flight_metadata | interval_month_day_nano | decimal_128 | column_data_type_extension | decimal_type_extension.
-type '$msg'() :: request_header() | response_header() | status() | auth_header() | basic() | token() | affected_rows() | flight_metadata() | interval_month_day_nano() | decimal_128() | column_data_type_extension() | decimal_type_extension().
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


get_msg_names() -> [request_header, response_header, status, auth_header, basic, token, affected_rows, flight_metadata, interval_month_day_nano, decimal_128, column_data_type_extension, decimal_type_extension].


get_group_names() -> [].


get_msg_or_group_names() -> [request_header, response_header, status, auth_header, basic, token, affected_rows, flight_metadata, interval_month_day_nano, decimal_128, column_data_type_extension, decimal_type_extension].


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


source_basename() -> "common.proto".


%% Retrieve all proto file names, also imported ones.
%% The order is top-down. The first element is always the main
%% source file. The files are returned with extension,
%% see get_all_proto_names/0 for a version that returns
%% the basenames sans extension
get_all_source_basenames() -> ["common.proto"].


%% Retrieve all proto file names, also imported ones.
%% The order is top-down. The first element is always the main
%% source file. The files are returned sans .proto extension,
%% to make it easier to use them with the various get_xyz_containment
%% functions.
get_all_proto_names() -> ["common"].


get_msg_containment("common") -> [affected_rows, auth_header, basic, column_data_type_extension, decimal_128, decimal_type_extension, flight_metadata, interval_month_day_nano, request_header, response_header, status, token];
get_msg_containment(P) -> error({gpb_error, {badproto, P}}).


get_pkg_containment("common") -> 'greptime.v1';
get_pkg_containment(P) -> error({gpb_error, {badproto, P}}).


get_service_containment("common") -> [];
get_service_containment(P) -> error({gpb_error, {badproto, P}}).


get_rpc_containment("common") -> [];
get_rpc_containment(P) -> error({gpb_error, {badproto, P}}).


get_enum_containment("common") -> ['greptime.v1.ColumnDataType', 'greptime.v1.SemanticType'];
get_enum_containment(P) -> error({gpb_error, {badproto, P}}).


get_proto_by_msg_name_as_fqbin(<<"greptime.v1.FlightMetadata">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.ResponseHeader">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.RequestHeader">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.AuthHeader">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Status">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Basic">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.AffectedRows">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Decimal128">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.Token">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.DecimalTypeExtension">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.ColumnDataTypeExtension">>) -> "common";
get_proto_by_msg_name_as_fqbin(<<"greptime.v1.IntervalMonthDayNano">>) -> "common";
get_proto_by_msg_name_as_fqbin(E) -> error({gpb_error, {badmsg, E}}).


-spec get_proto_by_service_name_as_fqbin(_) -> no_return().
get_proto_by_service_name_as_fqbin(E) -> error({gpb_error, {badservice, E}}).


get_proto_by_enum_name_as_fqbin(<<"greptime.v1.SemanticType">>) -> "common";
get_proto_by_enum_name_as_fqbin(<<"greptime.v1.ColumnDataType">>) -> "common";
get_proto_by_enum_name_as_fqbin(E) -> error({gpb_error, {badenum, E}}).


get_protos_by_pkg_name_as_fqbin(<<"greptime.v1">>) -> ["common"];
get_protos_by_pkg_name_as_fqbin(E) -> error({gpb_error, {badpkg, E}}).



gpb_version_as_string() ->
    "4.20.0".

gpb_version_as_list() ->
    [4,20,0].

gpb_version_source() ->
    "file".
