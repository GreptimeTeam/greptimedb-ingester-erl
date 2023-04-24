-module(greptimedb).

-export([start_client/1, collect_columns/1, send/0]).

-define(TS_COLUMN, "greptime_timestamp").

start_client(#{endpoints := Endpoints} = Options) ->
    application:load(grpcbox),
    application:set_env(grpcbox,
                        client,
                        #{channels =>
                              [{default_channel,
                                lists:map(fun({Schema, Host, Port}) -> {Schema, Host, Port, []} end,
                                          Endpoints),
                                maps:get(Options, options, #{})}]}),
    application:ensure_all_started(grpcbox).

send() ->
    Columns =
        greptimedb:collect_columns(#{fields => [],
                                     tags => [],
                                     timestamp => 1}),
    greptime_v_1_greptime_database_client:handle(#{header =>
                                                       #{catalog => "greptime", schema => "public"},
                                                   request =>
                                                       {insert,
                                                        #{table_name => "test",
                                                          columns => Columns,
                                                          row_count => 1}}}).

ddl() ->
    ok.

%% convert a point to columns in pb format
convert_columns(#{fields := Fields,
                  tags := Tags,
                  timestamp := Ts}) ->
    TsColumn = ts_column(Ts),
    FieldsAndTsColumns =
        maps:fold(fun(K, V1, Acc) ->
                     V2 = field_column(K, V1),
                     [V2 | Acc]
                  end,
                  [TsColumn],
                  Fields),

    Columns =
        maps:fold(fun(K, V1, Acc) ->
                     V2 = tag_column(K, V1),
                     [V2 | Acc]
                  end,
                  FieldsAndTsColumns,
                  Tags),

    Columns.

collect_columns(List) ->
    collect_columns(List, #{}).

collect_columns([], Acc) ->
    Acc;
collect_columns([#{} | T], Acc) ->
    collect_columns(T, Acc).

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
