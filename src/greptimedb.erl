-module(greptimedb).

-export([start_client/1, collect_columns/1, send/0]).

start_client(#{endpoints := Endpoints} = Options) ->
    application:load(grpcbox),
    application:set_env(grpcbox, client,
                        #{channels => [{default_channel,
                                        lists:map(fun({Schema, Host, Port}) ->
                                                          {Schema, Host, Port, []}
                                                  end,
                                                  Endpoints),
                                        maps:get(Options, options, #{})}]}),
    application:ensure_all_started(grpcbox).

send() ->
    Columns = greptimedb:collect_columns(#{fields => [], tags => [], timestamp => 1}),
    greptime_v_1_greptime_database_client:handle(#{header => #{catalog=>"greptime", schema => "public"}, request => {insert , #{table_name => "test", columns => Columns, row_count=>1 }}}).

collect_columns(#{fields := Fields, tags := Tags, timestamp := Ts}) ->
    [#{column_name => "test",
       semantic_type => 'FIELD',
       values => #{ i64_values => [1]},
       datatype => 'INT64'
      },
     #{column_name => "ts",
       semantic_type => 'TIMESTAMP',
       values => #{ ts_second_values => [1682227689]},
       datatype => 'TIMESTAMP_SECOND'
      }];

collect_columns(List) ->
    collect_columns(List, #{}).

collect_columns([], Acc) ->
    Acc;

collect_columns([#{} | T], Acc) ->
    collect_columns(T, Acc).
