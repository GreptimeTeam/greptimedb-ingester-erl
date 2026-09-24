# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.3.0] - 2026-09-24

### Added

- `greptimedb_values:json_value/1` for legacy `JSON` columns, taking encoded JSON text.
- `greptimedb_values:json2_value/1` for `JSON2` columns, taking a decoded JSON term
  (maps with binary keys, lists, binaries, numbers, `true`/`false`/`null`). JSON2 is
  supported in fields only, and tables with a `JSON2` column must be append-only.

### Changed

- Synced `common.proto`, `row.proto` and `column.proto` with greptime-proto.

## [0.2.6] - 2026-09-23

### Added

- Configurable timeouts as client options: `connect_timeout` (default `5000`),
  `request_timeout` (default `10000`) and `health_check_timeout` (default `10000`),
  all in milliseconds. `connect_timeout` and `request_timeout` keep the values that
  were hardcoded before; the default health check deadline increases from
  1000 ms to 10000 ms.
- `tcp_user_timeout` client option (default `0`, disabled), set as `TCP_USER_TIMEOUT`
  on the connection. The client sends neither TCP keepalive nor HTTP/2 pings, so
  a silently broken connection otherwise stays in the pool and every request on it
  has to exhaust `request_timeout`. Linux only.
- `greptimedb_stream:finish/2` is now exported, so a caller can pass its own
  result wait timeout without changing the stream's gRPC deadline.

### Fixed

- Pending async batches still flush after health checks and other worker calls.
  Each async enqueue resets the independent 20 ms linger timer (#56).
- `connect_timeout` is now actually applied. It was only accepted inside
  `grpc_opts`, which grpcbox does not consult for the connect timeout, so the
  value was dropped and chatterbox fell back to its own default. The option is
  still accepted there for compatibility, with the dedicated option taking
  precedence.
- `is_alive/1` no longer reports a `gen_server` timeout in place of the real gRPC
  error. The caller and the gRPC deadline were both 1000 ms, so the caller always
  gave up first. Every caller now waits 2000 ms beyond the deadline it set,
  `greptimedb_stream:finish/1` included.
- A caller can no longer give up before the deadline it set, and an
  `async_write` no longer runs on a deadline other than the one its caller
  asked for. `request_timeout` and `health_check_timeout` travel with each
  request, including through the async batching queue, where a batch now only
  groups requests with the same `request_timeout`. They used to be read separately
  by the caller and by the worker, which disagreed whenever a pool was started a
  second time with different options.
- `request_timeout` now bounds an async write end to end. A queued request was
  given a fresh full timeout when its batch started, so it could be sent after
  the deadline its caller asked for, and one that expired behind a live entry
  stayed queued instead of failing with `{error, timeout}`. The wait for the
  batch's result is measured against the same deadline, so opening the stream
  and sending no longer buy it extra time on top.
- `grpc_hints` are now sent on streaming writes. `write_stream/1` built its
  context without the hint header, so table options such as `ttl` and
  `append_mode` were silently dropped on tables created through a stream.
- Recover a stale gRPC channel left behind when a pool worker terminates before
  its channel is stopped (#58).

### Changed

- CI runs against GreptimeDB v1.2.1.
- Bumped grpcbox to 0.18.0 (pulling in ts_chatterbox 0.16.0) and ecpool to 0.6.4.
  ecpool 0.6.4 marks the pool supervisors `permanent`, so a crashed pool is
  restarted instead of staying down.
- `greptimedb_worker` takes the resolved timeouts from its callers:
  `handle/2`, `health_check/1`, `stream/1` and `async_handle/3` became `handle/3`,
  `health_check/2`, `stream/2` and `async_handle/4`. The `greptimedb` and
  `greptimedb_stream` APIs are unchanged.
