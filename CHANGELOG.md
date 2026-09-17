# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Configurable timeouts as client options: `connect_timeout` (default `5000`),
  `request_timeout` (default `10000`) and `health_check_timeout` (default `1000`),
  all in milliseconds. The defaults match the values that were hardcoded before.
- `tcp_user_timeout` client option (default `0`, disabled), set as `TCP_USER_TIMEOUT`
  on the connection. The client sends neither TCP keepalive nor HTTP/2 pings, so
  a silently broken connection otherwise stays in the pool and every request on it
  has to exhaust `request_timeout`. Linux only.
- `greptimedb_stream:finish/2` is now exported, so a caller can pass its own
  timeout instead of the client's `request_timeout`.

### Fixed

- `connect_timeout` is now actually applied. It was only accepted inside
  `grpc_opts`, which grpcbox does not consult for the connect timeout, so the
  value was dropped and chatterbox fell back to its own default. The option is
  still accepted there for compatibility, with the dedicated option taking
  precedence.
- `is_alive/1` no longer reports a `gen_server` timeout in place of the real gRPC
  error. The caller and the gRPC deadline were both 1000 ms, so the caller always
  gave up first; callers now wait 2000 ms beyond the deadline.
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
