## Release v0.16.0
- Update function signatures:
  - `Server.create` uses `(Websocket.t -> unit Deferred.t)` instead of `(string Pipe.Reader.t -> string Pipe.Writer.t -> unit Deferred.t)`
  - `Client.create` and `Client.with_websocket_client` use `Websocket.t` instead of `string Pipe.Reader.t` and `string Pipe.Writer.t`

- Add new parameters to `Cohttp_async_websocket.Client` functions:
  - `bind_to_address` in `Client.create` allows binding to a specific address when creating a WebSocket client
  - `opcode` in `Client.create` and `Client.with_websocket_client` enables specifying the desired WebSocket frame opcode (`Text` or `Binary`)
