open! Core
open! Async
open! Import
include module type of Cohttp.Header

val origin_and_host_match
  :  ?ignore_port:bool (* If set, only check hostname not the port (default: false) *)
  -> t
  -> unit Or_error.t

val origin_matches_host_or_is_one_of
  :  ?ignore_port:bool (* If set, only check hostname not the port (default: false) *)
  -> t
  -> origins:string list
       (* Allowlist entries must have the form scheme://host or scheme://host:port *)
  -> unit Or_error.t

(** Get the Sec-Websocket-Protocol entries in the header *)
val websocket_subprotocols : t -> string list

(** Add a Sec-Websocket-Protocol entry to the header *)
val add_websocket_subprotocol : t -> subprotocol:string -> t
