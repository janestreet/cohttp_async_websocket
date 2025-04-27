open! Core
open! Async
open! Import
module Header = Header

type http_handler :=
  body:Body.t -> Socket.Address.Inet.t -> Request.t -> Server.response Deferred.t

type raw_http_handler :=
  body:Body.t -> Socket.Address.Inet.t -> Request.t -> Server.response_action Deferred.t

module Server : sig
  module On_connection : sig
    (** Represents the policy a server should use to when establishing a websocket
        connection response. [On_connection] is used to determine what headers to respond
        with, and how to handle the websocket bytes. *)
    type 'msg t

    (** [create ?set_response_headers ?should_overwrite_sec_accept_header f] allows one to
        choose response headers and byte processing for the connection.

        [set_response_headers] will seed the default header values for the http response.

        [should_overwrite_sec_accept_header] is used to determine if cohttp should be
        allowed to overwrite the [sec-accept] header value, even if that value was present
        in [set_response_headers].

        [f] is given a pipe version of the reader and writer to the websocket client; no
        websocket framing is expected of [f]. *)
    val create
      :  ?set_response_headers:Header.t (** Default: empty *)
      -> ?should_overwrite_sec_accept_header:bool (** Default: true *)
      -> ('msg Websocket.t -> unit Deferred.t)
      -> 'msg t
  end

  type 'msg websocket_handler =
    inet:Socket.Address.Inet.t
    -> subprotocol:string option
    -> Cohttp.Request.t
    -> 'msg On_connection.t Deferred.t

  val create
    :  non_ws_request:http_handler
    -> ?opcode:[ `Text | `Binary ]
         (** [should_process_request] allows a caller to guard any http handling or
             websocket handling from occurring. The default ignores the address and
             invokes [Header.origin_and_host_match] if [is_websocket_request], otherwise
             it returns [Ok ()]. All websocket requests should perform validation on the
             origin header field for security purposes.

             As an example, in the case of a web server which only wants to allow
             websocket requests that originate from itself, or "https://some-client":
             {[
               let origins = [ "https://some-client" ] in
               let should_process_request
                     (_ : Socket.Address.Inet.t)
                     header
                     ~is_websocket_request
                 =
                 if is_websocket_request then
                   Cohttp.Header.origin_matches_host_or_is_one_of header ~origins
                 else Ok ()
             ]} *)
    -> ?should_process_request:
         (Socket.Address.Inet.t
          -> Header.t
          -> is_websocket_request:bool
          -> unit Deferred.Or_error.t)
         (** [websocket_subprotocol_selection] allows the server to pick the protocol to
             use for a websocket request once [should_process_request] has accepted the
             connection. The subprotocol selected is sent back to the client as part of
             the 'Sec-Websocket-Protocol' header, and included in the [subprotocol]
             argument of the [websocket_handler]. *)
    -> ?websocket_subprotocol_selection:(Request.t -> [ `Subprotocol of string option ])
    -> string websocket_handler
    -> raw_http_handler

  (** Similar to [create] but
      - takes a [raw_http_handler] instead for [~non_ws_request]. This can be useful when
        kerberos is not handled for websockets and only on some raw HTTP URLs; and
      - takes a [Websocket.Frame_content.t websocket_handler] that has control over frame
        types in addition to content bytes. *)
  val create'
    :  non_ws_request:raw_http_handler
    -> ?should_process_request:
         (Socket.Address.Inet.t
          -> Header.t
          -> is_websocket_request:bool
          -> unit Deferred.Or_error.t)
    -> ?websocket_subprotocol_selection:(Request.t -> [ `Subprotocol of string option ])
    -> Websocket.Frame_content.t websocket_handler
    -> raw_http_handler
end

module Client : sig
  (** [create endpoint] creates a websocket client connected to [endpoint]. In order to
      close the connection, close the writer pipe.

      [force_ssl_overriding_SNI_hostname] will unconditionally cause [create] to use
      TLS/SSL, and will send its value as the SNI hostname. *)
  val create
    :  ?bind_to_address:Unix.Inet_addr.t
    -> ?force_ssl_overriding_SNI_hostname:string
    -> ?opcode:[ `Text | `Binary ]
    -> ?headers:Header.t
    -> Uri.t
    -> (Response.t * string Websocket.t) Deferred.Or_error.t

  (** [with_websocket_client uri ~f] applies [f] to the result of [create uri], and closes
      the connection once the result of [f] becomes determined. *)
  val with_websocket_client
    :  ?opcode:[ `Text | `Binary ]
    -> ?headers:Header.t
    -> Uri.t
    -> f:(Response.t -> string Websocket.t -> 'a Deferred.t)
    -> 'a Deferred.Or_error.t

  (** Similar to [with_websocket_client] but handles
      [Websocket.Frame_content.t websocket_handler] instead of content bytes of frames *)
  val with_websocket_client'
    :  ?headers:Header.t
    -> Uri.t
    -> f:(Response.t -> Websocket.Frame_content.t Websocket.t -> 'a Deferred.t)
    -> 'a Deferred.Or_error.t
end
