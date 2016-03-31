(*
 * lTerm_unix.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Unix specific functions *)

val sigwinch : int option
(** The number of the signal used to indicate that the terminal size have changed. It is
    [None] on windows. *)

val system_encoding : string
(** The encoding used by the system. *)

module Event_parser : sig
  (** Parsing of escape sequence in input.

      For non-ASCII characters, UTF-8 encoding is assumed. Invalid UTF-8 sequences are
      treated as Latin-1. *)

  type t

  (** [escape_time] is the time waited before returning the escape key. *)
  val create : Unix.file_descr -> escape_time:float -> t

  val escape_time     : t -> float
  val set_escape_time : t -> float -> unit

  (** Discard all pending characters in the internal buffer. *)
  val discard : t -> unit

  (** Reads at least one event from the given [t]. This is a blocking operation, which in
      particular might sleep for [escape_time] it it needs to after reading the escape
      character.

      When the parser is disabled, this function will eventually return [None], except if
      it re-enabled in the meantime.

      Calls to read for a given [t] must be serialized. *)
  val read : t -> LTerm_event.t list

  (** Enable/disable the event parser *)
  val set_active : t -> bool -> unit
end
