(*
 * lt_unix.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Unix specific functions *)

val sigwinch : int option
  (** The number of the signal used to indicate that the terminal size
      have changed. It is [None] on windows. *)

val get_sequence : Text.t Lwt_stream.t -> Text.t Lwt.t
  (** [get_sequence stream] reads an escape sequence or a character
      from the given stream of characters. *)

val parse_event : Text.t -> Lt_event.t
  (** [parse_event sequence] returns the event that correspond to the
      given escape sequence. *)
