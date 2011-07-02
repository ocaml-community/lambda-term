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

val system_encoding : string
  (** The encoding used by the system. *)

val parse_event : ?escape_time : float -> Lt_iconv.t -> char Lwt_stream.t -> Lt_event.t Lwt.t
  (** [parse_event cd stream] parses one event from the given input
      stream. [cd] is the conversion descriptor used to decode
      non-ascii characters. It must be a converter from the stream
      encoding to "UCS-4BE". If an invalid sequence is encountered in
      the input, it fallbacks to Latin-1. [escape_time] is the time
      waited before returning the escape key. It defaults to [0.1]. *)
