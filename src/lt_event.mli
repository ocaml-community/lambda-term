(*
 * lt_event.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Events *)

type size = { lines : int; columns : int }
    (** Type of terminal sizes. *)

(** Event from the terminal. *)
type t =
  | Resize of size
      (** The terminal has been resized. *)
  | Key of Lt_key.modifiers * Lt_key.t
      (** A key has been pressed. *)
  | Key_sequence of Text.t
      (** An unknown escape sequence. *)

val to_string : t -> string
  (** [to_text event] returns the string representation of the given
      event. *)
