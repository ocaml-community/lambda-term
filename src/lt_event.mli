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
  | Key of Lt_key.t
      (** A key has been pressed. *)
  | Sequence of string
      (** An uninterpreted escape sequence. *)
  | Mouse of Lt_mouse.t
      (** A mouse button has been pressed. *)

val to_string : t -> string
  (** [to_string event] returns the string representation of the given
      event. *)
