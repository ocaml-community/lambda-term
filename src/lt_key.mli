(*
 * lt_key.mli
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Keys *)

(** Type of key code. *)
type code =
  | Char of int
      (** A unicode character. *)
  | Enter
  | Escape
  | Tab
  | Up
  | Down
  | Left
  | Right
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Next_page
  | Prev_page
  | Home
  | End
  | Insert
  | Delete
  | Backspace

(** Type of key. *)
type t = {
  ctrl : bool;
  (** Is the control key down ? *)
  meta : bool;
  (** Is the meta key down ? *)
  code : code;
  (** The code of the key. *)
}

val ctrl : t -> bool
val meta : t -> bool
val code : t -> code

val to_string : t -> string
  (** Returns the string representation of the given key. *)
