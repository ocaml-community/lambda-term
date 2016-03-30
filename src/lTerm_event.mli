(*
 * lTerm_event.mli
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Events *)

(** The representation of events is optimized for pattern-matching. *)

module Modifiers : sig
  (** Modifiers: Control, Meta and Shift *)
  type t =
    | N
    | C
    | M
    | C_M
    | S
    | C_S
    | M_S
    | C_M_S

  val set_control : t -> bool -> t
  val set_meta    : t -> bool -> t
  val set_shift   : t -> bool -> t

  val control : t -> bool
  val meta    : t -> bool
  val shift   : t -> bool

  val make : control:bool -> meta:bool -> shift:bool -> t
end

module Key : sig
  type t =
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
    | Next
    | Prev
    | Home
    | End
    | Insert
    | Delete
    | Backspace
end

module Signal : sig
  type t =
    | Intr (** Ctrl+C *)
    | Quit (** Ctrl+Q *)
    | Susp (** Ctrl+Z *)
end

(** The [Text] case could be split in multiple [Char] or [Uchar] events but this turn out
    to be quite slow when pasting text with the mouse. *)
type t =
  | Text        of string (** Text without escape sequence (including newlines) *)
  | Char        of Modifiers.t * char (** Ascii characters *)
  | Uchar       of Modifiers.t * Uchar.t
  | Key         of Modifiers.t * Key.t
  | Sequence    of string
  | Button_down of Modifiers.t * int * LTerm_geom.coord (** Mouse button pressed  *)
  | Button_up   of Modifiers.t * int * LTerm_geom.coord (** Mouse button released *)
  | Signal      of Signal.t (** Signal received             *)
  | Resume                  (** Resuming from a TSTP signal *)
  | Resize
  | Closed

val to_string : t -> string
val of_string : string -> t

val with_coord   : t -> LTerm_geom.coord -> t
val remove_coord : t -> t
