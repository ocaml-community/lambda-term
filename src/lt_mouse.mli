(*
 * lt_mouse.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Mouse events *)

(** Type of mouse button. *)
type button =
  | Button1
  | Button2
  | Button3
  | Button4
  | Button5
  | Button6
  | Button7
  | Button8
  | Button9

(** Type of mouse click event. *)
type t = {
  control : bool;
  (** Is the control key down ? *)
  meta : bool;
  (** Is the meta key down ? *)
  button : button;
  (** Which button have been pressed ? *)
  line : int;
  (** The line at which the mouse was when the button has been
      pressed. *)
  column : int;
  (** The column at which the mouse was when the button has been
      pressed. *)
}

val control : t -> bool
val meta : t -> bool
val button : t -> button
val line : t -> int
val column : t -> int

val to_string : t -> string
  (** Returns the string representation of the given mouse event. *)
