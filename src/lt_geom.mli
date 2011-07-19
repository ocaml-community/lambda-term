(*
 * lt_geom.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Common types. *)

(** Type of sizes. *)
type size = {
  lines : int;
  columns : int;
}

val lines : size -> int
val columns : size -> int

val string_of_size : size -> string
  (** Returns the string representation of the given size. *)

(** Type of coordinates. *)
type coord = {
  line : int;
  column : int;
}

val line : coord -> int
val column : coord -> int

val string_of_coord : coord -> string
  (** Returns the string representation of the given coordinates. *)

(** Type of rectangles. *)
type rect = {
  r_line : int;
  r_column : int;
  r_lines : int;
  r_columns : int;
}

val r_line : rect -> int
val r_column : rect -> int
val r_lines : rect -> int
val r_columns : rect -> int

val size_of_rect : rect -> size
  (** Returns the size of a rectangle. *)

val string_of_rect : rect -> string
  (** Returns the string representation of the given rectangle. *)

(** Horizontal alignment. *)
type horz_alignment =
  | H_align_left
  | H_align_center
  | H_align_right

(** Vertical alignement. *)
type vert_alignment =
  | V_align_top
  | V_align_center
  | V_align_bottom
