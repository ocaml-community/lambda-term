(*
 * lt_draw.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Drawing *)

(** Type of a point in a matrix of styled characters. *)
type point = {
  mutable char : int;
  (** The unicode character. *)
  mutable bold : bool;
  (** Whether the character is in bold or not. *)
  mutable underlined : bool;
  (** Whether the character is underlined or not. *)
  mutable blink : bool;
  (** Whether the character is blinking or not. *)
  mutable foreground : Lt_style.color;
  (** The foreground color. *)
  mutable background : Lt_style.color;
  (** The background color. *)
}

type matrix = point array array
    (** Type of a matrix of points. The matrix is indexed by lines
        then columns, i.e. to access the point at line [l] and column
        [c] in matrix [m] you should use [m.(l).(c)]. *)

val make_matrix : Lt_types.size -> matrix
  (** [matrix size] creates a matrix of the given size containing only
      blank characters. *)
