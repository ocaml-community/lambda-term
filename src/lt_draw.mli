(*
 * lt_draw.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Drawing *)

open CamomileLibrary
open Lt_types

(** Type of a point in a matrix of styled characters. *)
type point = {
  mutable char : UChar.t;
  (** The unicode character. *)
  mutable bold : bool;
  (** Whether the character is in bold or not. *)
  mutable underline : bool;
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

type context
  (** Type of contexts. A context is used for drawing. *)

val context : matrix -> Lt_types.size -> context
  (** [context m s] creates a context from a matrix [m] of size
      [s]. It raises [Invalid_argument] if [s] is not the size of
      [m]. *)

exception Out_of_bounds
  (** Exception raised when trying to access a point that is outside
      the bounds of a context. *)

val size : context -> size
  (** [size ctx] returns the size of the given context. *)

val sub : context -> rect -> context
  (** [sub ctx rect] creates a sub-context from the given context. It
      raises {!Out_of_bounds} if the rectangle is not contained in the
      given context. *)

val clear : context -> unit
  (** [clear ctx] clears the given context. It resets all styles to
      their default and sets characters to spaces. *)

val fill : context -> UChar.t -> unit
  (** [fill ctx ch] fills the given context with [ch]. This does not
      affect styles. *)

val point : context -> int -> int -> point
  (** [point ctx line column] returns the point at given position in
      [ctx]. It raises {!Out_of_bounds} if the coordinates are outside
      the given context. *)

val draw_char : context -> int -> int -> UChar.t -> unit
  (** [draw_char ctx line column ch] sets the character at given
      coordinates to [ch]. This does not affect styles. It does
      nothing if the given coordinates are outside the bounds of the
      context. *)

val draw_string : context -> int -> int -> string -> unit
  (** [draw_string ctx line column str] draws the given string at
      given coordinates. This does not affect styles. [str] may
      contains newlines. *)

val draw_styled : context -> int -> int -> Lt_style.text -> unit
  (** [draw_styled ctx line column text] draws the given styled text
      at given coordinates. *)
