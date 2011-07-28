(*
 * lTerm_draw.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Drawing *)

open CamomileLibrary
open LTerm_geom

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
  mutable reverse : bool;
  (** Whether the character is in reverse video mode or not. *)
  mutable foreground : LTerm_style.color;
  (** The foreground color. *)
  mutable background : LTerm_style.color;
  (** The background color. *)
}

type matrix = point array array
    (** Type of a matrix of points. The matrix is indexed by lines
        then columns, i.e. to access the point at line [l] and column
        [c] in matrix [m] you should use [m.(l).(c)]. *)

val make_matrix : LTerm_geom.size -> matrix
  (** [matrix size] creates a matrix of the given size containing only
      blank characters. *)

val set_style : point -> LTerm_style.t -> unit
  (** [set_style point style] sets fields of [point] according to
      fields of [style]. For example:

      {[
        set_style point { LTerm_style.none with LTerm_style.bold = Some true }
      ]}

      will have the following effect:

      {[
        point.bold <- true
      ]}
  *)

type context
  (** Type of contexts. A context is used for drawing. *)

val context : matrix -> LTerm_geom.size -> context
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

val fill : context -> ?style : LTerm_style.t -> UChar.t -> unit
  (** [fill ctx ch] fills the given context with [ch]. *)

val fill_style : context -> LTerm_style.t -> unit
  (** [fill_style style] fills the given context with [style]. *)

val point : context -> int -> int -> point
  (** [point ctx row column] returns the point at given position in
      [ctx]. It raises {!Out_of_bounds} if the coordinates are outside
      the given context. *)

val draw_char : context -> int -> int -> ?style : LTerm_style.t -> UChar.t -> unit
  (** [draw_char ctx row column ?style ch] sets the character at given
      coordinates to [ch]. It does nothing if the given coordinates
      are outside the bounds of the context. *)

val draw_string : context -> int -> int -> ?style : LTerm_style.t -> string -> unit
  (** [draw_string ctx row column ?style str] draws the given string
      at given coordinates. This does not affect styles. [str] may
      contains newlines. *)

val draw_styled : context -> int -> int -> ?style : LTerm_style.t -> LTerm_text.t -> unit
  (** [draw_styled ctx row column ?style text] draws the given styled
      text at given coordinates. *)

val draw_string_aligned : context -> int -> horz_alignment -> ?style : LTerm_style.t -> string -> unit
  (** Draws a string with the given alignment. *)

val draw_styled_aligned : context -> int -> horz_alignment -> ?style : LTerm_style.t -> LTerm_text.t -> unit
  (** Draws a styled string with the given aglienment. *)

(** Type of an connection in a piece that can be connected to other
    pieces. *)
type connection =
  | Blank
      (** No connection. *)
  | Light
      (** Connection with a light line. *)
  | Heavy
      (** Connection with a heavy line. *)

type piece = { top : connection; bottom : connection; left : connection; right : connection }
    (** Type of a piece, given by its four connection. *)

val draw_piece : context -> int -> int -> ?style : LTerm_style.t -> piece -> unit
  (** Draws a piece. It may modify pieces around it. *)

val draw_hline : context -> int -> int -> int -> ?style : LTerm_style.t -> connection -> unit
  (** [draw_hline ctx row column length connection] draws an
      horizontal line. *)

val draw_vline : context -> int -> int -> int -> ?style : LTerm_style.t -> connection -> unit
  (** [draw_hline ctx row column length connection] draws a vertical
      line. *)

val draw_frame : context -> rect -> ?style : LTerm_style.t -> connection -> unit
  (** Draws a rectangle. *)
