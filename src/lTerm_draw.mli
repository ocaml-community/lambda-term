(*
 * lTerm_draw.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Drawing *)

open Zed
open LTerm_geom

(** Type of a point in a matrix of styled characters. *)
type point =
  { mutable char  : Uchar.t
  ; mutable style : LTerm_style.t
  } [@@deriving sexp]

type matrix = point array array [@@deriving sexp]
    (** Type of a matrix of points. The matrix is indexed by lines
        then columns, i.e. to access the point at line [l] and column
        [c] in matrix [m] you should use [m.(l).(c)]. *)

val make_matrix : size -> matrix
  (** [matrix size] creates a matrix of the given size containing only
      blank characters. *)

val set_style : point -> style:LTerm_style.t -> unit

(** In all the following functions, the [style] argument defaults to [LTerm_style.none] *)

type context [@@deriving sexp_of]
  (** Type of contexts. A context is used for drawing. *)

val context : matrix -> size -> context
  (** [context m s] creates a context from a matrix [m] of size
      [s]. It raises [Invalid_argument] if [s] is not the size of
      [m]. *)

exception Out_of_bounds
  (** Exception raised when trying to access a point that is outside
      the bounds of a context. *)

val size : context -> size
  (** [size ctx] returns the size of the given context. *)

val matrix : context -> matrix

val sub : context -> rect -> context
  (** [sub ctx rect] creates a sub-context from the given context. It
      raises {!Out_of_bounds} if the rectangle is not contained in the
      given context. *)

val clear : context -> unit
  (** [clear ctx] clears the given context. It resets all styles to
      their default and sets characters to spaces. *)

val fill
  :  context
  -> ?style:LTerm_style.t
  -> Uchar.t
  -> unit

val fill_style
  :  context
  -> style:LTerm_style.t
  -> unit

val point : context -> row:int -> col:int -> point
  (** [point ctx row column] returns the point at given position in
      [ctx]. It raises {!Out_of_bounds} if the coordinates are outside
      the given context. *)

val draw_char
  :  context
  -> row:int
  -> col:int
  -> ?style:LTerm_style.t
  -> Uchar.t
  -> unit

module type Text_drawing = sig
  type t

  val draw
    :  context
    -> row:int
    -> col:int
    -> ?style:LTerm_style.t
    -> t
    -> unit

  val draw_aligned
    :  context
    -> row:int
    -> align:Horz_alignment.t
    -> ?style:LTerm_style.t
    -> t
    -> unit
end

module UTF8 : Text_drawing with type t = Zed_utf8.t
module Text : Text_drawing with type t = LTerm_text.t
module Latin1 : Text_drawing with type t = String.t

(** Type of an connection in a piece that can be connected to other
    pieces. *)
module Connection : sig
  type t =
    | Blank  (** No connection. *)
    | Light  (** Connection with a light line. *)
    | Heavy  (** Connection with a heavy line. *)
    | Double (** Connection with a double line. *)
  [@@deriving sexp]
end

(** Type of a piece, given by its four Connection.t. *)
module Piece : sig
  type t [@@deriving sexp]

  (** exchange top-bottom and left-right *)
  val reverse : t -> t

  val top    : t -> Connection.t
  val bottom : t -> Connection.t
  val left   : t -> Connection.t
  val right  : t -> Connection.t

  val set_top    : t -> Connection.t -> t
  val set_bottom : t -> Connection.t -> t
  val set_left   : t -> Connection.t -> t
  val set_right  : t -> Connection.t -> t

  val make
    :  top:   Connection.t
    -> bottom:Connection.t
    -> left:  Connection.t
    -> right: Connection.t
    -> t

  val hline : Connection.t -> t
  val vline : Connection.t -> t
  val br : Connection.t -> t
  val bl : Connection.t -> t
  val tr : Connection.t -> t
  val tl : Connection.t -> t

  (** Not all combinations are supported *)
  val to_char : t -> Uchar.t
end

val draw_piece
  :  context
  -> row:int
  -> col:int
  -> ?style:LTerm_style.t
  -> Piece.t
  -> unit
(** Draws a piece. It may modify pieces around it. *)

val draw_hline
  :  context
  -> row:int
  -> col:int
  -> len:int
  -> ?style:LTerm_style.t
  -> Connection.t
  -> unit

val draw_vline
  :  context
  -> row:int
  -> col:int
  -> len:int
  -> ?style : LTerm_style.t
  -> Connection.t
  -> unit

val draw_frame
  :  context
  -> rect
  -> ?style : LTerm_style.t
  -> Connection.t
  -> unit
