(*
 * lTerm_draw.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Drawing *)

open LTerm_geom

type context
(** Type of a drawing contexts. *)

exception Out_of_bounds
(** Exception raised when trying to access a point that is outside the bounds of a
    context. *)

(** Underlying storage *)
module Matrix : sig
  type t = private LTerm_matrix_private.t

  val size : t -> size

  val create  : size -> t
  (** Create a new matrix of the given size *)

  val resize  : t -> size -> t
  (** Resize a matrix, if necessary. If the size is unchanged, it returns the original
      matrix, otherwise a new one where the intersection of the old and new one is
      preserved.

      In any case, you shouldn't use the original matrix after that. *)

  val context : t -> context

  val context_with_hidden_newlines : t -> context
  (** Same as {!context} except that it accepts "hidden newlines" at the edge of the
      screen. In this context you can add a hidden newline at the end of any row.

      This is for the case where you want to print text ending with a newline character
      and the text ends at the right border.

      For instance with a terminal with 6 columns:

      {[
        abcdef
        123456
      ]}

      Without a hidden newline at the end of [abcdef], selecting the two lines with the
      mouse will select "abcdef123456". With a hidden newlines it will select
      "abcdef\n123456". *)
end

val size : context -> size
(** [size ctx] returns the size of the given context. *)

val sub : context -> rect -> context
(** [sub ctx rect] creates a sub-context from the given context. It raises
    {!Out_of_bounds} if the rectangle is not contained in the given context.

    The new context doesn't support hidden newlines, as this only make sense for the main
    context.
*)

val clear : context -> unit
(** [clear ctx] clears the given context. It resets all styles to their default and sets
    characters to spaces. *)

(** In all the following functions, the [style] argument defaults to [LTerm_style.none] *)

val set
  :  context
  -> row:int
  -> col:int
  -> ?style:LTerm_style.t
  -> Uchar.t
  -> unit

val set_style : context -> row:int -> col:int -> LTerm_style.t -> unit

val set_hidden_newline : context -> row:int -> bool -> unit
(** Raises [Invalid_argument] if the context doesn't support hidden newlines *)

val get
  :  context
  -> row:int
  -> col:int
  -> Uchar.t

val get_style
  :  context
  -> row:int
  -> col:int
  -> LTerm_style.t

val fill
  :  context
  -> ?style:LTerm_style.t
  -> Uchar.t
  -> unit

val fill_style
  :  context
  -> style:LTerm_style.t
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
  (** Draw some text on the given context. This handles hidden newlines properly on the
      main context. *)

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
