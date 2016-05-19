(*
 * lTerm_matrix_private.mli
 * ------------------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 *)

type point =
  { mutable char       : Uchar.t
  ; mutable char_trail : Uchar.t list
  ; mutable switches   : LTerm_style.Switches.t
  ; mutable foreground : LTerm_style.Color.t
  ; mutable background : LTerm_style.Color.t
  }

type t =
  { data                              : point array array
  ; size                              : LTerm_geom.size
  ; main_context                      : context
  ; main_context_with_hidden_newlines : context
  }

and context =
  { matrix          : point array array
  ; row1            : int
  ; col1            : int
  ; row2            : int
  ; col2            : int
  ; hidden_newlines : bool
  }

val size : t -> LTerm_geom.size
val create : LTerm_geom.size -> t
val resize : t -> LTerm_geom.size -> t
val context : t -> context
val context_with_hidden_newlines : t -> context
