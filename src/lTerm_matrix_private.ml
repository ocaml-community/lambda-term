(*
 * lTerm_matrix_private.ml
 * -----------------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 *)

open StdLabels
module Geom = LTerm_geom

type point =
  { mutable char       : Uchar.t
  ; mutable char_trail : Uchar.t list
  ; mutable switches   : LTerm_style.Switches.t
  ; mutable foreground : LTerm_style.Color.t
  ; mutable background : LTerm_style.Color.t
  }

type t =
  { data                              : point array array
  ; size                              : Geom.size
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

let size t = t.size
let context t = t.main_context
let context_with_hidden_newlines t = t.main_context_with_hidden_newlines

let create (size : Geom.size) =
  let create_point _ =
    { char       = Uchar.of_char ' '
    ; char_trail = []
    ; switches   = LTerm_style.Switches.default
    ; foreground = LTerm_style.Color.default
    ; background = LTerm_style.Color.default
    }
  in
  let data =
    Array.init size.rows ~f:(fun _ ->
      Array.init (size.cols + 1) ~f:create_point)
  in
  { size
  ; data
  ; main_context =
      { matrix          = data
      ; row1            = 0
      ; col1            = 0
      ; row2            = size.rows
      ; col2            = size.cols
      ; hidden_newlines = false
      }
  ; main_context_with_hidden_newlines =
      { matrix          = data
      ; row1            = 0
      ; col1            = 0
      ; row2            = size.rows
      ; col2            = size.cols
      ; hidden_newlines = true
      }
  }
;;

let resize t (size : Geom.size) =
  if size = t.size then
    t
  else begin
    let res = create size in
    for row = 0 to min t.size.rows size.rows - 1 do
      for col = 0 to min t.size.cols size.cols do
        let old_pt = t.  data.(row).(col) in
        let new_pt = res.data.(row).(col) in
        new_pt.char       <- old_pt.char;
        new_pt.switches   <- old_pt.switches;
        new_pt.foreground <- old_pt.foreground;
        old_pt.background <- old_pt.background;
      done
    done;
    res
  end
;;
