(*
 * lt_geom.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type size = {
  lines : int;
  columns : int;
}

let lines size = size.lines
let columns size = size.columns

let string_of_size size =
  Printf.sprintf "{ lines = %d; columns = %d }" size.lines size.columns

type coord = {
  line : int;
  column : int;
}

let line size = size.line
let column size = size.column

let string_of_coord coord =
  Printf.sprintf "{ line = %d; column = %d }" coord.line coord.column

type rect = {
  r_line : int;
  r_column : int;
  r_lines : int;
  r_columns : int;
}

let r_line rect = rect.r_line
let r_column rect = rect.r_column
let r_lines rect = rect.r_lines
let r_columns rect = rect.r_columns

let string_of_rect rect =
  Printf.sprintf
    "{ r_line = %d; r_column = %d; r_lines = %d; r_columns = %d }"
    rect.r_line rect.r_column rect.r_lines rect.r_columns

type horz_alignment =
  | H_align_left
  | H_align_center
  | H_align_right

type vert_alignment =
  | V_align_top
  | V_align_center
  | V_align_bottom
