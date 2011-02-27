(*
 * lt_event.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type size = { lines : int; columns : int }

type t =
  | Resize of size
  | Key of Lt_key.t
  | Sequence of string
  | Mouse of int * int * int

let to_string = function
  | Resize size ->
      Printf.sprintf "Resize { lines = %d; columns = %d }" size.lines size.columns
  | Key key ->
      Printf.sprintf "Key %s" (Lt_key.to_string key)
  | Sequence seq ->
      Printf.sprintf "Sequence %S" seq
  | Mouse(mask, x, y) ->
      Printf.sprintf "Mouse(%x, %d, %d)" mask x y
