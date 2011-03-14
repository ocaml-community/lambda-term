(*
 * lt_event.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type t =
  | Resize of Lt_geom.size
  | Key of Lt_key.t
  | Sequence of string
  | Mouse of Lt_mouse.t

let to_string = function
  | Resize size ->
      Printf.sprintf "Resize %s" (Lt_geom.string_of_size size)
  | Key key ->
      Printf.sprintf "Key %s" (Lt_key.to_string key)
  | Sequence seq ->
      Printf.sprintf "Sequence %S" seq
  | Mouse mouse ->
      Printf.sprintf "Mouse %s" (Lt_mouse.to_string mouse)
