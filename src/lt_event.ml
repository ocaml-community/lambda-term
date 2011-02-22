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
  | Key of Lt_key.modifiers * Lt_key.t
  | Key_sequence of Text.t

let to_string = function
  | Resize { lines; columns } ->
      Printf.sprintf "Resize { lines = %d; columns = %d }" lines columns
  | Key(mods, key) ->
      Printf.sprintf "Key(%s, %s)" (Lt_key.string_of_modifiers mods) (Lt_key.to_string key)
  | Key_sequence seq ->
      Format.sprintf "Key_sequence %S" seq

