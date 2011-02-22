(*
 * lt_key.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type t =
  | Char of Text.t
  | Enter
  | Escape
  | Tab
  | Up
  | Down
  | Left
  | Right
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Next_page
  | Prev_page
  | Home
  | End
  | Insert
  | Delete
  | Backspace

type modifiers = {
  control : bool;
  meta : bool;
}

let to_string = function
  | Char ch -> Printf.sprintf "Char %S" ch
  | Enter -> "Enter"
  | Escape -> "Escape"
  | Tab -> "Tab"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | F1 -> "F1"
  | F2 -> "F2"
  | F3 -> "F3"
  | F4 -> "F4"
  | F5 -> "F5"
  | F6 -> "F6"
  | F7 -> "F7"
  | F8 -> "F8"
  | F9 -> "F9"
  | F10 -> "F10"
  | F11 -> "F11"
  | F12 -> "F12"
  | Next_page -> "Next_page"
  | Prev_page -> "Prev_page"
  | Home -> "Home"
  | End -> "End"
  | Insert -> "Insert"
  | Delete -> "Delete"
  | Backspace -> "Backspace"

let string_of_modifiers m =
  Printf.sprintf "{ control = %B; meta = %B }" m.control m.meta
