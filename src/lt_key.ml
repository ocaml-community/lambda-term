(*
 * lt_key.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type code =
  | Char of int
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

type t = {
  control : bool;
  meta : bool;
  shift : bool;
  code : code;
}

let control key = key.control
let meta key = key.meta
let code key = key.code

let string_of_code = function
  | Char ch -> Printf.sprintf "Char 0x%02x" ch
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

let to_string key =
  Printf.sprintf "{ control = %B; meta = %B; shift = %B; code = %s }" key.control key.meta key.shift (string_of_code key.code)
