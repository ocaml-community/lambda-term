(*
 * lt_style.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* +-----------------------------------------------------------------+
   | Colors                                                          |
   +-----------------------------------------------------------------+ *)

type color =
  | Default
  | Index of int
  | Light of int
  | RGB of int * int * int

let black = Index 0
let red = Index 1
let green = Index 2
let yellow = Index 3
let blue = Index 4
let magenta = Index 5
let cyan = Index 6
let white = Index 7
let lblack = Light 0
let lred = Light 1
let lgreen = Light 2
let lyellow = Light 3
let lblue = Light 4
let lmagenta = Light 5
let lcyan = Light 6
let lwhite = Light 7

(* +-----------------------------------------------------------------+
   | Styled text                                                     |
   +-----------------------------------------------------------------+ *)

type item =
  | String of string
  | Reset
  | Bold
  | Underlined
  | Blink
  | Inverse
  | Hidden
  | Foreground of color
  | Background of color

type text = item list

let format fmt = Printf.ksprintf (fun str -> String str) fmt

let strip text =
  let rec loop acc = function
    | [] ->
        Lt_utf8.rev_concat "" acc
    | String str :: rest ->
        loop (str :: acc) rest
    | _ :: rest ->
        loop acc rest
  in
  loop [] text
