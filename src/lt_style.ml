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
  | RGB of int * int * int

let default = Default
let index n = Index n
let rgb r g b =
  if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
    invalid_arg "Lt_style.rgb"
  else
    RGB(r, g, b)

let black = Index 0
let red = Index 1
let green = Index 2
let yellow = Index 3
let blue = Index 4
let magenta = Index 5
let cyan = Index 6
let white = Index 7
let lblack = Index 8
let lred = Index 9
let lgreen = Index 10
let lyellow = Index 11
let lblue = Index 12
let lmagenta = Index 13
let lcyan = Index 14
let lwhite = Index 15

(* +-----------------------------------------------------------------+
   | Styled text                                                     |
   +-----------------------------------------------------------------+ *)

type item =
  | String of string
  | Reset
  | Bold
  | Underline
  | Blink
  | Inverse
  | Hide
  | Foreground of color
  | Background of color

type text = item list

let format fmt = Printf.ksprintf (fun str -> String str) fmt

let strip text =
  let rec loop acc = function
    | [] ->
        Zed_utf8.rev_concat "" acc
    | String str :: rest ->
        loop (str :: acc) rest
    | _ :: rest ->
        loop acc rest
  in
  loop [] text
