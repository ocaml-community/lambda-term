(*
 * lTerm_event.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Zed
open Core.Std

module Modifiers = struct
  type t =
    | N
    | C
    | M
    | S
    | C_M
    | C_S
    | M_S
    | C_M_S

  let with_c = function
    | N -> C
    | C -> C
    | M -> C_M
    | S -> C_S
    | C_M -> C_M
    | C_S -> C_S
    | M_S -> C_M_S
    | C_M_S -> C_M_S
  ;;

  let with_m = function
    | N -> M
    | C -> C_M
    | M -> M
    | S -> M_S
    | C_M -> C_M
    | C_S -> C_M_S
    | M_S -> M_S
    | C_M_S -> C_M_S
  ;;

  let with_s = function
    | N -> S
    | C -> C_S
    | M -> M_S
    | S -> S
    | C_M -> C_M_S
    | C_S -> C_S
    | M_S -> M_S
    | C_M_S -> C_M_S
  ;;

  let to_string = function
    | N -> ""
    | C -> "C-"
    | M -> "M-"
    | S -> "S-"
    | C_M -> "C-M-"
    | C_S -> "C-S-"
    | M_S -> "M-S-"
    | C_M_S -> "C-M-S-"
  ;;

  let prefixes =
    [ "C-M-S-" , C_M_S
    ; "C-S-"   , C_S
    ; "C-M-"   , C_M
    ; "M-S-"   , M_S
    ; "C-"     , C
    ; "M-"     , M
    ; "S-"     , S
    ; ""       , N
    ]
  ;;

  let extract_from_string s =
    let rec loop s = function
      | [] -> (N, s)
      | (prefix, m) :: rest ->
        if String.is_prefix s ~prefix then
          (m, String.drop_prefix s (String.length prefix))
        else
          loop s rest
    in
    loop s prefixes
  ;;
end

module Key = struct
  type t =
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
    | Next
    | Prev
    | Home
    | End
    | Insert
    | Delete
    | Backspace

  let to_string = function
    | Enter     -> "enter"
    | Escape    -> "escape"
    | Tab       -> "tab"
    | Up        -> "up"
    | Down      -> "down"
    | Left      -> "left"
    | Right     -> "right"
    | F1        -> "f1"
    | F2        -> "f2"
    | F3        -> "f3"
    | F4        -> "f4"
    | F5        -> "f5"
    | F6        -> "f6"
    | F7        -> "f7"
    | F8        -> "f8"
    | F9        -> "f9"
    | F10       -> "f10"
    | F11       -> "f11"
    | F12       -> "f12"
    | Next      -> "next"
    | Prev      -> "prev"
    | Home      -> "home"
    | End       -> "end"
    | Insert    -> "insert"
    | Delete    -> "delete"
    | Backspace -> "backspace"

  let of_string = function
    | "enter"     -> Some Enter
    | "escape"    -> Some Escape
    | "tab"       -> Some Tab
    | "up"        -> Some Up
    | "down"      -> Some Down
    | "left"      -> Some Left
    | "right"     -> Some Right
    | "f1"        -> Some F1
    | "f2"        -> Some F2
    | "f3"        -> Some F3
    | "f4"        -> Some F4
    | "f5"        -> Some F5
    | "f6"        -> Some F6
    | "f7"        -> Some F7
    | "f8"        -> Some F8
    | "f9"        -> Some F9
    | "f10"       -> Some F10
    | "f11"       -> Some F11
    | "f12"       -> Some F12
    | "next"      -> Some Next
    | "prev"      -> Some Prev
    | "home"      -> Some Home
    | "end"       -> Some End
    | "insert"    -> Some Insert
    | "delete"    -> Some Delete
    | "backspace" -> Some Backspace
    | _           -> None
end

module Signal = struct
  type t =
    | Intr
    | Quit
    | Susp

  let to_string = function
    | Intr -> "intr"
    | Quit -> "quit"
    | Susp -> "susp"
end

type t =
  | Text         of string
  | Char         of Modifiers.t * char
  | Uchar        of Modifiers.t * Uchar.t
  | Key          of Modifiers.t * Key.t
  | Sequence     of string
  | Button_down  of Modifiers.t * int * LTerm_geom.coord
  | Button_up    of Modifiers.t * int * LTerm_geom.coord
  | Signal       of Signal.t
  | Resume
  | Resize
  | Closed

let hash = Hashtbl.hash

let string_of_coord (c : LTerm_geom.coord) =
  match c with
  | { row = 0; col = 0 } -> ""
  | { row    ; col     } -> Printf.sprintf "-%d:%d" row col
;;

let to_string = function
  | Text s                -> s
  | Char  (m, ' ')        -> Modifiers.to_string m ^ "space"
  | Char  (m, c)          -> Modifiers.to_string m ^ Zed_utf8.singleton
                                                       (Uchar.of_char c)
  | Uchar (m, c)          -> Modifiers.to_string m ^ Zed_utf8.singleton c
  | Key   (m, k)          -> Modifiers.to_string m ^ Key.to_string k
  | Sequence s            -> s
  | Button_down (m, b, c) -> Printf.sprintf !"%{Modifiers}button-down-%d"
                               m b ^ string_of_coord c
  | Button_up   (m, b, c) -> Printf.sprintf !"%{Modifiers}button-up-%d"
                               m b ^ string_of_coord c
  | Signal s              -> Signal.to_string s
  | Resume                -> "resume"
  | Resize                -> "resize"
  | Closed                -> "closed"
;;

type pattern =
  | No_mod
    :  ('a, Scanf.Scanning.scanbuf, 'b, 'c -> t, 'a -> 'd, 'd) format6
       * 'c
    -> pattern
  | With_mod
    :  ('a, Scanf.Scanning.scanbuf, 'b, 'c -> t, 'a -> 'd, 'd) format6
       * (Modifiers.t -> 'c)
    -> pattern

let make_char m n =
  if n <= 127 then
    Char (m, Char.of_int_exn n)
  else
    Uchar (m, Uchar.of_int n)
;;

let zero_coord : LTerm_geom.coord = { row = 0; col = 0 }

let patterns =
  [ No_mod   ("\027%s"               , fun s -> Sequence s)
  ; No_mod   ("intr"                 , Signal Intr)
  ; No_mod   ("quit"                 , Signal Quit)
  ; No_mod   ("susp"                 , Signal Susp)
  ; No_mod   ("resume"               , Resume)
  ; No_mod   ("resize"               , Resize)
  ; No_mod   ("closed"               , Closed)
  ; With_mod ("space"                , fun m -> Char (m, ' '))
  ; With_mod ("U+%x"                 , fun m n -> make_char m n)
  ; With_mod ("button-down-%u"       , fun m b -> Button_down (m, b, zero_coord))
  ; With_mod ("button-up-%u"         , fun m b -> Button_up (m, b, zero_coord))
  ; With_mod ("button-down-%u-%u:%u" , fun m b row col -> Button_down (m, b, {row; col}))
  ; With_mod ("button-up-%u-%u:%u"   , fun m b row col -> Button_up (m, b, {row; col}))
  ; No_mod   ("%s"                   , fun s -> Text s)
  ]
;;

let patterns =
  List.map patterns ~f:(function
    |   No_mod (pat, f) ->   No_mod (pat ^^ "\x00", f)
    | With_mod (pat, f) -> With_mod (pat ^^ "\x00", f))
;;

let rec search_patterns m s patterns =
  match patterns with
  | [] ->  Text s
  | No_mod (pat, f) :: rest ->
    if m = Modifiers.N then try_pattern pat f m s rest else search_patterns m s rest
  | With_mod (pat, f) :: rest ->
    try_pattern pat (f m) m s rest
and try_pattern
  :  'a 'b 'c 'd. ('a, Scanf.Scanning.scanbuf, 'b, 'c -> t, 'a -> 'd, 'd) format6
    -> 'c
    -> Modifiers.t
    -> string
    -> pattern list
    -> t
  = fun pat f m s patterns ->
    let buf = Scanf.Scanning.from_string s in
    try
      let x = Scanf.bscanf buf pat f in
      assert (Scanf.Scanning.end_of_input buf);
      x
    with _ ->
      search_patterns m s patterns
;;

let of_string s =
  let m, s = Modifiers.extract_from_string s in
  try
    Key (m, Key.of_string s)
  with _ ->
  match Zed_utf8.check s with
  | Ok 1 ->
    let c = Zed_utf8.extract s 0 in
    if Uchar.code c <= 127 then
      Char (m, Uchar.to_char c)
    else
      Uchar (m, c)
  | _ ->
    search_patterns m (s ^ "\x00") patterns
;;

let with_coord t c =
  match t with
  | Button_down (m, b, c') when c <> c' -> Button_down (m, b, c)
  | Button_up   (m, b, c') when c <> c' -> Button_up   (m, b, c)
  | t -> t
;;

let remove_coord t = with_coord t zero_coord
