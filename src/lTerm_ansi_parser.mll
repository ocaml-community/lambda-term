(*
 * lTerm_ansi_parser.mli
 * ---------------------
 * Copyright : (c) 2015, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

{
let apply_sequence =
  let rec loop seq style =
    let open LTerm_style in
    match seq with
    | x :: seq when x >= 30 && x < 38 ->
      set_foreground style (Color.index (x - 30)) |> loop seq
    | x :: seq when x >= 40 && x < 48 ->
      set_background style (Color.index (x - 40)) |> loop seq
    | 39 :: seq -> set_foreground style Color.default |> loop seq
    | 49 :: seq -> set_background style Color.default |> loop seq
    | 38 :: 5 :: n :: seq -> set_foreground style (Color.index n) |> loop seq
    | 48 :: 5 :: n :: seq -> set_background style (Color.index n) |> loop seq
    | 0 :: seq -> loop seq default
    | 1 :: seq -> set_bold      style On |> loop seq
    | 4 :: seq -> set_underline style On |> loop seq
    | 5 :: seq -> set_blink     style On |> loop seq
    | 7 :: seq -> set_reverse   style On |> loop seq
    | _ :: seq -> loop seq style
    | [] -> style
  in
  fun style seq -> loop seq style


let split_seq s =
  let rec loop s i =
    match String.index_from s i ';' with
    | exception _ ->
      [String.sub s i (String.length s - i)]
    | j ->
      String.sub s i (j - i) :: loop s (i + 1)
  in
  loop s 0
}

let arg = ['0'-'9'] ['0'-'9']*

rule token style = parse
  | "\027[" (arg (';' arg)* as args) 'm'
      { let style =
          split_seq args
          |> List.map int_of_string
          |> apply_sequence style
        in
        (Lexing.lexeme_start lexbuf,
         Lexing.lexeme_end   lexbuf,
         style)
      }
  | _
    { token style lexbuf }
  | eof
    { (Lexing.lexeme_start lexbuf,
       Lexing.lexeme_end   lexbuf,
       style)
    }
