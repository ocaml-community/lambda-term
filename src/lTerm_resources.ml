(*
 * lTerm_resources.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type pattern = string list
    (* Type of a pattern. For example the pattern ["foo*bar*"] is
       represented by the list [["foo"; "bar"; ""]]. *)

type t = (pattern * string) list

(* +-----------------------------------------------------------------+
   | Pattern matching                                                |
   +-----------------------------------------------------------------+ *)

let sub_equal str ofs patt =
  let str_len = String.length str and patt_len = String.length patt in
  let rec loop ofs ofs_patt =
    ofs_patt = patt_len || (str.[ofs] = patt.[ofs_patt] && loop (ofs + 1) (ofs_patt + 1))
  in
  ofs + patt_len <= str_len && loop ofs 0

let pattern_match pattern string =
  let length = String.length string in
  let rec loop offset pattern =
    if offset = length then
      pattern = [] || pattern = [""]
    else
      match pattern with
        | [] ->
            false
        | literal :: pattern ->
            let literal_length = String.length literal in
            let max_offset = length - literal_length in
            let rec search offset =
              offset <= max_offset
              && ((sub_equal string offset literal && loop (offset + literal_length) pattern)
                  || search (offset + 1))
            in
            search offset
  in
  match pattern with
    | [] ->
        string = ""
    | literal :: pattern ->
        sub_equal string 0 literal && loop (String.length literal) pattern

(* +-----------------------------------------------------------------+
   | Pattern creation                                                |
   +-----------------------------------------------------------------+ *)

let split pattern =
  let len = String.length pattern in
  let rec loop ofs =
    if ofs = len then
      [""]
    else
      match try Some(String.index_from pattern ofs '*') with Not_found -> None with
        | Some ofs' ->
            String.sub pattern ofs (ofs' - ofs) :: loop (ofs' + 1)
        | None ->
            [String.sub pattern ofs (len - ofs)]
  in
  loop 0

(* +-----------------------------------------------------------------+
   | Set operations                                                  |
   +-----------------------------------------------------------------+ *)

let empty = []

let rec get key = function
  | [] ->
      ""
  | (pattern, value) :: rest ->
      if pattern_match pattern key then
        value
      else
        get key rest

let add pattern value resources = (split pattern, value) :: resources

let merge = ( @ )

(* +-----------------------------------------------------------------+
   | Readers                                                         |
   +-----------------------------------------------------------------+ *)

exception Error of string
let error str = raise (Error str)

let get_bool key resources =
  match String.lowercase (get key resources) with
    | "true" -> Some true
    | "false" -> Some false
    | "" | "none" -> None
    | s -> Printf.ksprintf error "invalid boolean value %S" s

let hex_of_char ch = match ch with
  | '0' .. '9' -> Char.code ch - Char.code '0'
  | 'A' .. 'F' -> Char.code ch - Char.code 'A' + 10
  | 'a' .. 'f' -> Char.code ch - Char.code 'a' + 10
  | ch -> raise Exit

let get_color key resources =
  match String.lowercase (get key resources) with
    | "default" -> Some LTerm_style.default
    | "black" -> Some LTerm_style.black
    | "red" -> Some LTerm_style.red
    | "green" -> Some LTerm_style.green
    | "yellow" -> Some LTerm_style.yellow
    | "blue" -> Some LTerm_style.blue
    | "magenta" -> Some LTerm_style.magenta
    | "cyan" -> Some LTerm_style.cyan
    | "white" -> Some LTerm_style.white
    | "lblack" -> Some LTerm_style.lblack
    | "lred" -> Some LTerm_style.lred
    | "lgreen" -> Some LTerm_style.lgreen
    | "lyellow" -> Some LTerm_style.lyellow
    | "lblue" -> Some LTerm_style.lblue
    | "lmagenta" -> Some LTerm_style.lmagenta
    | "lcyan" -> Some LTerm_style.lcyan
    | "lwhite" -> Some LTerm_style.lwhite
    | "light-black" -> Some LTerm_style.lblack
    | "light-red" -> Some LTerm_style.lred
    | "light-green" -> Some LTerm_style.lgreen
    | "light-yellow" -> Some LTerm_style.lyellow
    | "light-blue" -> Some LTerm_style.lblue
    | "light-magenta" -> Some LTerm_style.lmagenta
    | "light-cyan" -> Some LTerm_style.lcyan
    | "light-white" -> Some LTerm_style.lwhite
    | "" | "none" -> None
    | str when str.[0] = '#' ->
        if String.length str = 7 then
          try
            Some(LTerm_style.rgb
                   (hex_of_char str.[1] lsl 4 lor hex_of_char str.[2])
                   (hex_of_char str.[3] lsl 4 lor hex_of_char str.[4])
                   (hex_of_char str.[5] lsl 4 lor hex_of_char str.[6]))
          with Exit ->
            Printf.ksprintf error "invalid color %S" str
        else
          Printf.ksprintf error "invalid color %S" str
    | str ->
        try
          Some(LTerm_style.index (int_of_string str))
        with Failure _ ->
          Printf.ksprintf error "invalid color %S" str

let get_style prefix resources = {
  LTerm_style.bold = get_bool (prefix ^ ".bold") resources;
  LTerm_style.underline = get_bool (prefix ^ ".underline") resources;
  LTerm_style.blink = get_bool (prefix ^ ".blink") resources;
  LTerm_style.reverse = get_bool (prefix ^ ".reverse") resources;
  LTerm_style.foreground = get_color (prefix ^ ".foreground") resources;
  LTerm_style.background = get_color (prefix ^ ".background") resources;
}

let get_connection key resources =
  match String.lowercase (get key resources) with
    | "blank" -> LTerm_draw.Blank
    | "light" -> LTerm_draw.Light
    | "heavy" -> LTerm_draw.Heavy
    | "" -> LTerm_draw.Light
    | str -> Printf.ksprintf error "invalid connection %S" str

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

exception Parse_error of string * int * string

let parse str =
  let lexbuf = Lexing.from_string str in
  let rec loop line acc =
    match LTerm_resource_lexer.line lexbuf with
      | `EOF ->
          acc
      | `Empty ->
          loop (line + 1) acc
      | `Assoc(pattern, value) ->
          loop (line + 1) (add pattern value acc)
      | `Error msg ->
          raise (Parse_error("<string>", line, msg))
  in
  loop 1 []

let load file =
  lwt ic = Lwt_io.open_file ~mode:Lwt_io.input file in
  let rec loop line acc =
    match_lwt Lwt_io.read_line_opt ic with
      | None ->
          Lwt.return acc
      | Some str ->
          match LTerm_resource_lexer.line (Lexing.from_string str) with
            | `EOF ->
                loop (line + 1) acc
            | `Empty ->
                loop (line + 1) acc
            | `Assoc(pattern, value) ->
                loop (line + 1) (add pattern value acc)
            | `Error msg ->
                raise_lwt (Parse_error(file, line, msg))
  in
  try_lwt
    loop 1 []
  finally
    Lwt_io.close ic
