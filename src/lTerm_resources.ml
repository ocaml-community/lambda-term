
(*
 * lTerm_resources.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

let home =
  try
    Sys.getenv "HOME"
  with Not_found ->
    try
      (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
    with Unix.Unix_error _ | Not_found ->
      if Sys.win32 then
        try
          Sys.getenv "AppData"
        with Not_found ->
          ""
      else
        ""

type xdg_location = Cache | Config | Data

module XDGBD = struct
  let ( / ) = Filename.concat

  let get env_var unix_default win32_default =
    try
      Sys.getenv env_var
    with Not_found ->
      if Sys.win32 then win32_default else unix_default

  let cache  = get "XDG_CACHE_HOME"  (home / ".cache")           (home / "Local Settings" / "Cache")
  let config = get "XDG_CONFIG_HOME" (home / ".config")          (home / "Local Settings")
  let data   = get "XDG_DATA_HOME"   (home / ".local" / "share") (try Sys.getenv "AppData" with Not_found -> "")

  let user_dir = function
    | Cache  -> cache
    | Config -> config
    | Data   -> data
end

let xdgbd_warning loc file_name =
  let loc_name = match loc with
    | Cache  -> "$XDG_CACHE_HOME"
    | Config -> "$XDG_CONFIG_HOME"
    | Data   -> "$XDG_DATA_HOME" in
  Printf.eprintf
    "Warning: it is recommended to move `%s` to `%s`, see:\n%s\n"
    file_name loc_name
    "http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

let xdgbd_file ~loc ?(allow_legacy_location=false) name =
  let home_file = Filename.concat home name in
  if allow_legacy_location && Sys.file_exists home_file then
    let () = xdgbd_warning loc home_file in
    home_file
  else
    Filename.concat (XDGBD.user_dir loc) name

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
  | "true"      -> Some true
  | "false"     -> Some false
  | "" | "none" -> None
  | s           -> Printf.ksprintf error "invalid boolean value %S" s

let get_switch key resources : LTerm_style.Switch.t =
  match String.lowercase (get key resources) with
  | "on"  | "true"        -> On
  | "off" | "false"       -> Off
  | "unset" | "none" | "" -> Unset
  | s                     -> Printf.ksprintf error "invalid switch value %S" s

let get_color key resources =
  match String.lowercase (get key resources) with
  | "" | "none" -> LTerm_style.Color.transparent
  | s  -> LTerm_style.Color.of_string s

let get_style prefix resources =
  LTerm_style.make
    ~bold:(get_switch (prefix ^ ".bold") resources)
    ~underline:(get_switch (prefix ^ ".underline") resources)
    ~blink:(get_switch (prefix ^ ".blink") resources)
    ~reverse:(get_switch (prefix ^ ".reverse") resources)
    ~foreground:(get_color (prefix ^ ".foreground") resources)
    ~background:(get_color (prefix ^ ".background") resources)
    ()

let get_connection key resources : LTerm_draw.Connection.t =
  match String.lowercase (get key resources) with
  | "blank"  -> Blank
  | "light"  -> Light
  | "heavy"  -> Heavy
  | "double" -> Double
  | ""       -> Light
  | str      -> Printf.ksprintf error "invalid connection %S" str

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

let load_sync file =
  let rec loop ic lineno acc =
    match input_line ic with
    | exception End_of_file -> acc
    | str ->
      match LTerm_resource_lexer.line (Lexing.from_string str) with
      | `EOF ->
        loop ic (lineno + 1) acc
      | `Empty ->
        loop ic (lineno + 1) acc
      | `Assoc(pattern, value) ->
        loop ic (lineno + 1) (add pattern value acc)
      | `Error msg ->
        raise (Parse_error(file, lineno, msg))
  in
  let ic = open_in file in
  match loop ic 1 [] with
  | x -> close_in ic; x
  | exception e -> close_in ic; raise e
