(*
 * lt_windows.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt

external get_acp : unit -> int = "lt_windows_get_acp"
external get_console_cp : unit -> int = "lt_windows_get_console_cp"
external set_console_cp : int -> unit = "lt_windows_set_console_cp"
external get_console_output_cp : unit -> int = "lt_windows_get_console_output_cp"
external set_console_output_cp : int -> unit = "lt_windows_set_console_output_cp"

type original_input =
  | OI_resize
  | OI_code of Lt_key.modifiers * int
  | OI_key of Lt_key.modifiers * Lt_key.t

type input =
  | Resize
  | Key of Lt_key.modifiers * Lt_key.t

external read_console_input_job : Unix.file_descr -> [ `read_console_input ] Lwt_unix.job = "lt_windows_read_console_input_job"
external read_console_input_result : [ `read_console_input ] Lwt_unix.job -> original_input = "lt_windows_read_console_input_result"
external read_console_input_free : [ `read_console_input ] Lwt_unix.job -> unit = "lt_windows_read_console_input_free"

let controls = [|
  " ";
  "a";
  "b";
  "c";
  "d";
  "e";
  "f";
  "g";
  "h";
  "i";
  "j";
  "k";
  "l";
  "m";
  "n";
  "o";
  "p";
  "q";
  "r";
  "s";
  "t";
  "u";
  "v";
  "w";
  "x";
  "y";
  "z";
  "[";
  "\\";
  "]";
  "^";
  "_";
|]

let read_console_input fd =
  Lwt_unix.check_descriptor fd;
  Lwt_unix.execute_job
    (read_console_input_job (Lwt_unix.unix_file_descr fd))
    read_console_input_result
    read_console_input_free
  >|= function
    | OI_resize ->
        Resize
    | OI_code(mods, code) ->
        if code < 32 then
          Key(mods, Lt_key.Char controls.(code))
        else
          Key(mods, Lt_key.Char(Text.char code))
    | OI_key(mods, key) ->
        Key(mods, key)
