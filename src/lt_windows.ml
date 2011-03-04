(*
 * lt_windows.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt

external get_acp : unit -> int = "lt_windows_get_acp"
external get_console_cp : unit -> int = "lt_windows_get_console_cp"
external set_console_cp : int -> unit = "lt_windows_set_console_cp"
external get_console_output_cp : unit -> int = "lt_windows_get_console_output_cp"
external set_console_output_cp : int -> unit = "lt_windows_set_console_output_cp"

type input =
  | Resize
  | Key of Lt_key.t
  | Mouse of Lt_mouse.t

external read_console_input_job : Unix.file_descr -> [ `read_console_input ] Lwt_unix.job = "lt_windows_read_console_input_job"
external read_console_input_result : [ `read_console_input ] Lwt_unix.job -> input = "lt_windows_read_console_input_result"
external read_console_input_free : [ `read_console_input ] Lwt_unix.job -> unit = "lt_windows_read_console_input_free"

let controls = [|
  UChar.of_char ' ';
  UChar.of_char 'a';
  UChar.of_char 'b';
  UChar.of_char 'c';
  UChar.of_char 'd';
  UChar.of_char 'e';
  UChar.of_char 'f';
  UChar.of_char 'g';
  UChar.of_char 'h';
  UChar.of_char 'i';
  UChar.of_char 'j';
  UChar.of_char 'k';
  UChar.of_char 'l';
  UChar.of_char 'm';
  UChar.of_char 'n';
  UChar.of_char 'o';
  UChar.of_char 'p';
  UChar.of_char 'q';
  UChar.of_char 'r';
  UChar.of_char 's';
  UChar.of_char 't';
  UChar.of_char 'u';
  UChar.of_char 'v';
  UChar.of_char 'w';
  UChar.of_char 'x';
  UChar.of_char 'y';
  UChar.of_char 'z';
  UChar.of_char '[';
  UChar.of_char '\\';
  UChar.of_char ']';
  UChar.of_char '^';
  UChar.of_char '_';
|]

let read_console_input fd =
  Lwt_unix.check_descriptor fd;
  Lwt_unix.execute_job
    (read_console_input_job (Lwt_unix.unix_file_descr fd))
    read_console_input_result
    read_console_input_free
  >|= function
    | Key({ Lt_key.code = Lt_key.Char ch } as key) when UChar.code ch < 32 ->
        Key { key with Lt_key.code = Lt_key.Char controls.(UChar.code ch) }
    | input ->
        input

type text_attributes = {
  foreground : int;
  background : int;
}

type console_screen_buffer_info = {
  size : Lt_types.size;
  cursor_position : Lt_types.coord;
  attributes : text_attributes;
  window : Lt_types.rect;
  maximum_window_size : Lt_types.size;
}

external get_console_screen_buffer_info : Unix.file_descr -> console_screen_buffer_info = "lt_windows_get_console_screen_buffer_info"

let get_console_screen_buffer_info fd =
  Lwt_unix.check_descriptor fd;
  get_console_screen_buffer_info (Lwt_unix.unix_file_descr fd)

type console_mode = {
  cm_echo_input : bool;
  cm_insert_mode : bool;
  cm_line_input : bool;
  cm_mouse_input : bool;
  cm_processed_input : bool;
  cm_quick_edit_mode : bool;
  cm_window_input : bool;
}

external get_console_mode : Unix.file_descr -> console_mode = "lt_windows_get_console_mode"
external set_console_mode : Unix.file_descr -> console_mode -> unit = "lt_windows_set_console_mode"

let get_console_mode fd =
  Lwt_unix.check_descriptor fd;
  get_console_mode (Lwt_unix.unix_file_descr fd)

let set_console_mode fd mode =
  Lwt_unix.check_descriptor fd;
  set_console_mode (Lwt_unix.unix_file_descr fd) mode

external get_console_cursor_info : Unix.file_descr -> int * bool = "lt_windows_get_console_cursor_info"
external set_console_cursor_info : Unix.file_descr -> int -> bool -> unit = "lt_windows_set_console_cursor_info"

let get_console_cursor_info fd =
  Lwt_unix.check_descriptor fd;
  get_console_cursor_info (Lwt_unix.unix_file_descr fd)

let set_console_cursor_info fd size visible =
  Lwt_unix.check_descriptor fd;
  set_console_cursor_info (Lwt_unix.unix_file_descr fd) size visible

external set_console_cursor_position : Unix.file_descr -> Lt_types.coord -> unit = "lt_windows_set_console_cursor_position"

let set_console_cursor_position fd coord =
  Lwt_unix.check_descriptor fd;
  set_console_cursor_position (Lwt_unix.unix_file_descr fd) coord

external set_console_text_attribute : Unix.file_descr -> text_attributes -> unit = "lt_windows_set_console_text_attribute"

let set_console_text_attribute fd attrs =
  Lwt_unix.check_descriptor fd;
  set_console_text_attribute (Lwt_unix.unix_file_descr fd) attrs

type char_info = {
  ci_char : UChar.t;
  ci_foreground : int;
  ci_background : int;
}

external write_console_output : Unix.file_descr -> char_info array array -> Lt_types.size -> Lt_types.coord -> Lt_types.rect -> Lt_types.rect = "lt_windows_write_console_output"

let write_console_output fd chars size coord rect =
  Lwt_unix.check_descriptor fd;
  if Array.length chars <> size.Lt_types.lines then invalid_arg "Lt_windows.write_console_output";
  Array.iter
    (fun line ->
       if Array.length line <> size.Lt_types.columns then invalid_arg "Lt_windows.write_console_output")
    chars;
  write_console_output (Lwt_unix.unix_file_descr fd) chars size coord rect
