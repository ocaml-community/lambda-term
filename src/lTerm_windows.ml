(*
 * lTerm_windows.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

external get_acp : unit -> int = "lt_windows_get_acp"
external get_console_cp : unit -> int = "lt_windows_get_console_cp"
external set_console_cp : int -> unit = "lt_windows_set_console_cp"
external get_console_output_cp : unit -> int = "lt_windows_get_console_output_cp"
external set_console_output_cp : int -> unit = "lt_windows_set_console_output_cp"

include struct
  (* Disable warning 37 for unused constructors, as these are only created from C *)
  [@@@warning "-37"]
  type input =
    | Resize
    | Uchar       of LTerm_event.Modifiers.t * Uchar.t
    | Key         of LTerm_event.Modifiers.t * LTerm_event.Key.t
    | Button_down of LTerm_event.Modifiers.t * int * LTerm_geom.coord
end

external read_console_input : Unix.file_descr -> input = "lt_windows_read_console_input"

let controls =
  [| ' '
   ; 'a'
   ; 'b'
   ; 'c'
   ; 'd'
   ; 'e'
   ; 'f'
   ; 'g'
   ; 'h'
   ; 'i'
   ; 'j'
   ; 'k'
   ; 'l'
   ; 'm'
   ; 'n'
   ; 'o'
   ; 'p'
   ; 'q'
   ; 'r'
   ; 's'
   ; 't'
   ; 'u'
   ; 'v'
   ; 'w'
   ; 'x'
   ; 'y'
   ; 'z'
   ; '['
   ; '\\'
   ; ']'
   ; '^'
   ; '_'
  |]

let read_console_input fd : LTerm_event.t =
  match read_console_input fd with
  | Uchar (mods, ch) ->
    if Uchar.to_int ch < 32 then
      Char (mods, controls.(Uchar.to_int ch))
    else if Uchar.to_int ch < 256 then
      Char (mods, Uchar.to_char ch)
    else
      Uchar (mods, ch)
  | Key (mods, key) ->
    Key (mods, key)
  | Button_down (mods, button, coord) ->
    Button_down (mods, button, coord)
  | Resize -> Resize { rows = 0; cols = 0 }

type text_attributes =
  { foreground : int
  ; background : int
  }

type console_screen_buffer_info =
  { size                : LTerm_geom.size
  ; cursor_position     : LTerm_geom.coord
  ; attributes          : text_attributes
  ; window              : LTerm_geom.rect
  ; maximum_window_size : LTerm_geom.size
  }

external get_console_screen_buffer_info : Unix.file_descr -> console_screen_buffer_info
  = "lt_windows_get_console_screen_buffer_info"

type console_mode = {
  cm_echo_input : bool;
  cm_insert_mode : bool;
  cm_line_input : bool;
  cm_mouse_input : bool;
  cm_processed_input : bool;
  cm_quick_edit_mode : bool;
  cm_window_input : bool;
}

external get_console_mode : Unix.file_descr -> console_mode
  = "lt_windows_get_console_mode"
external set_console_mode : Unix.file_descr -> console_mode -> unit
  = "lt_windows_set_console_mode"

external get_console_cursor_info : Unix.file_descr -> int * bool
  = "lt_windows_get_console_cursor_info"
external set_console_cursor_info : Unix.file_descr -> int -> bool -> unit
  = "lt_windows_set_console_cursor_info"

external set_console_cursor_position : Unix.file_descr -> LTerm_geom.coord -> unit
  = "lt_windows_set_console_cursor_position"
external set_console_text_attribute : Unix.file_descr -> text_attributes -> unit
  = "lt_windows_set_console_text_attribute"

type char_info = {
  ci_char : Uchar.t;
  ci_foreground : int;
  ci_background : int;
}

external write_console_output
  :  Unix.file_descr
    -> char_info array array
    -> LTerm_geom.size
    -> LTerm_geom.coord
    -> LTerm_geom.rect
    -> LTerm_geom.rect = "lt_windows_write_console_output"

let write_console_output fd chars size coord rect =
  if Array.length chars <> size.LTerm_geom.rows then
    invalid_arg "LTerm_windows.write_console_output";
  Array.iter
    (fun line ->
       if Array.length line <> size.LTerm_geom.cols then
         invalid_arg "LTerm_windows.write_console_output")
    chars;
  write_console_output fd chars size coord rect

external fill_console_output_character
  :  Unix.file_descr
    -> Uchar.t
    -> int
    -> LTerm_geom.coord
    -> int = "lt_windows_fill_console_output_character"
