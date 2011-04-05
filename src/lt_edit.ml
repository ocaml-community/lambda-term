(*
 * lt_edit.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Zed_edit
open Lt_key

(* +-----------------------------------------------------------------+
   | Bindings                                                        |
   +-----------------------------------------------------------------+ *)

let bindings = Hashtbl.create 128

let () =
  let ( --> ) key action = Hashtbl.add bindings key action in
  { control = false; meta = false; shift = false; code = Left } --> Prev_char;
  { control = false; meta = false; shift = false; code = Right } --> Next_char;
  { control = false; meta = false; shift = false; code = Home } --> Goto_bot;
  { control = false; meta = false; shift = false; code = End } --> Goto_eot;
  { control = false; meta = false; shift = false; code = Insert } --> Switch_erase_mode;
  { control = false; meta = false; shift = false; code = Delete } --> Delete_next_char;
  { control = false; meta = false; shift = false; code = Enter } --> Newline;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char ' ') } --> Set_mark;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'a') } --> Goto_bol;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'e') } --> Goto_eol;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'd') } --> Delete_next_char;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'k') } --> Kill_next_line;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'u') } --> Kill_prev_line;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'n') } --> Prev_char;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'p') } --> Next_char;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'w') } --> Kill;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'y') } --> Yank;
  { control = false; meta = false; shift = false; code = Backspace } --> Delete_prev_char;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'w') } --> Copy;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'c') } --> Capitalize_word;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'l') } --> Lowercase_word;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'u') } --> Uppercase_word;
  { control = false; meta = true; shift = false; code = Right } --> Next_word;
  { control = false; meta = true; shift = false; code = Left } --> Prev_word;
  { control = true; meta = false; shift = false; code = Right } --> Next_word;
  { control = true; meta = false; shift = false; code = Left } --> Prev_word
