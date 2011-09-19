(*
 * lTerm_unix.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt
open LTerm_key

external get_sigwinch : unit -> int option = "lt_unix_get_sigwinch"
external get_system_encoding : unit -> string = "lt_unix_get_system_encoding"

let sigwinch = get_sigwinch ()
let system_encoding = get_system_encoding ()

(* +-----------------------------------------------------------------+
   | Parsing of encoded characters                                   |
   +-----------------------------------------------------------------+ *)

class output_single (cell : UChar.t option ref) = object
  method put char = cell := Some char
  method flush () = ()
  method close_out () = ()
end

let parse_char encoding st first_byte =
  let cell = ref None in
  let output = new CharEncoding.convert_uchar_output encoding (new output_single cell) in
  let rec loop st =
    match !cell with
      | Some char ->
          return char
      | None ->
          lwt byte = Lwt_stream.next st in
          assert (output#output (String.make 1 byte) 0 1 = 1);
          output#flush ();
          loop st
  in
  try_lwt
    assert (output#output (String.make 1 first_byte) 0 1 = 1);
    Lwt_stream.parse st loop
  with CharEncoding.Malformed_code | Lwt_stream.Empty ->
    return (UChar.of_char first_byte)

(* +-----------------------------------------------------------------+
   | Input of escape sequence                                        |
   +-----------------------------------------------------------------+ *)

exception Not_a_sequence

let parse_escape escape_time st =
  let buf = Buffer.create 32 in
  (* Read one character and add it to [buf]: *)
  let get () =
    match_lwt pick [Lwt_stream.get st; Lwt_unix.sleep escape_time >> return None] with
      | None ->
          (* If the rest is not immediatly available, conclude that
             this is not an escape sequence but just the escape
             key: *)
          raise_lwt Not_a_sequence
      | Some('\x00' .. '\x1f' | '\x80' .. '\xff') ->
          (* Control characters and non-ascii characters are not part
             of escape sequences. *)
          raise_lwt Not_a_sequence
      | Some ch ->
          Buffer.add_char buf ch;
          return ch
  in

  let rec loop () =
    get () >>= function
      | '0' .. '9' | ';' | '[' ->
          loop ()
      | ch ->
          return (Buffer.contents buf)
  in

  get () >>= function
    | '[' | 'O' ->
        loop ()
    | _ ->
        raise_lwt Not_a_sequence

(* +-----------------------------------------------------------------+
   | Escape sequences mapping                                        |
   +-----------------------------------------------------------------+ *)

let controls = [|
  Char(UChar.of_char ' ');
  Char(UChar.of_char 'a');
  Char(UChar.of_char 'b');
  Char(UChar.of_char 'c');
  Char(UChar.of_char 'd');
  Char(UChar.of_char 'e');
  Char(UChar.of_char 'f');
  Char(UChar.of_char 'g');
  Char(UChar.of_char 'h');
  Tab;
  Enter;
  Char(UChar.of_char 'k');
  Char(UChar.of_char 'l');
  Char(UChar.of_char 'm');
  Char(UChar.of_char 'n');
  Char(UChar.of_char 'o');
  Char(UChar.of_char 'p');
  Char(UChar.of_char 'q');
  Char(UChar.of_char 'r');
  Char(UChar.of_char 's');
  Char(UChar.of_char 't');
  Char(UChar.of_char 'u');
  Char(UChar.of_char 'v');
  Char(UChar.of_char 'w');
  Char(UChar.of_char 'x');
  Char(UChar.of_char 'y');
  Char(UChar.of_char 'z');
  Escape;
  Char(UChar.of_char '\\');
  Char(UChar.of_char ']');
  Char(UChar.of_char '^');
  Char(UChar.of_char '_');
|]

let sequences = [|
  "[1~", { control = false; meta = false; shift = false; code = Home };
  "[2~", { control = false; meta = false; shift = false; code = Insert };
  "[3~", { control = false; meta = false; shift = false; code = Delete };
  "[4~", { control = false; meta = false; shift = false; code = End };
  "[5~", { control = false; meta = false; shift = false; code = Prev_page };
  "[6~", { control = false; meta = false; shift = false; code = Next_page };
  "[7~", { control = false; meta = false; shift = false; code = Home };
  "[8~", { control = false; meta = false; shift = false; code = End };
  "[11~", { control = false; meta = false; shift = false; code = F1 };
  "[12~", { control = false; meta = false; shift = false; code = F2 };
  "[13~", { control = false; meta = false; shift = false; code = F3 };
  "[14~", { control = false; meta = false; shift = false; code = F4 };
  "[15~", { control = false; meta = false; shift = false; code = F5 };
  "[17~", { control = false; meta = false; shift = false; code = F6 };
  "[18~", { control = false; meta = false; shift = false; code = F7 };
  "[19~", { control = false; meta = false; shift = false; code = F8 };
  "[20~", { control = false; meta = false; shift = false; code = F9 };
  "[21~", { control = false; meta = false; shift = false; code = F10 };
  "[23~", { control = false; meta = false; shift = false; code = F11 };
  "[24~", { control = false; meta = false; shift = false; code = F12 };

  "[1^", { control = true; meta = false; shift = false; code = Home };
  "[2^", { control = true; meta = false; shift = false; code = Insert };
  "[3^", { control = true; meta = false; shift = false; code = Delete };
  "[4^", { control = true; meta = false; shift = false; code = End };
  "[5^", { control = true; meta = false; shift = false; code = Prev_page };
  "[6^", { control = true; meta = false; shift = false; code = Next_page };
  "[7^", { control = true; meta = false; shift = false; code = Home };
  "[8^", { control = true; meta = false; shift = false; code = End };
  "[11^", { control = true; meta = false; shift = false; code = F1 };
  "[12^", { control = true; meta = false; shift = false; code = F2 };
  "[13^", { control = true; meta = false; shift = false; code = F3 };
  "[14^", { control = true; meta = false; shift = false; code = F4 };
  "[15^", { control = true; meta = false; shift = false; code = F5 };
  "[17^", { control = true; meta = false; shift = false; code = F6 };
  "[18^", { control = true; meta = false; shift = false; code = F7 };
  "[19^", { control = true; meta = false; shift = false; code = F8 };
  "[20^", { control = true; meta = false; shift = false; code = F9 };
  "[21^", { control = true; meta = false; shift = false; code = F10 };
  "[23^", { control = true; meta = false; shift = false; code = F11 };
  "[24^", { control = true; meta = false; shift = false; code = F12 };

  "[1$", { control = false; meta = false; shift = true; code = Home };
  "[2$", { control = false; meta = false; shift = true; code = Insert };
  "[3$", { control = false; meta = false; shift = true; code = Delete };
  "[4$", { control = false; meta = false; shift = true; code = End };
  "[5$", { control = false; meta = false; shift = true; code = Prev_page };
  "[6$", { control = false; meta = false; shift = true; code = Next_page };
  "[7$", { control = false; meta = false; shift = true; code = Home };
  "[8$", { control = false; meta = false; shift = true; code = End };

  "[1@", { control = true; meta = false; shift = true; code = Home };
  "[2@", { control = true; meta = false; shift = true; code = Insert };
  "[3@", { control = true; meta = false; shift = true; code = Delete };
  "[4@", { control = true; meta = false; shift = true; code = End };
  "[5@", { control = true; meta = false; shift = true; code = Prev_page };
  "[6@", { control = true; meta = false; shift = true; code = Next_page };
  "[7@", { control = true; meta = false; shift = true; code = Home };
  "[8@", { control = true; meta = false; shift = true; code = End };

  "[25~", { control = false; meta = false; shift = true; code = F3 };
  "[26~", { control = false; meta = false; shift = true; code = F4 };
  "[28~", { control = false; meta = false; shift = true; code = F5 };
  "[29~", { control = false; meta = false; shift = true; code = F6 };
  "[31~", { control = false; meta = false; shift = true; code = F7 };
  "[32~", { control = false; meta = false; shift = true; code = F8 };
  "[33~", { control = false; meta = false; shift = true; code = F9 };
  "[34~", { control = false; meta = false; shift = true; code = F10 };
  "[23$", { control = false; meta = false; shift = true; code = F11 };
  "[24$", { control = false; meta = false; shift = true; code = F12 };

  "[25^", { control = true; meta = false; shift = true; code = F3 };
  "[26^", { control = true; meta = false; shift = true; code = F4 };
  "[28^", { control = true; meta = false; shift = true; code = F5 };
  "[29^", { control = true; meta = false; shift = true; code = F6 };
  "[31^", { control = true; meta = false; shift = true; code = F7 };
  "[32^", { control = true; meta = false; shift = true; code = F8 };
  "[33^", { control = true; meta = false; shift = true; code = F9 };
  "[34^", { control = true; meta = false; shift = true; code = F10 };
  "[23@", { control = true; meta = false; shift = true; code = F11 };
  "[24@", { control = true; meta = false; shift = true; code = F12 };

  "[Z", { control = false; meta = false; shift = true; code = Tab };

  "[A", { control = false; meta = false; shift = false; code = Up };
  "[B", { control = false; meta = false; shift = false; code = Down };
  "[C", { control = false; meta = false; shift = false; code = Right };
  "[D", { control = false; meta = false; shift = false; code = Left };

  "[a", { control = false; meta = false; shift = true; code = Up };
  "[b", { control = false; meta = false; shift = true; code = Down };
  "[c", { control = false; meta = false; shift = true; code = Right };
  "[d", { control = false; meta = false; shift = true; code = Left };

  "A", { control = false; meta = false; shift = false; code = Up };
  "B", { control = false; meta = false; shift = false; code = Down };
  "C", { control = false; meta = false; shift = false; code = Right };
  "D", { control = false; meta = false; shift = false; code = Left };

  "OA", { control = false; meta = false; shift = false; code = Up };
  "OB", { control = false; meta = false; shift = false; code = Down };
  "OC", { control = false; meta = false; shift = false; code = Right };
  "OD", { control = false; meta = false; shift = false; code = Left };

  "Oa", { control = true; meta = false; shift = false; code = Up };
  "Ob", { control = true; meta = false; shift = false; code = Down };
  "Oc", { control = true; meta = false; shift = false; code = Right };
  "Od", { control = true; meta = false; shift = false; code = Left };

  "OP", { control = false; meta = false; shift = false; code = F1 };
  "OQ", { control = false; meta = false; shift = false; code = F2 };
  "OR", { control = false; meta = false; shift = false; code = F3 };
  "OS", { control = false; meta = false; shift = false; code = F4 };

  "O2P", { control = false; meta = false; shift = true; code = F1 };
  "O2Q", { control = false; meta = false; shift = true; code = F2 };
  "O2R", { control = false; meta = false; shift = true; code = F3 };
  "O2S", { control = false; meta = false; shift = true; code = F4 };

  "O3P", { control = false; meta = true; shift = false; code = F1 };
  "O3Q", { control = false; meta = true; shift = false; code = F2 };
  "O3R", { control = false; meta = true; shift = false; code = F3 };
  "O3S", { control = false; meta = true; shift = false; code = F4 };

  "O4P", { control = false; meta = true; shift = true; code = F1 };
  "O4Q", { control = false; meta = true; shift = true; code = F2 };
  "O4R", { control = false; meta = true; shift = true; code = F3 };
  "O4S", { control = false; meta = true; shift = true; code = F4 };

  "O5P", { control = true; meta = false; shift = false; code = F1 };
  "O5Q", { control = true; meta = false; shift = false; code = F2 };
  "O5R", { control = true; meta = false; shift = false; code = F3 };
  "O5S", { control = true; meta = false; shift = false; code = F4 };

  "O6P", { control = true; meta = false; shift = true; code = F1 };
  "O6Q", { control = true; meta = false; shift = true; code = F2 };
  "O6R", { control = true; meta = false; shift = true; code = F3 };
  "O6S", { control = true; meta = false; shift = true; code = F4 };

  "O7P", { control = true; meta = true; shift = false; code = F1 };
  "O7Q", { control = true; meta = true; shift = false; code = F2 };
  "O7R", { control = true; meta = true; shift = false; code = F3 };
  "O7S", { control = true; meta = true; shift = false; code = F4 };

  "O8P", { control = true; meta = true; shift = true; code = F1 };
  "O8Q", { control = true; meta = true; shift = true; code = F2 };
  "O8R", { control = true; meta = true; shift = true; code = F3 };
  "O8S", { control = true; meta = true; shift = true; code = F4 };

  "[[A", { control = false; meta = false; shift = false; code = F1 };
  "[[B", { control = false; meta = false; shift = false; code = F2 };
  "[[C", { control = false; meta = false; shift = false; code = F3 };
  "[[D", { control = false; meta = false; shift = false; code = F4 };
  "[[E", { control = false; meta = false; shift = false; code = F5 };

  "[H", { control = false; meta = false; shift = false; code = Home };
  "[F", { control = false; meta = false; shift = false; code = End };

  "OH", { control = false; meta = false; shift = false; code = Home };
  "OF", { control = false; meta = false; shift = false; code = End };

  "H", { control = false; meta = false; shift = false; code = Home };
  "F", { control = false; meta = false; shift = false; code = End };

  "[1;2A", { control = false; meta = false; shift = true; code = Up };
  "[1;2B", { control = false; meta = false; shift = true; code = Down };
  "[1;2C", { control = false; meta = false; shift = true; code = Right };
  "[1;2D", { control = false; meta = false; shift = true; code = Left };

  "[1;3A", { control = false; meta = true; shift = false; code = Up };
  "[1;3B", { control = false; meta = true; shift = false; code = Down };
  "[1;3C", { control = false; meta = true; shift = false; code = Right };
  "[1;3D", { control = false; meta = true; shift = false; code = Left };

  "[1;4A", { control = false; meta = true; shift = true; code = Up };
  "[1;4B", { control = false; meta = true; shift = true; code = Down };
  "[1;4C", { control = false; meta = true; shift = true; code = Right };
  "[1;4D", { control = false; meta = true; shift = true; code = Left };

  "[1;5A", { control = true; meta = false; shift = false; code = Up };
  "[1;5B", { control = true; meta = false; shift = false; code = Down };
  "[1;5C", { control = true; meta = false; shift = false; code = Right };
  "[1;5D", { control = true; meta = false; shift = false; code = Left };

  "[1;6A", { control = true; meta = false; shift = true; code = Up };
  "[1;6B", { control = true; meta = false; shift = true; code = Down };
  "[1;6C", { control = true; meta = false; shift = true; code = Right };
  "[1;6D", { control = true; meta = false; shift = true; code = Left };

  "[1;7A", { control = true; meta = true; shift = false; code = Up };
  "[1;7B", { control = true; meta = true; shift = false; code = Down };
  "[1;7C", { control = true; meta = true; shift = false; code = Right };
  "[1;7D", { control = true; meta = true; shift = false; code = Left };

  "[1;8A", { control = true; meta = true; shift = true; code = Up };
  "[1;8B", { control = true; meta = true; shift = true; code = Down };
  "[1;8C", { control = true; meta = true; shift = true; code = Right };
  "[1;8D", { control = true; meta = true; shift = true; code = Left };

  "[1;2P", { control = false; meta = false; shift = true; code = F1 };
  "[1;2Q", { control = false; meta = false; shift = true; code = F2 };
  "[1;2R", { control = false; meta = false; shift = true; code = F3 };
  "[1;2S", { control = false; meta = false; shift = true; code = F4 };

  "[1;3P", { control = false; meta = true; shift = false; code = F1 };
  "[1;3Q", { control = false; meta = true; shift = false; code = F2 };
  "[1;3R", { control = false; meta = true; shift = false; code = F3 };
  "[1;3S", { control = false; meta = true; shift = false; code = F4 };

  "[1;4P", { control = false; meta = true; shift = true; code = F1 };
  "[1;4Q", { control = false; meta = true; shift = true; code = F2 };
  "[1;4R", { control = false; meta = true; shift = true; code = F3 };
  "[1;4S", { control = false; meta = true; shift = true; code = F4 };

  "[1;5P", { control = true; meta = false; shift = false; code = F1 };
  "[1;5Q", { control = true; meta = false; shift = false; code = F2 };
  "[1;5R", { control = true; meta = false; shift = false; code = F3 };
  "[1;5S", { control = true; meta = false; shift = false; code = F4 };

  "[1;6P", { control = true; meta = false; shift = true; code = F1 };
  "[1;6Q", { control = true; meta = false; shift = true; code = F2 };
  "[1;6R", { control = true; meta = false; shift = true; code = F3 };
  "[1;6S", { control = true; meta = false; shift = true; code = F4 };

  "[1;7P", { control = true; meta = true; shift = false; code = F1 };
  "[1;7Q", { control = true; meta = true; shift = false; code = F2 };
  "[1;7R", { control = true; meta = true; shift = false; code = F3 };
  "[1;7S", { control = true; meta = true; shift = false; code = F4 };

  "[1;8P", { control = true; meta = true; shift = true; code = F1 };
  "[1;8Q", { control = true; meta = true; shift = true; code = F2 };
  "[1;8R", { control = true; meta = true; shift = true; code = F3 };
  "[1;8S", { control = true; meta = true; shift = true; code = F4 };

  "O1;2P", { control = false; meta = false; shift = true; code = F1 };
  "O1;2Q", { control = false; meta = false; shift = true; code = F2 };
  "O1;2R", { control = false; meta = false; shift = true; code = F3 };
  "O1;2S", { control = false; meta = false; shift = true; code = F4 };

  "O1;3P", { control = false; meta = true; shift = false; code = F1 };
  "O1;3Q", { control = false; meta = true; shift = false; code = F2 };
  "O1;3R", { control = false; meta = true; shift = false; code = F3 };
  "O1;3S", { control = false; meta = true; shift = false; code = F4 };

  "O1;4P", { control = false; meta = true; shift = true; code = F1 };
  "O1;4Q", { control = false; meta = true; shift = true; code = F2 };
  "O1;4R", { control = false; meta = true; shift = true; code = F3 };
  "O1;4S", { control = false; meta = true; shift = true; code = F4 };

  "O1;5P", { control = true; meta = false; shift = false; code = F1 };
  "O1;5Q", { control = true; meta = false; shift = false; code = F2 };
  "O1;5R", { control = true; meta = false; shift = false; code = F3 };
  "O1;5S", { control = true; meta = false; shift = false; code = F4 };

  "O1;6P", { control = true; meta = false; shift = true; code = F1 };
  "O1;6Q", { control = true; meta = false; shift = true; code = F2 };
  "O1;6R", { control = true; meta = false; shift = true; code = F3 };
  "O1;6S", { control = true; meta = false; shift = true; code = F4 };

  "O1;7P", { control = true; meta = true; shift = false; code = F1 };
  "O1;7Q", { control = true; meta = true; shift = false; code = F2 };
  "O1;7R", { control = true; meta = true; shift = false; code = F3 };
  "O1;7S", { control = true; meta = true; shift = false; code = F4 };

  "O1;8P", { control = true; meta = true; shift = true; code = F1 };
  "O1;8Q", { control = true; meta = true; shift = true; code = F2 };
  "O1;8R", { control = true; meta = true; shift = true; code = F3 };
  "O1;8S", { control = true; meta = true; shift = true; code = F4 };

  "[15;2~", { control = false; meta = false; shift = true; code = F5 };
  "[17;2~", { control = false; meta = false; shift = true; code = F6 };
  "[18;2~", { control = false; meta = false; shift = true; code = F7 };
  "[19;2~", { control = false; meta = false; shift = true; code = F8 };
  "[20;2~", { control = false; meta = false; shift = true; code = F9 };
  "[21;2~", { control = false; meta = false; shift = true; code = F10 };
  "[23;2~", { control = false; meta = false; shift = true; code = F11 };
  "[24;2~", { control = false; meta = false; shift = true; code = F12 };

  "[15;3~", { control = false; meta = true; shift = false; code = F5 };
  "[17;3~", { control = false; meta = true; shift = false; code = F6 };
  "[18;3~", { control = false; meta = true; shift = false; code = F7 };
  "[19;3~", { control = false; meta = true; shift = false; code = F8 };
  "[20;3~", { control = false; meta = true; shift = false; code = F9 };
  "[21;3~", { control = false; meta = true; shift = false; code = F10 };
  "[23;3~", { control = false; meta = true; shift = false; code = F11 };
  "[24;3~", { control = false; meta = true; shift = false; code = F12 };

  "[15;4~", { control = false; meta = true; shift = true; code = F5 };
  "[17;4~", { control = false; meta = true; shift = true; code = F6 };
  "[18;4~", { control = false; meta = true; shift = true; code = F7 };
  "[19;4~", { control = false; meta = true; shift = true; code = F8 };
  "[20;4~", { control = false; meta = true; shift = true; code = F9 };
  "[21;4~", { control = false; meta = true; shift = true; code = F10 };
  "[23;4~", { control = false; meta = true; shift = true; code = F11 };
  "[24;4~", { control = false; meta = true; shift = true; code = F12 };

  "[15;5~", { control = true; meta = false; shift = false; code = F5 };
  "[17;5~", { control = true; meta = false; shift = false; code = F6 };
  "[18;5~", { control = true; meta = false; shift = false; code = F7 };
  "[19;5~", { control = true; meta = false; shift = false; code = F8 };
  "[20;5~", { control = true; meta = false; shift = false; code = F9 };
  "[21;5~", { control = true; meta = false; shift = false; code = F10 };
  "[23;5~", { control = true; meta = false; shift = false; code = F11 };
  "[24;5~", { control = true; meta = false; shift = false; code = F12 };

  "[15;6~", { control = true; meta = false; shift = true; code = F5 };
  "[17;6~", { control = true; meta = false; shift = true; code = F6 };
  "[18;6~", { control = true; meta = false; shift = true; code = F7 };
  "[19;6~", { control = true; meta = false; shift = true; code = F8 };
  "[20;6~", { control = true; meta = false; shift = true; code = F9 };
  "[21;6~", { control = true; meta = false; shift = true; code = F10 };
  "[23;6~", { control = true; meta = false; shift = true; code = F11 };
  "[24;6~", { control = true; meta = false; shift = true; code = F12 };

  "[15;7~", { control = true; meta = true; shift = false; code = F5 };
  "[17;7~", { control = true; meta = true; shift = false; code = F6 };
  "[18;7~", { control = true; meta = true; shift = false; code = F7 };
  "[19;7~", { control = true; meta = true; shift = false; code = F8 };
  "[20;7~", { control = true; meta = true; shift = false; code = F9 };
  "[21;7~", { control = true; meta = true; shift = false; code = F10 };
  "[23;7~", { control = true; meta = true; shift = false; code = F11 };
  "[24;7~", { control = true; meta = true; shift = false; code = F12 };

  "[15;8~", { control = true; meta = true; shift = true; code = F5 };
  "[17;8~", { control = true; meta = true; shift = true; code = F6 };
  "[18;8~", { control = true; meta = true; shift = true; code = F7 };
  "[19;8~", { control = true; meta = true; shift = true; code = F8 };
  "[20;8~", { control = true; meta = true; shift = true; code = F9 };
  "[21;8~", { control = true; meta = true; shift = true; code = F10 };
  "[23;8~", { control = true; meta = true; shift = true; code = F11 };
  "[24;8~", { control = true; meta = true; shift = true; code = F12 };

  "[1;2H", { control = false; meta = false; shift = true; code = Home };
  "[1;2F", { control = false; meta = false; shift = true; code = End };

  "[1;3H", { control = false; meta = true; shift = false; code = Home };
  "[1;3F", { control = false; meta = true; shift = false; code = End };

  "[1;4H", { control = false; meta = true; shift = true; code = Home };
  "[1;4F", { control = false; meta = true; shift = true; code = End };

  "[1;5H", { control = true; meta = false; shift = false; code = Home };
  "[1;5F", { control = true; meta = false; shift = false; code = End };

  "[1;6H", { control = true; meta = false; shift = true; code = Home };
  "[1;6F", { control = true; meta = false; shift = true; code = End };

  "[1;7H", { control = true; meta = true; shift = false; code = Home };
  "[1;7F", { control = true; meta = true; shift = false; code = End };

  "[1;8H", { control = true; meta = true; shift = true; code = Home };
  "[1;8F", { control = true; meta = true; shift = true; code = End };

  "[2;2~", { control = false; meta = false; shift = true; code = Insert };
  "[3;2~", { control = false; meta = false; shift = true; code = Delete };
  "[5;2~", { control = false; meta = false; shift = true; code = Prev_page };
  "[6;2~", { control = false; meta = false; shift = true; code = Next_page };

  "[2;3~", { control = false; meta = true; shift = false; code = Insert };
  "[3;3~", { control = false; meta = true; shift = false; code = Delete };
  "[5;3~", { control = false; meta = true; shift = false; code = Prev_page };
  "[6;3~", { control = false; meta = true; shift = false; code = Next_page };

  "[2;4~", { control = false; meta = true; shift = true; code = Insert };
  "[3;4~", { control = false; meta = true; shift = true; code = Delete };
  "[5;4~", { control = false; meta = true; shift = true; code = Prev_page };
  "[6;4~", { control = false; meta = true; shift = true; code = Next_page };

  "[2;5~", { control = true; meta = false; shift = false; code = Insert };
  "[3;5~", { control = true; meta = false; shift = false; code = Delete };
  "[5;5~", { control = true; meta = false; shift = false; code = Prev_page };
  "[6;5~", { control = true; meta = false; shift = false; code = Next_page };

  "[2;6~", { control = true; meta = false; shift = true; code = Insert };
  "[3;6~", { control = true; meta = false; shift = true; code = Delete };
  "[5;6~", { control = true; meta = false; shift = true; code = Prev_page };
  "[6;6~", { control = true; meta = false; shift = true; code = Next_page };

  "[2;7~", { control = true; meta = true; shift = false; code = Insert };
  "[3;7~", { control = true; meta = true; shift = false; code = Delete };
  "[5;7~", { control = true; meta = true; shift = false; code = Prev_page };
  "[6;7~", { control = true; meta = true; shift = false; code = Next_page };

  "[2;8~", { control = true; meta = true; shift = true; code = Insert };
  "[3;8~", { control = true; meta = true; shift = true; code = Delete };
  "[5;8~", { control = true; meta = true; shift = true; code = Prev_page };
  "[6;8~", { control = true; meta = true; shift = true; code = Next_page };
|]

let () = Array.sort (fun (seq1, _) (seq2, _) -> String.compare seq1 seq2) sequences

let find_sequence seq =
  let rec loop a b =
    if a = b then
      None
    else
      let c = (a + b) / 2 in
      let k, v = Array.unsafe_get sequences c in
      match String.compare seq k with
        | d when d < 0 ->
            loop a c
        | d when d > 0 ->
            loop (c + 1) b
        | _ ->
            Some v
  in
  loop 0 (Array.length sequences)

let rec parse_event ?(escape_time = 0.1) encoding stream =
  lwt byte = Lwt_stream.next stream in
  match byte with
    | '\x1b' -> begin
        (* Escape sequences *)
        try_lwt
          (* Try to parse an escape seqsuence *)
          Lwt_stream.parse stream (parse_escape escape_time) >>= function
            | "[M" -> begin
                (* Mouse report *)
                let open LTerm_mouse in
                lwt mask = Lwt_stream.next stream >|= Char.code in
                lwt x = Lwt_stream.next stream >|= Char.code in
                lwt y = Lwt_stream.next stream >|= Char.code in
                try
                  if mask = 0b00100011 then raise Exit;
                  return (LTerm_event.Mouse {
                            control = mask land 0b00010000 <> 0;
                            meta = mask land 0b00001000 <> 0;
                            shift = false;
                            row = y - 33;
                            col = x - 33;
                            button =
                              (match mask land 0b11000111 with
                                 | 0b00000000 -> Button1
                                 | 0b00000001 -> Button2
                                 | 0b00000010 -> Button3
                                 | 0b01000000 -> Button4
                                 | 0b01000001 -> Button5
                                 | 0b01000010 -> Button6
                                 | 0b01000011 -> Button7
                                 | 0b01000100 -> Button8
                                 | 0b01000101 -> Button9
                                 | _ -> raise Exit);
                          })
                with Exit ->
                  parse_event encoding stream
              end
            | seq ->
                match find_sequence seq with
                  | Some key ->
                      return (LTerm_event.Key key)
                  | None ->
                      return (LTerm_event.Sequence ("\x1b" ^ seq))
        with Not_a_sequence ->
          (* If it is not, test if it is META+key. *)
          match_lwt pick [Lwt_stream.peek stream; Lwt_unix.sleep escape_time >> return None] with
            | None ->
                return (LTerm_event.Key { control = false; meta = false; shift = false; code = Escape })
            | Some byte ->
                match byte with
                  | '\x1b' -> begin
                      (* Escape sequences *)
                      try_lwt
                        lwt seq =
                          Lwt_stream.parse stream
                            (fun stream ->
                               lwt () = Lwt_stream.junk stream in
                               match_lwt pick [Lwt_stream.peek stream; Lwt_unix.sleep escape_time >> return None] with
                                 | None ->
                                     raise_lwt Not_a_sequence
                                 | Some _ ->
                                     parse_escape escape_time stream)
                        in
                        match find_sequence seq with
                          | Some key ->
                              return (LTerm_event.Key { key with meta = true })
                          | None ->
                              return (LTerm_event.Sequence ("\x1b\x1b" ^ seq))
                      with Not_a_sequence ->
                        return (LTerm_event.Key { control = false; meta = false; shift = false; code = Escape })
                    end
                  | '\x00' .. '\x1b' ->
                      (* Control characters *)
                      lwt () = Lwt_stream.junk stream in
                      let code = controls.(Char.code byte) in
                      return (LTerm_event.Key { control = (match code with Char _ -> true | _ -> false); meta = true; shift = false; code })
                  | '\x7f' ->
                      (* Backspace *)
                      lwt () = Lwt_stream.junk stream in
                      return (LTerm_event.Key { control = false; meta = true; shift = false; code = Backspace })
                  | '\x00' .. '\x7f' ->
                      (* Other ascii characters *)
                      lwt () = Lwt_stream.junk stream in
                      return(LTerm_event.Key  { control = false; meta = true; shift = false; code = Char(UChar.of_char byte) })
                  | byte' ->
                      lwt () = Lwt_stream.junk stream in
                      lwt code = parse_char encoding stream byte' in
                      return (LTerm_event.Key { control = false; meta = true; shift = false; code = Char code })
      end
    | '\x00' .. '\x1f' ->
        (* Control characters *)
        let code = controls.(Char.code byte) in
        return (LTerm_event.Key { control = (match code with Char _ -> true | _ -> false); meta = false; shift = false; code })
    | '\x7f' ->
        (* Backspace *)
        return (LTerm_event.Key { control = false; meta = false; shift = false; code = Backspace })
    | '\x00' .. '\x7f' ->
        (* Other ascii characters *)
        return (LTerm_event.Key { control = false; meta = false; shift = false; code = Char(UChar.of_char byte) })
    | _ ->
        (* Encoded characters *)
        lwt code = parse_char encoding stream byte in
        return (LTerm_event.Key { control = false; meta = false; shift = false; code = Char code })
