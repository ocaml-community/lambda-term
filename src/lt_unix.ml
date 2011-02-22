(*
 * lt_unix.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open Lt_key

external get_sigwinch : unit -> int option = "lt_unix_get_sigwinch"

let sigwinch = get_sigwinch ()

(* +-----------------------------------------------------------------+
   | Input of escape sequence                                        |
   +-----------------------------------------------------------------+ *)

exception Exit_sequence

let parse_escape st =
  let buf = Buffer.create 32 in
  Buffer.add_char buf '\027';
  (* Read one character and add it to [buf]: *)
  let get () =
    match Lwt.state (Lwt_stream.get st) with
      | Sleep ->
          (* If the rest is not immediatly available, conclude that
             this is not an escape sequence but just the escape key: *)
          raise_lwt Exit_sequence
      | Fail exn ->
          raise_lwt exn
      | Return None ->
          raise_lwt Exit_sequence
      | Return(Some ch) ->
          let code = Text.code ch in
          (* Is it an ascii character ? *)
          if code < 128 then begin
            let ch = Char.unsafe_chr code in
            Buffer.add_char buf ch;
            return ch
          end else
            (* If it is not, then this is not an escape sequence: *)
            raise_lwt Exit_sequence
  in

  (* Sometimes sequences starts with several escape characters: *)
  let rec first count =
    get () >>= function
      | '\x1b' when count < 3 ->
          first (count + 1)
      | ch ->
          return ch
  in

  first 0 >>= function
    | '[' | 'O' ->
        let rec loop () =
          get () >>= function
            | '0' .. '9' | ';' ->
                loop ()
            | ch ->
                return (Buffer.contents buf)
        in
        loop ()

    | ch ->
        return (Buffer.contents buf)

let get_sequence st =
  Lwt_stream.next st >>= function
    | "\027" -> begin
        try_lwt
          Lwt_stream.parse st parse_escape
        with Exit_sequence ->
          return "\027"
      end
    | ch ->
        return ch

(* +-----------------------------------------------------------------+
   | Escape sequences mapping                                        |
   +-----------------------------------------------------------------+ *)

let sequences = [|
  "\x01", ({ control = true; meta = false }, Char " ");
  "\x01", ({ control = true; meta = false }, Char "a");
  "\x02", ({ control = true; meta = false }, Char "b");
  "\x03", ({ control = true; meta = false }, Char "c");
  "\x04", ({ control = true; meta = false }, Char "d");
  "\x05", ({ control = true; meta = false }, Char "e");
  "\x06", ({ control = true; meta = false }, Char "f");
  "\x07", ({ control = true; meta = false }, Char "g");
  "\x08", ({ control = true; meta = false }, Char "h");
  "\x09", ({ control = false; meta = false }, Tab);
  "\x0A", ({ control = false; meta = false }, Enter);
  "\x0B", ({ control = true; meta = false }, Char "k");
  "\x0C", ({ control = true; meta = false }, Char "l");
  "\x0D", ({ control = true; meta = false }, Char "m");
  "\x0E", ({ control = true; meta = false }, Char "n");
  "\x0F", ({ control = true; meta = false }, Char "o");
  "\x10", ({ control = true; meta = false }, Char "p");
  "\x11", ({ control = true; meta = false }, Char "q");
  "\x12", ({ control = true; meta = false }, Char "r");
  "\x13", ({ control = true; meta = false }, Char "s");
  "\x14", ({ control = true; meta = false }, Char "t");
  "\x15", ({ control = true; meta = false }, Char "u");
  "\x16", ({ control = true; meta = false }, Char "v");
  "\x17", ({ control = true; meta = false }, Char "w");
  "\x18", ({ control = true; meta = false }, Char "x");
  "\x19", ({ control = true; meta = false }, Char "y");
  "\x1A", ({ control = true; meta = false }, Char "z");
  "\x1B", ({ control = false; meta = false }, Escape);
  "\x1C", ({ control = true; meta = false }, Char "\\");
  "\x1D", ({ control = true; meta = false }, Char "]");
  "\x1E", ({ control = true; meta = false }, Char "^");
  "\x1F", ({ control = true; meta = false }, Char "_");
  "\x7F", ({ control = false; meta = false }, Backspace);

  "\027[1~", ({ control = false; meta = false }, Home);
  "\027[2~", ({ control = false; meta = false }, Insert);
  "\027[3~", ({ control = false; meta = false }, Delete);
  "\027[3~", ({ control = false; meta = false }, End);
  "\027[5~", ({ control = false; meta = false }, Prev_page);
  "\027[6~", ({ control = false; meta = false }, Next_page);
  "\027[7~", ({ control = false; meta = false }, Home);
  "\027[8~", ({ control = false; meta = false }, End);
  "\027[11~", ({ control = false; meta = false }, F1);
  "\027[12~", ({ control = false; meta = false }, F2);
  "\027[13~", ({ control = false; meta = false }, F3);
  "\027[14~", ({ control = false; meta = false }, F4);
  "\027[15~", ({ control = false; meta = false }, F5);
  "\027[17~", ({ control = false; meta = false }, F6);
  "\027[18~", ({ control = false; meta = false }, F7);
  "\027[19~", ({ control = false; meta = false }, F8);
  "\027[20~", ({ control = false; meta = false }, F9);
  "\027[21~", ({ control = false; meta = false }, F10);
  "\027[23~", ({ control = false; meta = false }, F11);
  "\027[24~", ({ control = false; meta = false }, F12);

  "\027[1^", ({ control = true; meta = false }, Home);
  "\027[2^", ({ control = true; meta = false }, Insert);
  "\027[3^", ({ control = true; meta = false }, Delete);
  "\027[3^", ({ control = true; meta = false }, End);
  "\027[5^", ({ control = true; meta = false }, Prev_page);
  "\027[6^", ({ control = true; meta = false }, Next_page);
  "\027[7^", ({ control = true; meta = false }, Home);
  "\027[8^", ({ control = true; meta = false }, End);
  "\027[11^", ({ control = true; meta = false }, F1);
  "\027[12^", ({ control = true; meta = false }, F2);
  "\027[13^", ({ control = true; meta = false }, F3);
  "\027[14^", ({ control = true; meta = false }, F4);
  "\027[15^", ({ control = true; meta = false }, F5);
  "\027[17^", ({ control = true; meta = false }, F6);
  "\027[18^", ({ control = true; meta = false }, F7);
  "\027[19^", ({ control = true; meta = false }, F8);
  "\027[20^", ({ control = true; meta = false }, F9);
  "\027[21^", ({ control = true; meta = false }, F10);
  "\027[23^", ({ control = true; meta = false }, F11);
  "\027[24^", ({ control = true; meta = false }, F12);

  "\027[A", ({ control = false; meta = false }, Up);
  "\027[B", ({ control = false; meta = false }, Down);
  "\027[C", ({ control = false; meta = false }, Right);
  "\027[D", ({ control = false; meta = false }, Left);

  "\027A", ({ control = false; meta = false }, Up);
  "\027B", ({ control = false; meta = false }, Down);
  "\027C", ({ control = false; meta = false }, Right);
  "\027D", ({ control = false; meta = false }, Left);

  "\027OA", ({ control = false; meta = false }, Up);
  "\027OB", ({ control = false; meta = false }, Down);
  "\027OC", ({ control = false; meta = false }, Right);
  "\027OD", ({ control = false; meta = false }, Left);

  "\027Oa", ({ control = true; meta = false }, Up);
  "\027Ob", ({ control = true; meta = false }, Down);
  "\027Oc", ({ control = true; meta = false }, Right);
  "\027Od", ({ control = true; meta = false }, Left);

  "\027OP", ({ control = false; meta = false }, F1);
  "\027OQ", ({ control = false; meta = false }, F2);
  "\027OR", ({ control = false; meta = false }, F3);
  "\027OS", ({ control = false; meta = false }, F4);

  "\027[H", ({ control = false; meta = false }, Home);
  "\027[F", ({ control = false; meta = false }, End);

  "\027OH", ({ control = false; meta = false }, Home);
  "\027OF", ({ control = false; meta = false }, End);

  "\027H", ({ control = false; meta = false }, Home);
  "\027F", ({ control = false; meta = false }, End);

  "\027[1;3A", ({ control = false; meta = true }, Up);
  "\027[1;3B", ({ control = false; meta = true }, Down);
  "\027[1;3C", ({ control = false; meta = true }, Right);
  "\027[1;3D", ({ control = false; meta = true }, Left);

  "\027[1;5A", ({ control = true; meta = false }, Up);
  "\027[1;5B", ({ control = true; meta = false }, Down);
  "\027[1;5C", ({ control = true; meta = false }, Right);
  "\027[1;5D", ({ control = true; meta = false }, Left);

  "\027[1;7A", ({ control = true; meta = true }, Up);
  "\027[1;7B", ({ control = true; meta = true }, Down);
  "\027[1;7C", ({ control = true; meta = true }, Right);
  "\027[1;7D", ({ control = true; meta = true }, Left);

  "\027[1;3P", ({ control = false; meta = true }, F1);
  "\027[1;3Q", ({ control = false; meta = true }, F2);
  "\027[1;3R", ({ control = false; meta = true }, F3);
  "\027[1;3S", ({ control = false; meta = true }, F4);

  "\027[1;5P", ({ control = true; meta = false }, F1);
  "\027[1;5Q", ({ control = true; meta = false }, F2);
  "\027[1;5R", ({ control = true; meta = false }, F3);
  "\027[1;5S", ({ control = true; meta = false }, F4);

  "\027O1;3P", ({ control = false; meta = true }, F1);
  "\027O1;3Q", ({ control = false; meta = true }, F2);
  "\027O1;3R", ({ control = false; meta = true }, F3);
  "\027O1;3S", ({ control = false; meta = true }, F4);

  "\027O1;5P", ({ control = true; meta = false }, F1);
  "\027O1;5Q", ({ control = true; meta = false }, F2);
  "\027O1;5R", ({ control = true; meta = false }, F3);
  "\027O1;5S", ({ control = true; meta = false }, F4);

  "\027[15;3~", ({ control = false; meta = true }, F5);
  "\027[17;3~", ({ control = false; meta = true }, F6);
  "\027[18;3~", ({ control = false; meta = true }, F7);
  "\027[19;3~", ({ control = false; meta = true }, F8);
  "\027[20;3~", ({ control = false; meta = true }, F9);
  "\027[21;3~", ({ control = false; meta = true }, F10);
  "\027[23;3~", ({ control = false; meta = true }, F11);
  "\027[24;3~", ({ control = false; meta = true }, F12);

  "\027[15;5~", ({ control = true; meta = false }, F5);
  "\027[17;5~", ({ control = true; meta = false }, F6);
  "\027[18;5~", ({ control = true; meta = false }, F7);
  "\027[19;5~", ({ control = true; meta = false }, F8);
  "\027[20;5~", ({ control = true; meta = false }, F9);
  "\027[21;5~", ({ control = true; meta = false }, F10);
  "\027[23;5~", ({ control = true; meta = false }, F11);
  "\027[24;5~", ({ control = true; meta = false }, F12);

  "\027[1;3H", ({ control = false; meta = true }, Home);
  "\027[1;3F", ({ control = false; meta = true }, End);

  "\027[1;5H", ({ control = true; meta = false }, Home);
  "\027[1;5F", ({ control = true; meta = false }, End);

  "\027[1;7H", ({ control = true; meta = true }, Home);
  "\027[1;7F", ({ control = true; meta = true }, End);

  "\027[2;3~", ({ control = false; meta = true }, Insert);
  "\027[3;3~", ({ control = false; meta = true }, Delete);
  "\027[5;3~", ({ control = false; meta = true }, Prev_page);
  "\027[6;3~", ({ control = false; meta = true }, Next_page);

  "\027[2;5~", ({ control = true; meta = false }, Insert);
  "\027[3;5~", ({ control = true; meta = false }, Delete);
  "\027[5;5~", ({ control = true; meta = false }, Prev_page);
  "\027[6;5~", ({ control = true; meta = false }, Next_page);

  "\027[2;7~", ({ control = true; meta = true }, Insert);
  "\027[3;7~", ({ control = true; meta = true }, Delete);
  "\027[5;7~", ({ control = true; meta = true }, Prev_page);
  "\027[6;7~", ({ control = true; meta = true }, Next_page);
|]

let () = Array.sort (fun (seq1, _) (seq2, _) -> String.compare seq1 seq2) sequences

let binary_search key arr =
  let rec loop a b =
    if a = b then
      None
    else
      let c = (a + b) / 2 in
      let k, v = Array.unsafe_get arr c in
      match String.compare key k with
        | d when d < 0 ->
            loop a c
        | d when d > 0 ->
            loop (c + 1) b
        | _ ->
            Some v
  in
  loop 0 (Array.length arr)

let parse_event seq =
  if Text.length seq = 0 then
    Lt_event.Key_sequence ""
  else
    match binary_search seq sequences with
      | Some(mods, key) ->
          Lt_event.Key(mods, key)
      | None ->
          if Text.length seq = 1 then
            Lt_event.Key({ control = false; meta = false }, Char seq)
          else if Text.get seq 0 = "\027" then
            match binary_search (Text.lchop seq) sequences with
              | Some(mods, key) ->
                  Lt_event.Key({ mods with meta = true }, key)
              | None ->
                  if Text.length seq = 2 then
                    Lt_event.Key({ control = false; meta = true }, Char(Text.get seq 1))
                  else
                    Lt_event.Key_sequence seq
          else
            Lt_event.Key_sequence seq
