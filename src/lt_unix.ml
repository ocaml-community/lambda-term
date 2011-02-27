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
external get_system_encoding : unit -> string = "lt_unix_get_system_encoding"

let sigwinch = get_sigwinch ()
let system_encoding = get_system_encoding ()

(* +-----------------------------------------------------------------+
   | Parsing of encoded characters                                   |
   +-----------------------------------------------------------------+ *)

exception Fallback

let parse_char cd st first_byte =
  let open Lt_iconv in
  let src = {
    bytes = String.create 128;
    index = 0;
    limit = 1;
  }
  and dst = {
    bytes = String.create 4;
    index = 0;
    limit = 4;
  } in
  let rec loop st =
    try
      iconv cd ~src ~dst;
      return ((Char.code dst.bytes.[0] lsl 24)
              lor (Char.code dst.bytes.[1] lsl 16)
              lor (Char.code dst.bytes.[2] lsl 8)
              lor (Char.code dst.bytes.[3]))
    with
      | Invalid_sequence ->
          raise_lwt Fallback
      | Unterminated_sequence ->
          Lwt_stream.get st >>= function
            | Some ch ->
                src.bytes.[src.limit] <- ch;
                src.limit <- src.limit + 1;
                loop st
            | None ->
                raise_lwt Fallback
  in
  try_lwt
    src.bytes.[0] <- first_byte;
    reset cd;
    Lwt_stream.parse st loop
  with
    | Fallback ->
        return (Char.code first_byte)

(* +-----------------------------------------------------------------+
   | Input of escape sequence                                        |
   +-----------------------------------------------------------------+ *)

exception Not_a_sequence

let parse_escape st =
  let buf = Buffer.create 32 in
  (* Read one character and add it to [buf]: *)
  let get () =
    match Lwt.state (Lwt_stream.get st) with
      | Sleep ->
          (* If the rest is not immediatly available, conclude that
             this is not an escape sequence but just the escape key: *)
          raise_lwt Not_a_sequence
      | Fail exn ->
          raise_lwt exn
      | Return None ->
          raise_lwt Not_a_sequence
      | Return(Some ch) ->
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
        return (Buffer.contents buf)

(* +-----------------------------------------------------------------+
   | Escape sequences mapping                                        |
   +-----------------------------------------------------------------+ *)

let controls = [|
  Char(Char.code ' ');
  Char(Char.code 'a');
  Char(Char.code 'b');
  Char(Char.code 'c');
  Char(Char.code 'd');
  Char(Char.code 'e');
  Char(Char.code 'f');
  Char(Char.code 'g');
  Char(Char.code 'h');
  Tab;
  Enter;
  Char(Char.code 'k');
  Char(Char.code 'l');
  Char(Char.code 'm');
  Char(Char.code 'n');
  Char(Char.code 'o');
  Char(Char.code 'p');
  Char(Char.code 'q');
  Char(Char.code 'r');
  Char(Char.code 's');
  Char(Char.code 't');
  Char(Char.code 'u');
  Char(Char.code 'v');
  Char(Char.code 'w');
  Char(Char.code 'x');
  Char(Char.code 'y');
  Char(Char.code 'z');
  Escape;
  Char(Char.code '\\');
  Char(Char.code ']');
  Char(Char.code '^');
  Char(Char.code '_');
|]

let sequences = [|
  "[1~", { ctrl = false; meta = false; code = Home };
  "[2~", { ctrl = false; meta = false; code = Insert };
  "[3~", { ctrl = false; meta = false; code = Delete };
  "[4~", { ctrl = false; meta = false; code = End };
  "[5~", { ctrl = false; meta = false; code = Prev_page };
  "[6~", { ctrl = false; meta = false; code = Next_page };
  "[7~", { ctrl = false; meta = false; code = Home };
  "[8~", { ctrl = false; meta = false; code = End };
  "[11~", { ctrl = false; meta = false; code = F1 };
  "[12~", { ctrl = false; meta = false; code = F2 };
  "[13~", { ctrl = false; meta = false; code = F3 };
  "[14~", { ctrl = false; meta = false; code = F4 };
  "[15~", { ctrl = false; meta = false; code = F5 };
  "[17~", { ctrl = false; meta = false; code = F6 };
  "[18~", { ctrl = false; meta = false; code = F7 };
  "[19~", { ctrl = false; meta = false; code = F8 };
  "[20~", { ctrl = false; meta = false; code = F9 };
  "[21~", { ctrl = false; meta = false; code = F10 };
  "[23~", { ctrl = false; meta = false; code = F11 };
  "[24~", { ctrl = false; meta = false; code = F12 };

  "[1^", { ctrl = true; meta = false; code = Home };
  "[2^", { ctrl = true; meta = false; code = Insert };
  "[3^", { ctrl = true; meta = false; code = Delete };
  "[4^", { ctrl = true; meta = false; code = End };
  "[5^", { ctrl = true; meta = false; code = Prev_page };
  "[6^", { ctrl = true; meta = false; code = Next_page };
  "[7^", { ctrl = true; meta = false; code = Home };
  "[8^", { ctrl = true; meta = false; code = End };
  "[11^", { ctrl = true; meta = false; code = F1 };
  "[12^", { ctrl = true; meta = false; code = F2 };
  "[13^", { ctrl = true; meta = false; code = F3 };
  "[14^", { ctrl = true; meta = false; code = F4 };
  "[15^", { ctrl = true; meta = false; code = F5 };
  "[17^", { ctrl = true; meta = false; code = F6 };
  "[18^", { ctrl = true; meta = false; code = F7 };
  "[19^", { ctrl = true; meta = false; code = F8 };
  "[20^", { ctrl = true; meta = false; code = F9 };
  "[21^", { ctrl = true; meta = false; code = F10 };
  "[23^", { ctrl = true; meta = false; code = F11 };
  "[24^", { ctrl = true; meta = false; code = F12 };

  "[A", { ctrl = false; meta = false; code = Up };
  "[B", { ctrl = false; meta = false; code = Down };
  "[C", { ctrl = false; meta = false; code = Right };
  "[D", { ctrl = false; meta = false; code = Left };

  "A", { ctrl = false; meta = false; code = Up };
  "B", { ctrl = false; meta = false; code = Down };
  "C", { ctrl = false; meta = false; code = Right };
  "D", { ctrl = false; meta = false; code = Left };

  "OA", { ctrl = false; meta = false; code = Up };
  "OB", { ctrl = false; meta = false; code = Down };
  "OC", { ctrl = false; meta = false; code = Right };
  "OD", { ctrl = false; meta = false; code = Left };

  "Oa", { ctrl = true; meta = false; code = Up };
  "Ob", { ctrl = true; meta = false; code = Down };
  "Oc", { ctrl = true; meta = false; code = Right };
  "Od", { ctrl = true; meta = false; code = Left };

  "OP", { ctrl = false; meta = false; code = F1 };
  "OQ", { ctrl = false; meta = false; code = F2 };
  "OR", { ctrl = false; meta = false; code = F3 };
  "OS", { ctrl = false; meta = false; code = F4 };

  "O3P", { ctrl = false; meta = true; code = F1 };
  "O3Q", { ctrl = false; meta = true; code = F2 };
  "O3R", { ctrl = false; meta = true; code = F3 };
  "O3S", { ctrl = false; meta = true; code = F4 };

  "O5P", { ctrl = true; meta = false; code = F1 };
  "O5Q", { ctrl = true; meta = false; code = F2 };
  "O5R", { ctrl = true; meta = false; code = F3 };
  "O5S", { ctrl = true; meta = false; code = F4 };

  "[[A", { ctrl = false; meta = false; code = F1 };
  "[[B", { ctrl = false; meta = false; code = F2 };
  "[[C", { ctrl = false; meta = false; code = F3 };
  "[[D", { ctrl = false; meta = false; code = F4 };
  "[[E", { ctrl = false; meta = false; code = F5 };

  "[H", { ctrl = false; meta = false; code = Home };
  "[F", { ctrl = false; meta = false; code = End };

  "OH", { ctrl = false; meta = false; code = Home };
  "OF", { ctrl = false; meta = false; code = End };

  "H", { ctrl = false; meta = false; code = Home };
  "F", { ctrl = false; meta = false; code = End };

  "[1;3A", { ctrl = false; meta = true; code = Up };
  "[1;3B", { ctrl = false; meta = true; code = Down };
  "[1;3C", { ctrl = false; meta = true; code = Right };
  "[1;3D", { ctrl = false; meta = true; code = Left };

  "[1;5A", { ctrl = true; meta = false; code = Up };
  "[1;5B", { ctrl = true; meta = false; code = Down };
  "[1;5C", { ctrl = true; meta = false; code = Right };
  "[1;5D", { ctrl = true; meta = false; code = Left };

  "[1;7A", { ctrl = true; meta = true; code = Up };
  "[1;7B", { ctrl = true; meta = true; code = Down };
  "[1;7C", { ctrl = true; meta = true; code = Right };
  "[1;7D", { ctrl = true; meta = true; code = Left };

  "[1;3P", { ctrl = false; meta = true; code = F1 };
  "[1;3Q", { ctrl = false; meta = true; code = F2 };
  "[1;3R", { ctrl = false; meta = true; code = F3 };
  "[1;3S", { ctrl = false; meta = true; code = F4 };

  "[1;5P", { ctrl = true; meta = false; code = F1 };
  "[1;5Q", { ctrl = true; meta = false; code = F2 };
  "[1;5R", { ctrl = true; meta = false; code = F3 };
  "[1;5S", { ctrl = true; meta = false; code = F4 };

  "O1;3P", { ctrl = false; meta = true; code = F1 };
  "O1;3Q", { ctrl = false; meta = true; code = F2 };
  "O1;3R", { ctrl = false; meta = true; code = F3 };
  "O1;3S", { ctrl = false; meta = true; code = F4 };

  "O1;5P", { ctrl = true; meta = false; code = F1 };
  "O1;5Q", { ctrl = true; meta = false; code = F2 };
  "O1;5R", { ctrl = true; meta = false; code = F3 };
  "O1;5S", { ctrl = true; meta = false; code = F4 };

  "[15;3~", { ctrl = false; meta = true; code = F5 };
  "[17;3~", { ctrl = false; meta = true; code = F6 };
  "[18;3~", { ctrl = false; meta = true; code = F7 };
  "[19;3~", { ctrl = false; meta = true; code = F8 };
  "[20;3~", { ctrl = false; meta = true; code = F9 };
  "[21;3~", { ctrl = false; meta = true; code = F10 };
  "[23;3~", { ctrl = false; meta = true; code = F11 };
  "[24;3~", { ctrl = false; meta = true; code = F12 };

  "[15;5~", { ctrl = true; meta = false; code = F5 };
  "[17;5~", { ctrl = true; meta = false; code = F6 };
  "[18;5~", { ctrl = true; meta = false; code = F7 };
  "[19;5~", { ctrl = true; meta = false; code = F8 };
  "[20;5~", { ctrl = true; meta = false; code = F9 };
  "[21;5~", { ctrl = true; meta = false; code = F10 };
  "[23;5~", { ctrl = true; meta = false; code = F11 };
  "[24;5~", { ctrl = true; meta = false; code = F12 };

  "[1;3H", { ctrl = false; meta = true; code = Home };
  "[1;3F", { ctrl = false; meta = true; code = End };

  "[1;5H", { ctrl = true; meta = false; code = Home };
  "[1;5F", { ctrl = true; meta = false; code = End };

  "[1;7H", { ctrl = true; meta = true; code = Home };
  "[1;7F", { ctrl = true; meta = true; code = End };

  "[2;3~", { ctrl = false; meta = true; code = Insert };
  "[3;3~", { ctrl = false; meta = true; code = Delete };
  "[5;3~", { ctrl = false; meta = true; code = Prev_page };
  "[6;3~", { ctrl = false; meta = true; code = Next_page };

  "[2;5~", { ctrl = true; meta = false; code = Insert };
  "[3;5~", { ctrl = true; meta = false; code = Delete };
  "[5;5~", { ctrl = true; meta = false; code = Prev_page };
  "[6;5~", { ctrl = true; meta = false; code = Next_page };

  "[2;7~", { ctrl = true; meta = true; code = Insert };
  "[3;7~", { ctrl = true; meta = true; code = Delete };
  "[5;7~", { ctrl = true; meta = true; code = Prev_page };
  "[6;7~", { ctrl = true; meta = true; code = Next_page };
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

let parse_event cd stream =
  lwt byte = Lwt_stream.next stream in
  match byte with
    | '\x1b' -> begin
        (* Escape sequences *)
        try_lwt
          (* Try to parse an escape seqsuence *)
          lwt seq = Lwt_stream.parse stream parse_escape in
          match find_sequence seq with
            | Some key ->
                return (Lt_event.Key key)
            | None ->
                return (Lt_event.Sequence ("\x1b" ^ seq))
        with Not_a_sequence ->
          (* If it is not, test if it is META+key. *)
          match Lwt.state (Lwt_stream.peek stream) with
            | Sleep | Fail _ | Return None ->
                return (Lt_event.Key { ctrl = false; meta = false; code = Escape })
            | Return(Some byte) ->
                match byte with
                  | '\x1b' -> begin
                      (* Escape sequences *)
                      try_lwt
                        lwt seq = Lwt_stream.parse stream (fun stream -> Lwt_stream.junk stream >> parse_escape stream) in
                        match find_sequence seq with
                          | Some key ->
                              return (Lt_event.Key { key with meta = true })
                          | None ->
                              return (Lt_event.Sequence ("\x1b\x1b" ^ seq))
                      with Not_a_sequence ->
                        return (Lt_event.Key { ctrl = false; meta = false; code = Escape })
                    end
                  | '\x00' .. '\x1b' ->
                      (* Control characters *)
                      lwt () = Lwt_stream.junk stream in
                      return (Lt_event.Key { ctrl = true; meta = true; code = controls.(Char.code byte) })
                  | '\x7f' ->
                      (* Backspace *)
                      lwt () = Lwt_stream.junk stream in
                      return (Lt_event.Key { ctrl = false; meta = true; code = Backspace })
                  | '\x00' .. '\x7f' ->
                      (* Other ascii characters *)
                      lwt () = Lwt_stream.junk stream in
                      return(Lt_event.Key  { ctrl = false; meta = true; code = Char(Char.code byte) })
                  | byte' ->
                      lwt () = Lwt_stream.junk stream in
                      lwt code = parse_char cd stream byte' in
                      return (Lt_event.Key { ctrl = false; meta = true; code = Char code })
      end
    | '\x00' .. '\x1f' ->
        (* Control characters *)
        return (Lt_event.Key { ctrl = true; meta = false; code = controls.(Char.code byte) })
    | '\x7f' ->
        (* Backspace *)
        return (Lt_event.Key { ctrl = false; meta = false; code = Backspace })
    | '\x00' .. '\x7f' ->
        (* Other ascii characters *)
        return (Lt_event.Key { ctrl = false; meta = false; code = Char(Char.code byte) })
    | _ ->
        (* Encoded characters *)
        lwt code = parse_char cd stream byte in
        return (Lt_event.Key { ctrl = false; meta = false; code = Char code })
