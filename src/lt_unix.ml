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
      | Return(Some('\x00' .. '\x1f' | '\x80' .. '\xff')) ->
          (* Control characters and non-ascii characters are not part
             of escape sequences. *)
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
        raise_lwt Not_a_sequence

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
  "[1~", { control = false; meta = false; code = Home };
  "[2~", { control = false; meta = false; code = Insert };
  "[3~", { control = false; meta = false; code = Delete };
  "[4~", { control = false; meta = false; code = End };
  "[5~", { control = false; meta = false; code = Prev_page };
  "[6~", { control = false; meta = false; code = Next_page };
  "[7~", { control = false; meta = false; code = Home };
  "[8~", { control = false; meta = false; code = End };
  "[11~", { control = false; meta = false; code = F1 };
  "[12~", { control = false; meta = false; code = F2 };
  "[13~", { control = false; meta = false; code = F3 };
  "[14~", { control = false; meta = false; code = F4 };
  "[15~", { control = false; meta = false; code = F5 };
  "[17~", { control = false; meta = false; code = F6 };
  "[18~", { control = false; meta = false; code = F7 };
  "[19~", { control = false; meta = false; code = F8 };
  "[20~", { control = false; meta = false; code = F9 };
  "[21~", { control = false; meta = false; code = F10 };
  "[23~", { control = false; meta = false; code = F11 };
  "[24~", { control = false; meta = false; code = F12 };

  "[1^", { control = true; meta = false; code = Home };
  "[2^", { control = true; meta = false; code = Insert };
  "[3^", { control = true; meta = false; code = Delete };
  "[4^", { control = true; meta = false; code = End };
  "[5^", { control = true; meta = false; code = Prev_page };
  "[6^", { control = true; meta = false; code = Next_page };
  "[7^", { control = true; meta = false; code = Home };
  "[8^", { control = true; meta = false; code = End };
  "[11^", { control = true; meta = false; code = F1 };
  "[12^", { control = true; meta = false; code = F2 };
  "[13^", { control = true; meta = false; code = F3 };
  "[14^", { control = true; meta = false; code = F4 };
  "[15^", { control = true; meta = false; code = F5 };
  "[17^", { control = true; meta = false; code = F6 };
  "[18^", { control = true; meta = false; code = F7 };
  "[19^", { control = true; meta = false; code = F8 };
  "[20^", { control = true; meta = false; code = F9 };
  "[21^", { control = true; meta = false; code = F10 };
  "[23^", { control = true; meta = false; code = F11 };
  "[24^", { control = true; meta = false; code = F12 };

  "[A", { control = false; meta = false; code = Up };
  "[B", { control = false; meta = false; code = Down };
  "[C", { control = false; meta = false; code = Right };
  "[D", { control = false; meta = false; code = Left };

  "A", { control = false; meta = false; code = Up };
  "B", { control = false; meta = false; code = Down };
  "C", { control = false; meta = false; code = Right };
  "D", { control = false; meta = false; code = Left };

  "OA", { control = false; meta = false; code = Up };
  "OB", { control = false; meta = false; code = Down };
  "OC", { control = false; meta = false; code = Right };
  "OD", { control = false; meta = false; code = Left };

  "Oa", { control = true; meta = false; code = Up };
  "Ob", { control = true; meta = false; code = Down };
  "Oc", { control = true; meta = false; code = Right };
  "Od", { control = true; meta = false; code = Left };

  "OP", { control = false; meta = false; code = F1 };
  "OQ", { control = false; meta = false; code = F2 };
  "OR", { control = false; meta = false; code = F3 };
  "OS", { control = false; meta = false; code = F4 };

  "O3P", { control = false; meta = true; code = F1 };
  "O3Q", { control = false; meta = true; code = F2 };
  "O3R", { control = false; meta = true; code = F3 };
  "O3S", { control = false; meta = true; code = F4 };

  "O5P", { control = true; meta = false; code = F1 };
  "O5Q", { control = true; meta = false; code = F2 };
  "O5R", { control = true; meta = false; code = F3 };
  "O5S", { control = true; meta = false; code = F4 };

  "[[A", { control = false; meta = false; code = F1 };
  "[[B", { control = false; meta = false; code = F2 };
  "[[C", { control = false; meta = false; code = F3 };
  "[[D", { control = false; meta = false; code = F4 };
  "[[E", { control = false; meta = false; code = F5 };

  "[H", { control = false; meta = false; code = Home };
  "[F", { control = false; meta = false; code = End };

  "OH", { control = false; meta = false; code = Home };
  "OF", { control = false; meta = false; code = End };

  "H", { control = false; meta = false; code = Home };
  "F", { control = false; meta = false; code = End };

  "[1;3A", { control = false; meta = true; code = Up };
  "[1;3B", { control = false; meta = true; code = Down };
  "[1;3C", { control = false; meta = true; code = Right };
  "[1;3D", { control = false; meta = true; code = Left };

  "[1;5A", { control = true; meta = false; code = Up };
  "[1;5B", { control = true; meta = false; code = Down };
  "[1;5C", { control = true; meta = false; code = Right };
  "[1;5D", { control = true; meta = false; code = Left };

  "[1;7A", { control = true; meta = true; code = Up };
  "[1;7B", { control = true; meta = true; code = Down };
  "[1;7C", { control = true; meta = true; code = Right };
  "[1;7D", { control = true; meta = true; code = Left };

  "[1;3P", { control = false; meta = true; code = F1 };
  "[1;3Q", { control = false; meta = true; code = F2 };
  "[1;3R", { control = false; meta = true; code = F3 };
  "[1;3S", { control = false; meta = true; code = F4 };

  "[1;5P", { control = true; meta = false; code = F1 };
  "[1;5Q", { control = true; meta = false; code = F2 };
  "[1;5R", { control = true; meta = false; code = F3 };
  "[1;5S", { control = true; meta = false; code = F4 };

  "O1;3P", { control = false; meta = true; code = F1 };
  "O1;3Q", { control = false; meta = true; code = F2 };
  "O1;3R", { control = false; meta = true; code = F3 };
  "O1;3S", { control = false; meta = true; code = F4 };

  "O1;5P", { control = true; meta = false; code = F1 };
  "O1;5Q", { control = true; meta = false; code = F2 };
  "O1;5R", { control = true; meta = false; code = F3 };
  "O1;5S", { control = true; meta = false; code = F4 };

  "[15;3~", { control = false; meta = true; code = F5 };
  "[17;3~", { control = false; meta = true; code = F6 };
  "[18;3~", { control = false; meta = true; code = F7 };
  "[19;3~", { control = false; meta = true; code = F8 };
  "[20;3~", { control = false; meta = true; code = F9 };
  "[21;3~", { control = false; meta = true; code = F10 };
  "[23;3~", { control = false; meta = true; code = F11 };
  "[24;3~", { control = false; meta = true; code = F12 };

  "[15;5~", { control = true; meta = false; code = F5 };
  "[17;5~", { control = true; meta = false; code = F6 };
  "[18;5~", { control = true; meta = false; code = F7 };
  "[19;5~", { control = true; meta = false; code = F8 };
  "[20;5~", { control = true; meta = false; code = F9 };
  "[21;5~", { control = true; meta = false; code = F10 };
  "[23;5~", { control = true; meta = false; code = F11 };
  "[24;5~", { control = true; meta = false; code = F12 };

  "[1;3H", { control = false; meta = true; code = Home };
  "[1;3F", { control = false; meta = true; code = End };

  "[1;5H", { control = true; meta = false; code = Home };
  "[1;5F", { control = true; meta = false; code = End };

  "[1;7H", { control = true; meta = true; code = Home };
  "[1;7F", { control = true; meta = true; code = End };

  "[2;3~", { control = false; meta = true; code = Insert };
  "[3;3~", { control = false; meta = true; code = Delete };
  "[5;3~", { control = false; meta = true; code = Prev_page };
  "[6;3~", { control = false; meta = true; code = Next_page };

  "[2;5~", { control = true; meta = false; code = Insert };
  "[3;5~", { control = true; meta = false; code = Delete };
  "[5;5~", { control = true; meta = false; code = Prev_page };
  "[6;5~", { control = true; meta = false; code = Next_page };

  "[2;7~", { control = true; meta = true; code = Insert };
  "[3;7~", { control = true; meta = true; code = Delete };
  "[5;7~", { control = true; meta = true; code = Prev_page };
  "[6;7~", { control = true; meta = true; code = Next_page };
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

let rec parse_event cd stream =
  lwt byte = Lwt_stream.next stream in
  match byte with
    | '\x1b' -> begin
        (* Escape sequences *)
        try_lwt
          (* Try to parse an escape seqsuence *)
          Lwt_stream.parse stream parse_escape >>= function
            | "[M" -> begin
                (* Mouse report *)
                let open Lt_mouse in
                lwt mask = Lwt_stream.next stream >|= Char.code in
                lwt x = Lwt_stream.next stream >|= Char.code in
                lwt y = Lwt_stream.next stream >|= Char.code in
                try
                  if mask = 0b00100011 then raise Exit;
                  return (Lt_event.Mouse {
                            control = mask land 0b00010000 <> 0;
                            meta = mask land 0b00001000 <> 0;
                            line = y - 33;
                            column = x - 33;
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
                  parse_event cd stream
              end
            | seq ->
                match find_sequence seq with
                  | Some key ->
                      return (Lt_event.Key key)
                  | None ->
                      return (Lt_event.Sequence ("\x1b" ^ seq))
        with Not_a_sequence ->
          (* If it is not, test if it is META+key. *)
          match Lwt.state (Lwt_stream.peek stream) with
            | Sleep | Fail _ | Return None ->
                return (Lt_event.Key { control = false; meta = false; code = Escape })
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
                        return (Lt_event.Key { control = false; meta = false; code = Escape })
                    end
                  | '\x00' .. '\x1b' ->
                      (* Control characters *)
                      lwt () = Lwt_stream.junk stream in
                      return (Lt_event.Key { control = true; meta = true; code = controls.(Char.code byte) })
                  | '\x7f' ->
                      (* Backspace *)
                      lwt () = Lwt_stream.junk stream in
                      return (Lt_event.Key { control = false; meta = true; code = Backspace })
                  | '\x00' .. '\x7f' ->
                      (* Other ascii characters *)
                      lwt () = Lwt_stream.junk stream in
                      return(Lt_event.Key  { control = false; meta = true; code = Char(Char.code byte) })
                  | byte' ->
                      lwt () = Lwt_stream.junk stream in
                      lwt code = parse_char cd stream byte' in
                      return (Lt_event.Key { control = false; meta = true; code = Char code })
      end
    | '\x00' .. '\x1f' ->
        (* Control characters *)
        return (Lt_event.Key { control = true; meta = false; code = controls.(Char.code byte) })
    | '\x7f' ->
        (* Backspace *)
        return (Lt_event.Key { control = false; meta = false; code = Backspace })
    | '\x00' .. '\x7f' ->
        (* Other ascii characters *)
        return (Lt_event.Key { control = false; meta = false; code = Char(Char.code byte) })
    | _ ->
        (* Encoded characters *)
        lwt code = parse_char cd stream byte in
        return (Lt_event.Key { control = false; meta = false; code = Char code })
