(*
 * lTerm.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open Lwt
open LTerm_geom

(* +-----------------------------------------------------------------+
   | TTYs sizes                                                      |
   +-----------------------------------------------------------------+ *)

external get_size_from_fd : Unix.file_descr -> size = "lt_term_get_size_from_fd"
external set_size_from_fd : Unix.file_descr -> size -> unit = "lt_term_set_size_from_fd"

let get_size_from_fd fd =
  Lwt_unix.check_descriptor fd;
  get_size_from_fd (Lwt_unix.unix_file_descr fd)

let set_size_from_fd fd size =
  Lwt_unix.check_descriptor fd;
  set_size_from_fd (Lwt_unix.unix_file_descr fd) size

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

exception Not_a_tty

let () =
  Printexc.register_printer
    (function
       | Not_a_tty -> Some "terminal is not a tty"
       | _ -> None)

module Int_map = Map.Make(struct type t = int let compare a b = a - b end)

type t = {
  model : string;
  colors : int;
  windows : bool;
  bold_is_bright : bool;
  color_map : LTerm_color_mappings.map;
  (* Informations. *)

  mutable raw_mode : bool;
  (* Whether the terminal is currently in raw mode. *)

  incoming_fd : Lwt_unix.file_descr;
  outgoing_fd : Lwt_unix.file_descr;
  (* File descriptors. *)

  oc : Lwt_io.output_channel;
  (* Output channels. *)

  input_stream : char Lwt_stream.t;
  (* Stream of characters read from the terminal. *)

  mutable next_event : LTerm_event.t Lwt.t option;
  (* Thread reading the next event from the terminal. We cannot cancel
     the reading of an event, so we keep the last thread to reuse it
     in case the user cancels [read_event]. *)

  mutable read_event : bool;
  (* Whether a thread is currently reading an event. *)

  mutable last_reported_size : size;
  (* The last size reported by [read_event]. *)

  mutable size : size;
  (* The current size of the terminal. *)

  incoming_encoding : CharEncoding.t;
  outgoing_encoding : CharEncoding.t;
  (* Characters encodings. *)

  outgoing_is_utf8 : bool;
  (* Whether the outgoing encoding is UTF-8. *)

  notify : LTerm_event.t Lwt_condition.t;
  (* Condition used to send a spontaneous event. *)

  mutable event : unit event;
  (* Event which handles SIGWINCH. *)

  incoming_is_a_tty : bool;
  outgoing_is_a_tty : bool;
  (* Whether input/output are tty devices. *)

  mutable escape_time : float;
  (* Time to wait before returning the escape key. *)
}

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

let resize_event, send_resize = E.create ()

let () =
  match LTerm_unix.sigwinch with
    | None ->
        (* Check for size when something happen. *)
        ignore (Lwt_sequence.add_r send_resize Lwt_main.enter_iter_hooks)
    | Some signum ->
        try
          ignore (Lwt_unix.on_signal signum (fun _ -> send_resize ()))
        with Not_found ->
          ignore (Lwt_sequence.add_r send_resize Lwt_main.enter_iter_hooks)

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let default_model, term_defined =
  try
    (Sys.getenv "TERM", true)
  with Not_found ->
    ("dumb", false)

let colors_of_term = function
  | "Eterm-256color" -> 256
  | "Eterm-88color" -> 88
  | "gnome-256color" -> 256
  | "iTerm.app" -> 256
  | "konsole-256color" -> 256
  | "mlterm-256color" -> 256
  | "mrxvt-256color" -> 256
  | "putty-256color" -> 256
  | "rxvt-256color" -> 256
  | "rxvt-88color" -> 88
  | "rxvt-unicode-256color" -> 256
  | "rxvt-unicode" -> 88
  | "screen-256color" -> 256
  | "screen-256color-bce" -> 256
  | "screen-256color-bce-s" -> 256
  | "screen-256color-s" -> 256
  | "st-256color" -> 256
  | "vte-256color" -> 256
  | "xterm-256color" -> 256
  | "xterm+256color" -> 256
  | "xterm-88color" -> 88
  | "xterm+88color" -> 88
  | _ -> 16

exception No_such_encoding of string

let char_encoding_of_name name =
  try
    CharEncoding.of_name name
  with Not_found ->
    raise (No_such_encoding name)

(* UTF-8 in windows. *)
let () = CharEncoding.alias "CP65001" "UTF-8"

let create ?(windows=Lwt_sys.windows) ?(model=default_model) ?incoming_encoding ?outgoing_encoding incoming_fd ic outgoing_fd oc =
  try_lwt
    (* Select number of colors. *)
    let colors = if windows then 16 else colors_of_term model in
    (* Get encodings. *)
    let incoming_encoding =
      char_encoding_of_name
        (match incoming_encoding with
           | Some enc ->
               enc
           | None ->
               if windows then
                 Printf.sprintf "CP%d" (LTerm_windows.get_console_cp ())
               else
                 LTerm_unix.system_encoding)
    and outgoing_encoding =
      char_encoding_of_name
        (match outgoing_encoding with
           | Some enc ->
               enc
           | None ->
               if windows then
                 Printf.sprintf "CP%d" (LTerm_windows.get_console_output_cp ())
               else
                 LTerm_unix.system_encoding)
    in
    (* Check if fds are ttys using the OS dependent API. *)
    lwt incoming_is_a_tty = Lwt_unix.isatty incoming_fd
    and outgoing_is_a_tty = Lwt_unix.isatty outgoing_fd in
    (* Create the terminal. *)
    let term = {
      model;
      colors;
      windows;
      bold_is_bright = (match model with
                          | "linux" (* The linux frame buffer *)
                          | "xterm-color" (* The MacOS-X terminal *) ->
                              true
                          | _ ->
                              false);
      color_map = (match colors with
                     | 16 -> LTerm_color_mappings.colors_16
                     | 88 -> LTerm_color_mappings.colors_88
                     | 256 -> LTerm_color_mappings.colors_256
                     | n -> Printf.ksprintf failwith "LTerm.create: unknown number of colors (%d)" n);
      raw_mode = false;
      incoming_fd;
      outgoing_fd;
      oc;
      input_stream = Lwt_stream.from (fun () -> Lwt_io.read_char_opt ic);
      next_event = None;
      read_event = false;
      incoming_encoding;
      outgoing_encoding;
      outgoing_is_utf8 = CharEncoding.name_of outgoing_encoding = "UTF-8";
      notify = Lwt_condition.create ();
      event = E.never;
      incoming_is_a_tty;
      outgoing_is_a_tty;
      escape_time = 0.1;
      size = { rows = 0; cols = 0 };
      last_reported_size = { rows = 0; cols = 0 };
    } in
    (* Setup initial size and size updater. *)
    if term.outgoing_is_a_tty then begin
      let check_size () =
        let size = get_size_from_fd outgoing_fd in
        if size <> term.size then begin
          term.size <- size;
          Lwt_condition.signal term.notify (LTerm_event.Resize size)
        end
      in
      term.size <- get_size_from_fd outgoing_fd;
      term.last_reported_size <- term.size;
      term.event <- E.map check_size resize_event
    end;
    return term
  with exn ->
    raise_lwt exn

let model t = t.model
let colors t = t.colors
let windows t = t.windows
let is_a_tty t = t.incoming_is_a_tty && t.outgoing_is_a_tty
let incoming_is_a_tty t = t.incoming_is_a_tty
let outgoing_is_a_tty t = t.outgoing_is_a_tty
let escape_time t = t.escape_time
let set_escape_time t time = t.escape_time <- time

let size term =
  if term.outgoing_is_a_tty then begin
    let size = get_size_from_fd term.outgoing_fd in
    if size <> term.size then begin
      term.size <- size;
      Lwt_condition.signal term.notify (LTerm_event.Resize size)
    end;
    size
  end else
    raise Not_a_tty

let get_size term =
  try
    return (size term)
  with exn ->
    raise_lwt exn

let set_size term size = raise_lwt (Failure "LTerm.set_size is deprecated")

(* +-----------------------------------------------------------------+
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

class output_single (cell : UChar.t option ref) = object
  method put char = cell := Some char
  method flush () = ()
  method close_out () = ()
end

let read_char term =
  lwt first_byte =
    match_lwt Lwt_stream.get term.input_stream with
      | Some byte -> return byte
      | None -> raise_lwt End_of_file
  in
  let cell = ref None in
  let output = new CharEncoding.convert_uchar_output term.incoming_encoding (new output_single cell) in
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
  lwt char =
    try_lwt
      assert (output#output (String.make 1 first_byte) 0 1 = 1);
      Lwt_stream.parse term.input_stream loop
    with CharEncoding.Malformed_code | Lwt_stream.Empty ->
      return (UChar.of_char first_byte)
  in
  return (LTerm_event.Key {
            LTerm_key.control = false;
            LTerm_key.meta = false;
            LTerm_key.shift = false;
            LTerm_key.code = LTerm_key.Char char;
          })

let rec next_event term =
  if term.windows then
    match_lwt LTerm_windows.read_console_input term.incoming_fd with
      | LTerm_windows.Resize ->
          if term.outgoing_is_a_tty then
            let size = get_size_from_fd term.outgoing_fd in
            if size <> term.size then begin
              term.size <- size;
              return (LTerm_event.Resize size)
            end else
              next_event term
          else
            next_event term
      | LTerm_windows.Key key ->
          return (LTerm_event.Key key)
      | LTerm_windows.Mouse mouse ->
          let window = (LTerm_windows.get_console_screen_buffer_info term.outgoing_fd).LTerm_windows.window in
          return (LTerm_event.Mouse {
                    mouse with
                      LTerm_mouse.row = mouse.LTerm_mouse.row - window.row1;
                      LTerm_mouse.col = mouse.LTerm_mouse.col - window.col1;
                  })
  else
    LTerm_unix.parse_event ~escape_time:term.escape_time term.incoming_encoding term.input_stream

let wrap_next_event next_event term =
  match term.next_event with
    | Some thread ->
        thread
    | None ->
        (* Create a non-cancelable thread. *)
        let waiter, wakener = wait () in
        term.next_event <- Some waiter;
        (* Connect the [next_event term] thread to [waiter]. *)
        ignore
          (try_bind
             (fun () -> next_event term)
             (fun v ->
                term.next_event <- None;
                wakeup wakener v;
                return ())
             (fun e ->
                term.next_event <- None;
                wakeup_exn wakener e;
                return ()));
        waiter

let read_event term =
  if term.read_event then
    raise_lwt (Failure "LTerm.read_event: cannot read events from two thread at the same time")
  else if term.size <> term.last_reported_size then begin
    term.last_reported_size <- term.size;
    return (LTerm_event.Resize term.last_reported_size)
  end else begin
    term.read_event <- true;
    try_lwt
      if term.incoming_is_a_tty then
        match_lwt pick [wrap_next_event next_event term; Lwt_condition.wait term.notify] with
          | LTerm_event.Resize size ->
              term.last_reported_size <- size;
              return (LTerm_event.Resize size)
          | ev ->
              return ev
      else
        wrap_next_event read_char term
    finally
      term.read_event <- false;
      return ()
  end

(* +-----------------------------------------------------------------+
   | Modes                                                           |
   +-----------------------------------------------------------------+ *)

type mode =
  | Mode_fake
  | Mode_unix of Unix.terminal_io
  | Mode_windows of LTerm_windows.console_mode

let enter_raw_mode term =
  if term.incoming_is_a_tty then
    if term.raw_mode then
      return Mode_fake
    else if term.windows then begin
      let mode = LTerm_windows.get_console_mode term.incoming_fd in
      LTerm_windows.set_console_mode term.incoming_fd {
        mode with
          LTerm_windows.cm_echo_input = false;
          LTerm_windows.cm_line_input = false;
          LTerm_windows.cm_mouse_input = true;
          LTerm_windows.cm_processed_input = false;
          LTerm_windows.cm_window_input = true;
      };
      term.raw_mode <- true;
      return (Mode_windows mode)
    end else begin
      lwt attr = Lwt_unix.tcgetattr term.incoming_fd in
      lwt () = Lwt_unix.tcsetattr term.incoming_fd Unix.TCSAFLUSH {
        attr with
          (* Inspired from Python-3.0/Lib/tty.py: *)
          Unix.c_brkint = false;
          Unix.c_inpck = false;
          Unix.c_istrip = false;
          Unix.c_ixon = false;
          Unix.c_csize = 8;
          Unix.c_parenb = false;
          Unix.c_echo = false;
          Unix.c_icanon = false;
          Unix.c_vmin = 1;
          Unix.c_vtime = 0;
          Unix.c_isig = false;
      } in
      term.raw_mode <- true;
      return (Mode_unix attr)
    end
  else
    raise_lwt Not_a_tty

let leave_raw_mode term mode =
  if term.incoming_is_a_tty then
    match mode with
      | Mode_fake ->
          return ()
      | Mode_unix attr ->
          term.raw_mode <- false;
          Lwt_unix.tcsetattr term.incoming_fd Unix.TCSAFLUSH attr
      | Mode_windows mode ->
          term.raw_mode <- false;
          LTerm_windows.set_console_mode term.incoming_fd mode;
          return ()
  else
    raise_lwt Not_a_tty

let enable_mouse term =
  if term.outgoing_is_a_tty then
    if term.windows then
      return ()
    else
      Lwt_io.write term.oc "\027[?1000h"
  else
    raise_lwt Not_a_tty

let disable_mouse term =
  if term.outgoing_is_a_tty then
    if term.windows then
      return ()
    else
      Lwt_io.write term.oc "\027[?1000l"
  else
    raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Cursor                                                          |
   +-----------------------------------------------------------------+ *)

let show_cursor term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let size, _ = LTerm_windows.get_console_cursor_info term.outgoing_fd in
      LTerm_windows.set_console_cursor_info term.outgoing_fd size true;
      return ()
    end else
      Lwt_io.write term.oc "\027[?25h"
  else
    raise_lwt Not_a_tty

let hide_cursor term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let size, _ = LTerm_windows.get_console_cursor_info term.outgoing_fd in
      LTerm_windows.set_console_cursor_info term.outgoing_fd size false;
      return ()
    end else
      Lwt_io.write term.oc "\027[?25l"
  else
    raise_lwt Not_a_tty

let goto term coord =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      lwt () = Lwt_io.flush term.oc in
      let window = (LTerm_windows.get_console_screen_buffer_info term.outgoing_fd).LTerm_windows.window in
      LTerm_windows.set_console_cursor_position term.outgoing_fd {
        row = window.row1 + coord.row;
        col = window.col1 + coord.col;
      };
      return ()
    end else begin
      lwt () = Lwt_io.fprint term.oc "\027[H" in
      lwt () = if coord.row > 0 then Lwt_io.fprintf term.oc "\027[%dB" coord.row else return () in
      lwt () = if coord.col > 0 then Lwt_io.fprintf term.oc "\027[%dC" coord.col else return () in
      return ()
    end
  else
    raise_lwt Not_a_tty

let move term rows cols =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      lwt () = Lwt_io.flush term.oc in
      let pos = (LTerm_windows.get_console_screen_buffer_info term.outgoing_fd).LTerm_windows.cursor_position in
      LTerm_windows.set_console_cursor_position term.outgoing_fd {
        row = pos.row + rows;
        col = pos.col + cols;
      };
      return ()
    end else
      lwt () =
        match rows with
          | n when n < 0 ->
              Lwt_io.fprintf term.oc "\027[%dA" (-n)
          | n when n > 0 ->
              Lwt_io.fprintf term.oc "\027[%dB" n
          | _ ->
              return ()
      and () =
        match cols with
          | n when n < 0 ->
              Lwt_io.fprintf term.oc "\027[%dD" (-n)
          | n when n > 0 ->
              Lwt_io.fprintf term.oc "\027[%dC" n
          | _ ->
              return ()
      in
      return ()
  else
    raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Erasing text                                                    |
   +-----------------------------------------------------------------+ *)

let clear_screen term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        LTerm_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.LTerm_windows.size.cols * info.LTerm_windows.size.rows)
          { row = 0; col = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[2J"
  else
    raise_lwt Not_a_tty

let clear_screen_next term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        LTerm_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.LTerm_windows.size.cols * (info.LTerm_windows.size.rows - info.LTerm_windows.cursor_position.row)
           + info.LTerm_windows.size.cols - info.LTerm_windows.cursor_position.col)
          info.LTerm_windows.cursor_position
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[J"
  else
    raise_lwt Not_a_tty

let clear_screen_prev term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        LTerm_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.LTerm_windows.size.cols * info.LTerm_windows.cursor_position.row
           + info.LTerm_windows.cursor_position.col)
          { row = 0; col = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[1J"
  else
    raise_lwt Not_a_tty

let clear_line term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        LTerm_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          info.LTerm_windows.size.cols
          { row = info.LTerm_windows.cursor_position.row; col = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[2K"
  else
    raise_lwt Not_a_tty

let clear_line_next term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        LTerm_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.LTerm_windows.size.cols - info.LTerm_windows.cursor_position.col)
          info.LTerm_windows.cursor_position
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[K"
  else
    raise_lwt Not_a_tty

let clear_line_prev term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        LTerm_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          info.LTerm_windows.cursor_position.col
          { row = info.LTerm_windows.cursor_position.row; col = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[1K"
  else
    raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let save_state term =
  if term.outgoing_is_a_tty then
    if term.windows then
      return ()
    else
      Lwt_io.write term.oc "\027[?1049h"
  else
    raise_lwt Not_a_tty

let load_state term =
  if term.outgoing_is_a_tty then
    if term.windows then
      return ()
    else
      Lwt_io.write term.oc "\027[?1049l"
  else
    raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | String recoding                                                 |
   +-----------------------------------------------------------------+ *)

let vline = UChar.of_char '|'
let vlline = UChar.of_char '+'
let dlcorner = UChar.of_char '+'
let urcorner = UChar.of_char '+'
let huline = UChar.of_char '+'
let hdline = UChar.of_char '+'
let vrline = UChar.of_char '+'
let hline = UChar.of_char '-'
let cross = UChar.of_char '+'
let ulcorner = UChar.of_char '+'
let drcorner = UChar.of_char '+'
let question = UChar.of_char '?'

module UNF = UNF.Make (UText)

(* Map characters that cannot be encoded to ASCII ones. *)
let map_char char =
  match UChar.code char with
    | 0x2500 -> hline
    | 0x2501 -> hline
    | 0x2502 -> vline
    | 0x2503 -> vline
    | 0x2504 -> hline
    | 0x2505 -> hline
    | 0x2506 -> vline
    | 0x2507 -> vline
    | 0x2508 -> hline
    | 0x2509 -> hline
    | 0x250a -> vline
    | 0x250b -> vline
    | 0x250c -> drcorner
    | 0x250d -> drcorner
    | 0x250e -> drcorner
    | 0x250f -> drcorner
    | 0x2510 -> dlcorner
    | 0x2511 -> dlcorner
    | 0x2512 -> dlcorner
    | 0x2513 -> dlcorner
    | 0x2514 -> urcorner
    | 0x2515 -> urcorner
    | 0x2516 -> urcorner
    | 0x2517 -> urcorner
    | 0x2518 -> ulcorner
    | 0x2519 -> ulcorner
    | 0x251a -> ulcorner
    | 0x251b -> ulcorner
    | 0x251c -> vrline
    | 0x251d -> vrline
    | 0x251e -> vrline
    | 0x251f -> vrline
    | 0x2520 -> vrline
    | 0x2521 -> vrline
    | 0x2522 -> vrline
    | 0x2523 -> vrline
    | 0x2524 -> vlline
    | 0x2525 -> vlline
    | 0x2526 -> vlline
    | 0x2527 -> vlline
    | 0x2528 -> vlline
    | 0x2529 -> vlline
    | 0x252a -> vlline
    | 0x252b -> vlline
    | 0x252c -> hdline
    | 0x252d -> hdline
    | 0x252e -> hdline
    | 0x252f -> hdline
    | 0x2530 -> hdline
    | 0x2531 -> hdline
    | 0x2532 -> hdline
    | 0x2533 -> hdline
    | 0x2534 -> huline
    | 0x2535 -> huline
    | 0x2536 -> huline
    | 0x2537 -> huline
    | 0x2538 -> huline
    | 0x2539 -> huline
    | 0x253a -> huline
    | 0x253b -> huline
    | 0x253c -> cross
    | 0x253d -> cross
    | 0x253e -> cross
    | 0x253f -> cross
    | 0x2540 -> cross
    | 0x2541 -> cross
    | 0x2542 -> cross
    | 0x2543 -> cross
    | 0x2544 -> cross
    | 0x2545 -> cross
    | 0x2546 -> cross
    | 0x2547 -> cross
    | 0x2548 -> cross
    | 0x2549 -> cross
    | 0x254a -> cross
    | 0x254b -> cross
    | 0x254c -> hline
    | 0x254d -> hline
    | 0x254e -> vline
    | 0x254f -> vline
    | 0x2550 -> hline
    | 0x2551 -> vline
    | _ ->
        match UNF.nfd_decompose char with
          | char :: _ ->
              if UChar.code char <= 127 then
                char
              else
                question
          | [] ->
              question

class output_to_buffer buf res = object
  method output str ofs len =
    Buffer.add_substring buf str ofs len;
    len
  method flush () = ()
  method close_out () =
    res := Buffer.contents buf
end

let encode_string term str =
  if term.outgoing_is_utf8 then
    (* Do not recode [str] if the output is UTF-8. *)
    str
  else
    let buf = Buffer.create (String.length str) in
    let res = ref "" in
    let output = new CharEncoding.uchar_output_channel_of term.outgoing_encoding (new output_to_buffer buf res) in
    let rec loop ofs =
      if ofs = String.length str then begin
        output#close_out ();
        !res
      end else begin
        let ch, ofs = Zed_utf8.unsafe_extract_next str ofs in
        (try
           output#put ch
         with CharEncoding.Out_of_range ->
           output#put (map_char ch));
        loop ofs
      end
    in
    loop 0

let encode_char term ch =
  let res = ref "" in
  let output = new CharEncoding.uchar_output_channel_of term.outgoing_encoding (new output_to_buffer (Buffer.create 8) res) in
  (try
     output#put ch
   with CharEncoding.Out_of_range ->
     output#put (map_char ch));
  output#close_out ();
  !res

(* +-----------------------------------------------------------------+
   | Styled printing                                                 |
   +-----------------------------------------------------------------+ *)

module Codes = struct
  let bold = ";1"
  let underline = ";4"
  let blink = ";5"
  let reverse = ";7"
  let foreground = 30
  let background = 40
end

let fprint term str =
  Lwt_io.fprint term.oc (encode_string term str)

let fprintl term str =
  fprint term (str ^ "\n")

let fprintf term fmt =
  Printf.ksprintf (fun str -> fprint term str) fmt

let fprintlf term fmt =
  Printf.ksprintf (fun str -> fprintl term str) fmt

let add_int buf n =
  let rec loop = function
    | 0 ->
        ()
    | n ->
        loop (n / 10);
        Buffer.add_char buf (Char.unsafe_chr (48 + (n mod 10)))
  in
  if n = 0 then
    Buffer.add_char buf '0'
  else
    loop n

let map_color term r g b =
  let open LTerm_color_mappings in
  let map = term.color_map in
  (* The [String.unsafe_get] is safe because the private type
     [LTerm_style.color] ensure that all components are in the range
     [0..255]. *)
  Char.code (String.unsafe_get map.map (map.index_r.(r) + map.count_r * (map.index_g.(g) + map.count_g * map.index_b.(b))))

let add_index term buf base n =
  if n < 8 then begin
    Buffer.add_char buf ';';
    add_int buf (base + n)
  end else if n < 16 &&  term.bold_is_bright then
    if base = Codes.foreground then begin
      Buffer.add_string buf ";1;";
      add_int buf (base + n - 8)
    end else begin
      Buffer.add_char buf ';';
      add_int buf (base + n - 8)
    end
  else begin
    Buffer.add_char buf ';';
    add_int buf (base + 8);
    Buffer.add_string buf ";5;";
    add_int buf n
  end

let add_color term buf base = function
  | LTerm_style.Default ->
      ()
  | LTerm_style.Index n ->
      add_index term buf base n
  | LTerm_style.RGB(r, g, b) ->
      add_index term buf base (map_color term  r g b)

let add_style term buf style =
  let open LTerm_style in
  Buffer.add_string buf "\027[0";
  (match style.bold with Some true -> Buffer.add_string buf Codes.bold | _ -> ());
  (match style.underline with Some true -> Buffer.add_string buf Codes.underline | _ -> ());
  (match style.blink with Some true -> Buffer.add_string buf Codes.blink | _ -> ());
  (match style.reverse with Some true -> Buffer.add_string buf Codes.reverse | _ -> ());
  (match style.foreground with Some color -> add_color term buf Codes.foreground color | None -> ());
  (match style.background with Some color -> add_color term buf Codes.background color | None -> ());
  Buffer.add_char buf 'm'

let expand term text =
  if Array.length text = 0 then
    ""
  else begin
    let buf = Buffer.create 256 in
    Buffer.add_string buf "\027[0m";
    let rec loop idx prev_style =
      if idx = Array.length text then begin
        Buffer.add_string buf "\027[0m";
        Buffer.contents buf
      end else begin
        let ch, style = Array.unsafe_get text idx in
        if not (LTerm_style.equal style prev_style) then add_style term buf style;
        Buffer.add_string buf (Zed_utf8.singleton ch);
        loop (idx + 1) style
      end
    in
    loop 0 LTerm_style.none
  end

let windows_fg_color term = function
  | LTerm_style.Default -> 7
  | LTerm_style.Index n -> n
  | LTerm_style.RGB(r, g, b) -> map_color term r g b

let windows_bg_color term = function
  | LTerm_style.Default -> 0
  | LTerm_style.Index n -> n
  | LTerm_style.RGB(r, g, b) -> map_color term r g b

let windows_default_attributes = { LTerm_windows.foreground = 7; LTerm_windows.background = 0 }

let fprints_windows term oc text =
  let open LTerm_style in
  let rec loop idx prev_attr =
    if idx = Array.length text then begin
      lwt () = Lwt_io.flush oc in
      LTerm_windows.set_console_text_attribute term.outgoing_fd windows_default_attributes;
      return ()
    end else begin
      let ch, style = Array.unsafe_get text idx in
      let attr =
        if style.reverse = Some true then {
          LTerm_windows.foreground = (match style.background with Some color -> windows_bg_color term color | None -> 0);
          LTerm_windows.background = (match style.foreground with Some color -> windows_fg_color term color | None -> 7);
        } else {
          LTerm_windows.foreground = (match style.foreground with Some color -> windows_fg_color term color | None -> 7);
          LTerm_windows.background = (match style.background with Some color -> windows_bg_color term color | None -> 0);
        }
      in
      lwt () =
        if attr <> prev_attr then
          lwt () = Lwt_io.flush oc in
          LTerm_windows.set_console_text_attribute term.outgoing_fd attr;
          return ()
        else
          return ()
      in
      lwt () = Lwt_io.write oc (encode_char term ch) in
      loop (idx + 1) attr
    end
  in
  lwt () = Lwt_io.flush oc in
  LTerm_windows.set_console_text_attribute term.outgoing_fd windows_default_attributes;
  loop 0 windows_default_attributes

let fprints term text =
  if term.outgoing_is_a_tty then
    if term.windows then
      Lwt_io.atomic (fun oc -> fprints_windows term oc text) term.oc
    else
      fprint term (expand term text)
  else
    fprint term (LTerm_text.to_string text)

let fprintls term text =
  fprints term (Array.append text (LTerm_text.of_string "\n"))

(* +-----------------------------------------------------------------+
   | Styles setting                                                  |
   +-----------------------------------------------------------------+ *)

let set_style term style =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let open LTerm_style in
      let attr =
        if style.reverse = Some true then {
          LTerm_windows.foreground = (match style.background with Some color -> windows_bg_color term color | None -> 0);
          LTerm_windows.background = (match style.foreground with Some color -> windows_fg_color term color | None -> 7);
        } else {
          LTerm_windows.foreground = (match style.foreground with Some color -> windows_fg_color term color | None -> 7);
          LTerm_windows.background = (match style.background with Some color -> windows_bg_color term color | None -> 0);
        }
      in
      Lwt_io.atomic
        (fun oc ->
           lwt () = Lwt_io.flush oc in
           LTerm_windows.set_console_text_attribute term.outgoing_fd attr;
           return ())
        term.oc
    end else begin
      let buf = Buffer.create 16 in
      add_style term buf style;
      Lwt_io.fprint term.oc (Buffer.contents buf)
    end
  else
    return ()

(* +-----------------------------------------------------------------+
   | Rendering                                                       |
   +-----------------------------------------------------------------+ *)

let same_style p1 p2 =
  let open LTerm_draw in
  p1.bold = p2.bold &&
      p1.underline = p2.underline &&
      p1.blink = p2.blink &&
      p1.reverse = p2.reverse &&
      p1.foreground = p2.foreground &&
      p1.background = p2.background

let unknown_char = UChar.of_int 0xfffd
let unknown_utf8 = Zed_utf8.singleton unknown_char

let render_point term buf old_point new_point =
  let open LTerm_draw in
  if not (same_style new_point old_point) then begin
    (* Reset styles if they are different from the previous point. *)
    Buffer.add_string buf "\027[0";
    if new_point.bold then Buffer.add_string buf Codes.bold;
    if new_point.underline then Buffer.add_string buf Codes.underline;
    if new_point.blink then Buffer.add_string buf Codes.blink;
    if new_point.reverse then Buffer.add_string buf Codes.reverse;
    add_color term buf Codes.foreground new_point.foreground;
    add_color term buf Codes.background new_point.background;
    Buffer.add_char buf 'm';
  end;
  (* Skip control characters, otherwise output will be messy. *)
  if UChar.code new_point.char < 32 then
    Buffer.add_string buf unknown_utf8
  else
    Buffer.add_string buf (Zed_utf8.singleton new_point.char)

type render_kind = Render_screen | Render_box

let render_update_unix term kind old_matrix matrix =
  let open LTerm_draw in
  let buf = Buffer.create 16 in
  Buffer.add_string buf
    (match kind with
       | Render_screen ->
           (* Go the the top-left and reset attributes *)
           "\027[H\027[0m"
       | Render_box ->
           (* Go the the beginnig of line and reset attributes *)
           "\r\027[0m");
  (* The last displayed point. *)
  let last_point = ref {
    char = UChar.of_char ' ';
    bold = false;
    underline = false;
    blink = false;
    reverse = false;
    foreground = LTerm_style.default;
    background = LTerm_style.default;
  } in
  let rows = Array.length matrix and old_rows = Array.length old_matrix in
  for y = 0 to rows - 1 do
    let line = Array.unsafe_get matrix y in
    (* If the current line is equal to the displayed one, skip it *)
    if y >= old_rows || line <> Array.unsafe_get old_matrix y then begin
      for x = 0 to Array.length line - 1 do
        let point = Array.unsafe_get line x in
        render_point term buf !last_point point;
        last_point := point
      done
    end;
    if y < rows - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf "\027[0m";
  (* Go to the beginning of the line if rendering a box. *)
  if kind = Render_box then Buffer.add_char buf '\r';
  fprint term (Buffer.contents buf)

let blank_windows = {
  LTerm_windows.ci_char = UChar.of_char ' ';
  LTerm_windows.ci_foreground = 7;
  LTerm_windows.ci_background = 0;
}

let render_windows term kind handle_newlines matrix =
  let open LTerm_draw in
  (* Build the matrix of char infos *)
  let matrix =
    Array.map
      (fun line ->
         let len = Array.length line - (if handle_newlines then 1 else 0) in
         if len < 0 then invalid_arg "LTerm.print_box_with_newlines";
         let res = Array.make len blank_windows in
         let rec loop i =
           if i = len then
             res
           else begin
             let point = Array.unsafe_get line i in
             let code = UChar.code point.char in
             if handle_newlines && code = 10 then
               res
             else begin
               let char = if code < 32 then unknown_char else point.char in
               Array.unsafe_set res i (
                 if point.reverse then {
                   LTerm_windows.ci_char = char;
                   LTerm_windows.ci_foreground = windows_bg_color term point.background;
                   LTerm_windows.ci_background = windows_fg_color term point.foreground;
                 } else {
                   LTerm_windows.ci_char = char;
                   LTerm_windows.ci_foreground = windows_fg_color term point.foreground;
                   LTerm_windows.ci_background = windows_bg_color term point.background;
                 }
               );
               loop (i + 1)
             end
           end
         in
         loop 0)
      matrix
  in
  let rows = Array.length matrix in
  lwt () =
    match kind with
      | Render_screen ->
          return ()
      | Render_box ->
          (* Ensure that there is enough place to display the box. *)
          lwt () = fprint term "\r" in
          lwt () = fprint term (String.make (rows - 1) '\n') in
          Lwt_io.flush term.oc
  in
  let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
  let window_rect = info.LTerm_windows.window in
  let rect =
    match kind with
      | Render_screen ->
          window_rect
      | Render_box ->
          { window_rect with
              row1 = info.LTerm_windows.cursor_position.row - (rows - 1);
              row2 = info.LTerm_windows.cursor_position.row + 1 }
  in
  ignore (
    LTerm_windows.write_console_output
      term.outgoing_fd
      matrix
      { rows = Array.length matrix; cols = if matrix = [||] then 0 else Array.length matrix.(0) }
      { row = 0; col = 0 }
      rect
  );
  return ()

let render_update term old_matrix matrix =
  if term.outgoing_is_a_tty then
    if term.windows then
      render_windows term Render_screen false matrix
    else
      render_update_unix term Render_screen old_matrix matrix
  else
    raise_lwt Not_a_tty

let render term m = render_update term [||] m

let print_box term matrix =
  if term.outgoing_is_a_tty then begin
    if Array.length matrix > 0 then begin
      if term.windows then
        render_windows term Render_box false matrix
      else
        render_update_unix term Render_box [||] matrix
    end else
      fprint term "\r"
  end else
    raise_lwt Not_a_tty

let print_box_with_newlines_unix term matrix =
  let open LTerm_draw in
  let buf = Buffer.create 16 in
  (* Go the the beginnig of line and reset attributes *)
  Buffer.add_string buf "\r\027[0m";
  (* The last displayed point. *)
  let last_point = ref {
    char = UChar.of_char ' ';
    bold = false;
    underline = false;
    blink = false;
    reverse = false;
    foreground = LTerm_style.default;
    background = LTerm_style.default;
  } in
  let rows = Array.length matrix in
  for y = 0 to rows - 1 do
    let line = Array.unsafe_get matrix y in
    let cols = Array.length line - 1 in
    if cols < 0 then invalid_arg "LTerm.print_box_with_newlines";
    let rec loop x =
      let point = Array.unsafe_get line x in
      let code = UChar.code point.char in
      if x = cols then begin
        if code = 10 && y < rows - 1 then
          Buffer.add_char buf '\n'
      end else if code = 10 then begin
        (* Erase everything until the end of line. *)
        Buffer.add_string buf "\027[K";
        if y < rows - 1 then Buffer.add_char buf '\n'
      end else begin
        render_point term buf !last_point point;
        last_point := point;
        loop (x + 1)
      end
    in
    loop 0
  done;
  Buffer.add_string buf "\027[0m\r";
  fprint term (Buffer.contents buf)

let print_box_with_newlines term matrix =
  if term.outgoing_is_a_tty then begin
    if Array.length matrix > 0 then begin
      if term.windows then
        render_windows term Render_box true matrix
      else
        print_box_with_newlines_unix term matrix
    end else
      fprint term "\r"
  end else
    raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let flush term = Lwt_io.flush term.oc

let get_size_from_fd fd = return (get_size_from_fd fd)
let set_size_from_fd fd size = return (set_size_from_fd fd size)

(* +-----------------------------------------------------------------+
   | Standard terminals                                              |
   +-----------------------------------------------------------------+ *)

let stdout = lazy(create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout)
let stderr = lazy(create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stderr Lwt_io.stderr)

let print str = Lazy.force stdout >>= fun term -> fprint term str
let printl str = Lazy.force stdout >>= fun term -> fprintl term str
let printf fmt = Printf.ksprintf print fmt
let prints str = Lazy.force stdout >>= fun term -> fprints term str
let printlf fmt = Printf.ksprintf printl fmt
let printls str = Lazy.force stdout >>= fun term -> fprintls term str
let eprint str = Lazy.force stderr >>= fun term -> fprint term str
let eprintl str = Lazy.force stderr >>= fun term -> fprintl term str
let eprintf fmt = Printf.ksprintf eprint fmt
let eprints str = Lazy.force stderr >>= fun term -> fprints term str
let eprintlf fmt = Printf.ksprintf eprintl fmt
let eprintls str = Lazy.force stderr >>= fun term -> fprintls term str
