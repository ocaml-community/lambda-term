(*
 * lTerm.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open React
open Lwt
open LTerm_geom

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

exception Not_a_tty

let () =
  Printexc.register_printer
    (function
       | Not_a_tty -> Some "terminal is not a tty"
       | _ -> None)

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

  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  (* Channels. *)

  incoming_encoding : CharEncoding.t;
  outgoing_encoding : CharEncoding.t;
  (* Characters encodings. *)

  outgoing_is_utf8 : bool;
  (* Whether the outgoing encoding is UTF-8. *)

  input_stream : char Lwt_stream.t;
  (* Stream of character input from the terminal. *)

  mutable resize_received : bool;
  (* Whether a SIGWINCH signal have been received and have not yet
     been reported. *)

  mutable break_received : bool;
  (* Whether a SIGINT signal have been received and have not yet been
     reported. *)

  mutable suspend_received : bool;
  (* Whether a SIGTSTP signal have been received and have not yet been
     reported. *)

  mutable quit_received : bool;
  (* Whether a SIGQUIT signal have been received and have not yet been
     reported. *)

  event_cond : [ `Resize | `Event of LTerm_event.t ] Lwt_condition.t;
  (* Condition used to wakeup the read_event thread on unix. *)

  mutable event : unit event;
  (* Event which handle POSIX signals. *)

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

let ()=
  match LTerm_unix.sigwinch with
    | Some signum ->
        ignore (Lwt_unix.on_signal signum send_resize)
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let default_model =
  try
    Sys.getenv "TERM"
  with Not_found ->
    "dumb"

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

let create ?(windows=Lwt_sys.windows) ?(model=default_model) ?incoming_encoding ?outgoing_encoding incoming_fd ic outgoing_fd oc =
  let incoming_encoding =
    match incoming_encoding with
      | Some enc ->
          enc
      | None ->
          if windows then
            Printf.sprintf "CP%d" (LTerm_windows.get_console_cp ())
          else
            LTerm_unix.system_encoding
  and outgoing_encoding =
    match outgoing_encoding with
      | Some enc ->
          enc
      | None ->
          if windows then
            Printf.sprintf "CP%d" (LTerm_windows.get_console_output_cp ())
          else
            LTerm_unix.system_encoding
  in
  let colors = colors_of_term model in
  lwt incoming_is_a_tty = Lwt_unix.isatty incoming_fd
  and outgoing_is_a_tty = Lwt_unix.isatty outgoing_fd in
  let incoming_encoding = CharEncoding.of_name incoming_encoding
  and outgoing_encoding = CharEncoding.of_name outgoing_encoding in
  let term = {
    model;
    colors;
    windows;
    bold_is_bright =
      (match model with
         | "linux" (* The linux frame buffer *)
         | "xterm-color" (* The MacOS-X terminal *) ->
             true
         | _ ->
             false);
    color_map =
      (match colors with
         | 16 -> LTerm_color_mappings.colors_16
         | 88 -> LTerm_color_mappings.colors_88
         | 256 -> LTerm_color_mappings.colors_256
         | n -> Printf.ksprintf failwith "LTerm.create: unknown number of colors (%d)" n);
    raw_mode = false;
    incoming_fd;
    outgoing_fd;
    ic;
    oc;
    incoming_encoding;
    outgoing_encoding;
    outgoing_is_utf8 = CharEncoding.name_of outgoing_encoding = "UTF-8";
    input_stream = Lwt_stream.from (fun () -> Lwt_io.read_char_opt ic);
    resize_received = false;
    suspend_received = false;
    break_received = false;
    quit_received = false;
    event_cond = Lwt_condition.create ();
    event = E.never;
    incoming_is_a_tty;
    outgoing_is_a_tty;
    escape_time = 0.1;
  } in
  term.event <- (
    E.map
      (fun _ ->
         term.resize_received <- true;
         Lwt_condition.signal term.event_cond `Resize)
      resize_event
  );
  return term

let model t = t.model
let colors t = t.colors
let windows t = t.windows
let is_a_tty t = t.incoming_is_a_tty && t.outgoing_is_a_tty
let incoming_is_a_tty t = t.incoming_is_a_tty
let outgoing_is_a_tty t = t.outgoing_is_a_tty
let escape_time t = t.escape_time
let set_escape_time t time = t.escape_time <- time

(* +-----------------------------------------------------------------+
   | Sizes                                                           |
   +-----------------------------------------------------------------+ *)

external get_size_from_fd : Unix.file_descr -> size = "lt_term_get_size_from_fd"
external set_size_from_fd : Unix.file_descr -> size -> unit = "lt_term_set_size_from_fd"

let get_size_from_fd fd =
  Lwt_unix.check_descriptor fd;
  return (get_size_from_fd (Lwt_unix.unix_file_descr fd))

let set_size_from_fd fd size =
  Lwt_unix.check_descriptor fd;
  return (set_size_from_fd (Lwt_unix.unix_file_descr fd) size)

let get_size term =
  if term.outgoing_is_a_tty then
    get_size_from_fd term.outgoing_fd
  else
    raise_lwt Not_a_tty

let set_size term size =
  if term.outgoing_is_a_tty then
    set_size_from_fd term.outgoing_fd size
  else
    raise_lwt Not_a_tty

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
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

let read_event term =
  if term.windows then
    LTerm_windows.read_console_input term.incoming_fd >>= function
      | LTerm_windows.Resize ->
          lwt size = get_size term in
          return (LTerm_event.Resize size)
      | LTerm_windows.Key key ->
          return (LTerm_event.Key key)
      | LTerm_windows.Mouse mouse ->
          let window = (LTerm_windows.get_console_screen_buffer_info term.outgoing_fd).LTerm_windows.window in
          return (LTerm_event.Mouse {
                    mouse with
                      LTerm_mouse.row = mouse.LTerm_mouse.row - window.row1;
                      LTerm_mouse.col = mouse.LTerm_mouse.col - window.col1;
                  })
  else if term.resize_received then begin
    term.resize_received <- false;
    lwt size = get_size term in
    return (LTerm_event.Resize size)
  end else
    pick [LTerm_unix.parse_event ~escape_time:term.escape_time term.incoming_encoding term.input_stream >|= (fun ev -> `Event ev);
          Lwt_condition.wait term.event_cond] >>= function
      | `Event ev ->
          return ev
      | `Resize ->
          term.resize_received <- false;
          lwt size = get_size term in
          return (LTerm_event.Resize size)

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

let vline = UChar.of_char '\xb3'
let vlline = UChar.of_char '\xb4'
let dlcorner = UChar.of_char '\xbf'
let urcorner = UChar.of_char '\xc0'
let huline = UChar.of_char '\xc1'
let hdline = UChar.of_char '\xc2'
let vrline = UChar.of_char '\xc3'
let hline = UChar.of_char '\xc4'
let cross = UChar.of_char '\xc5'
let ulcorner = UChar.of_char '\xd9'
let drcorner = UChar.of_char '\xda'

(* Maps unicode characters used for drawing on windows. *)
let windows_map_char char =
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
    | _ -> char

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

let expand term text =
  if Array.length text = 0 then
    ""
  else begin
    let open LTerm_style in
    let buf = Buffer.create 256 in
    Buffer.add_string buf "\027[0m";
    let rec loop idx prev_style =
      if idx = Array.length text then begin
        Buffer.add_string buf "\027[0m";
        Buffer.contents buf
      end else begin
        let ch, style = Array.unsafe_get text idx in
        if not (LTerm_style.equal style prev_style) then begin
          Buffer.add_string buf "\027[0";
          (match style.bold with Some true -> Buffer.add_string buf Codes.bold | _ -> ());
          (match style.underline with Some true -> Buffer.add_string buf Codes.underline | _ -> ());
          (match style.blink with Some true -> Buffer.add_string buf Codes.blink | _ -> ());
          (match style.reverse with Some true -> Buffer.add_string buf Codes.reverse | _ -> ());
          (match style.foreground with Some color -> add_color term buf Codes.foreground color | None -> ());
          (match style.background with Some color -> add_color term buf Codes.background color | None -> ());
          Buffer.add_char buf 'm';
        end;
        Buffer.add_string buf (Zed_utf8.singleton ch);
        loop (idx + 1) style
      end
    in
    loop 0 none
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
  for y = 0 to Array.length matrix - 1 do
    let line = Array.unsafe_get matrix y in
    (* If the current line is equal to the displayed one, skip it *)
    if y >= Array.length old_matrix || line <> Array.unsafe_get old_matrix y then begin
      for x = 0 to Array.length line - 1 do
        let point = Array.unsafe_get line x in
        render_point term buf !last_point point;
        last_point := point
      done
    end;
    if y < Array.length matrix - 1 then  Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf "\027[0m";
  fprint term (Buffer.contents buf)

let render_windows term kind matrix =
  let open LTerm_draw in
  (* Build the matrix of char infos *)
  let matrix =
    Array.map
      (fun line ->
         Array.map
           (fun point ->
              if point.reverse then {
                LTerm_windows.ci_char = windows_map_char point.char;
                LTerm_windows.ci_foreground = windows_bg_color term point.background;
                LTerm_windows.ci_background = windows_fg_color term point.foreground;
              } else {
                LTerm_windows.ci_char = windows_map_char point.char;
                LTerm_windows.ci_foreground = windows_fg_color term point.foreground;
                LTerm_windows.ci_background = windows_bg_color term point.background;
              })
           line)
      matrix
  in
  let info = LTerm_windows.get_console_screen_buffer_info term.outgoing_fd in
  let window_rect = info.LTerm_windows.window in
  let rect =
    match kind with
      | Render_screen ->
          window_rect
      | Render_box ->
          { window_rect with
              row1 = info.LTerm_windows.cursor_position.row;
              row2 = info.LTerm_windows.cursor_position.row + Array.length matrix }
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
      render_windows term Render_screen matrix
    else
      render_update_unix term Render_screen old_matrix matrix
  else
    raise_lwt Not_a_tty

let render term m = render_update term [||] m

let print_box term matrix =
  if term.outgoing_is_a_tty then
    if term.windows then
      render_windows term Render_box matrix
    else
      render_update_unix term Render_box [||] matrix
  else
    raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let flush term = Lwt_io.flush term.oc

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
