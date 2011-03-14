(*
 * lt_term.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt
open Lt_geom

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
  color_map : Lt_color_mappings.map;
  (* Informations. *)

  mutable raw_mode : bool;
  (* Whether the terminal is currently in raw mode. *)

  incoming_fd : Lwt_unix.file_descr;
  outgoing_fd : Lwt_unix.file_descr;
  (* File descriptors. *)

  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  (* Channels. *)

  incoming_cd : Lt_iconv.t;
  outgoing_cd : Lt_iconv.t;
  (* Conversion descriptors. *)

  input_stream : char Lwt_stream.t;
  (* Stream of character input from the terminal. *)

  mutable size_changed : bool;
  (* Whether the size have changed and the corresponding event have
     not yet been reported. *)

  size_changed_cond : [ `Resize | `Event of Lt_event.t ] Lwt_condition.t;
  (* Condition used to wakeup the read_event thread on unix. *)

  incoming_is_a_tty : bool;
  outgoing_is_a_tty : bool;
  (* Whether input/output are tty devices. *)
}

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let default_model =
  try
    Sys.getenv "TERM"
  with Not_found ->
    "dumb"

let colors_of_term = function
  | "xterm" -> 256
  | "rxvt-256color" -> 256
  | "rxvt-unicode" -> 88
  | "rxvt" -> 88
  | _ -> 16

let create ?(windows=Lwt_sys.windows) ?(model=default_model) ?incoming_encoding ?outgoing_encoding incoming_fd ic outgoing_fd oc =
  let incoming_encoding, outgoing_encoding =
    match windows with
      | true ->
          (Printf.sprintf "cp%d" (Lt_windows.get_console_cp ()),
           Printf.sprintf "cp%d" (Lt_windows.get_console_output_cp ()))
      | false ->
          (Lt_unix.system_encoding,
           Lt_unix.system_encoding)
  in
  let colors = colors_of_term model in
  lwt incoming_is_a_tty = Lwt_unix.isatty incoming_fd
  and outgoing_is_a_tty = Lwt_unix.isatty outgoing_fd in
  let term = {
    model;
    colors;
    windows;
    bold_is_bright = model = "linux";
    color_map =
      (match colors with
         | 16 -> Lt_color_mappings.colors_16
         | 88 -> Lt_color_mappings.colors_88
         | 256 -> Lt_color_mappings.colors_256
         | n -> Printf.ksprintf failwith "Lt_term.create: unknown number of colors (%d)" n);
    raw_mode = false;
    incoming_fd;
    outgoing_fd;
    ic;
    oc;
    incoming_cd = Lt_iconv.iconv_open ~to_code:"UCS-4BE" ~of_code:incoming_encoding;
    outgoing_cd = Lt_iconv.iconv_open ~to_code:(outgoing_encoding ^ "//TRANSLIT") ~of_code:"UTF-8";
    input_stream = Lwt_stream.from (fun () -> Lwt_io.read_char_opt ic);
    size_changed = false;
    size_changed_cond = Lwt_condition.create ();
    incoming_is_a_tty;
    outgoing_is_a_tty;
  } in
  if not windows then begin
    match Lt_unix.sigwinch with
      | Some signum ->
          ignore
            (Lwt_unix.on_signal signum
               (fun _ ->
                  term.size_changed <- true;
                  Lwt_condition.signal term.size_changed_cond `Resize))
      | None ->
          ()
  end;
  return term

let model t = t.model
let colors t = t.colors
let windows t = t.windows
let is_a_tty t = t.incoming_is_a_tty && t.outgoing_is_a_tty
let incoming_is_a_tty t = t.incoming_is_a_tty
let outgoing_is_a_tty t = t.outgoing_is_a_tty

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
  | Mode_windows of Lt_windows.console_mode

let enter_raw_mode term =
  if term.incoming_is_a_tty then
    if term.raw_mode then
      return Mode_fake
    else if term.windows then begin
      let mode = Lt_windows.get_console_mode term.incoming_fd in
      Lt_windows.set_console_mode term.incoming_fd {
        mode with
          Lt_windows.cm_echo_input = false;
          Lt_windows.cm_line_input = false;
          Lt_windows.cm_mouse_input = true;
          Lt_windows.cm_processed_input = false;
          Lt_windows.cm_window_input = true;
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
          Unix.c_isig = false;
          Unix.c_vmin = 1;
          Unix.c_vtime = 0;
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
          Lt_windows.set_console_mode term.incoming_fd mode;
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
      let size, _ = Lt_windows.get_console_cursor_info term.outgoing_fd in
      Lt_windows.set_console_cursor_info term.outgoing_fd size true;
      return ()
    end else
      Lwt_io.write term.oc "\027[?25h"
  else
    raise_lwt Not_a_tty

let hide_cursor term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let size, _ = Lt_windows.get_console_cursor_info term.outgoing_fd in
      Lt_windows.set_console_cursor_info term.outgoing_fd size false;
      return ()
    end else
      Lwt_io.write term.oc "\027[?25l"
  else
    raise_lwt Not_a_tty

let goto term coord =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let window = (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.window in
      Lt_windows.set_console_cursor_position term.outgoing_fd {
        line = window.r_line + coord.line;
        column = window.r_column + coord.column;
      };
      return ()
    end else begin
      lwt () = Lwt_io.fprint term.oc "\027[H" in
      lwt () = if coord.line > 0 then Lwt_io.fprintf term.oc "\027[%dB" coord.line else return () in
      lwt () = if coord.line > 0 then Lwt_io.fprintf term.oc "\027[%dC" coord.column else return () in
      return ()
    end
  else
    raise_lwt Not_a_tty

let move term lines columns =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let pos = (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.cursor_position in
      Lt_windows.set_console_cursor_position term.outgoing_fd {
        line = pos.line + lines;
        column = pos.column + columns;
      };
      return ()
    end else
      lwt () =
        match lines with
          | n when n < 0 ->
              Lwt_io.fprintf term.oc "\027[%dA" (-n)
          | n when n > 0 ->
              Lwt_io.fprintf term.oc "\027[%dB" n
          | _ ->
              return ()
      and () =
        match columns with
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
      let info = Lt_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        Lt_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.Lt_windows.size.columns * info.Lt_windows.size.lines)
          { line = 0; column = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[2J"
  else
    raise_lwt Not_a_tty

let clear_screen_next term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = Lt_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        Lt_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.Lt_windows.size.columns * (info.Lt_windows.size.lines - info.Lt_windows.cursor_position.line)
           + info.Lt_windows.size.columns - info.Lt_windows.cursor_position.column)
          info.Lt_windows.cursor_position
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[J"
  else
    raise_lwt Not_a_tty

let clear_screen_prev term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = Lt_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        Lt_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.Lt_windows.size.columns * info.Lt_windows.cursor_position.line
           + info.Lt_windows.cursor_position.column)
          { line = 0; column = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[1J"
  else
    raise_lwt Not_a_tty

let clear_line term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = Lt_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        Lt_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          info.Lt_windows.size.columns
          { line = info.Lt_windows.cursor_position.line; column = 0 }
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[2K"
  else
    raise_lwt Not_a_tty

let clear_line_next term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = Lt_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        Lt_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          (info.Lt_windows.size.columns - info.Lt_windows.cursor_position.column)
          info.Lt_windows.cursor_position
      in
      return ()
    end else
      Lwt_io.write term.oc "\027[K"
  else
    raise_lwt Not_a_tty

let clear_line_prev term =
  if term.outgoing_is_a_tty then
    if term.windows then begin
      let info = Lt_windows.get_console_screen_buffer_info term.outgoing_fd in
      let _ =
        Lt_windows.fill_console_output_character
          term.outgoing_fd
          (UChar.of_char ' ')
          info.Lt_windows.cursor_position.column
          { line = info.Lt_windows.cursor_position.line; column = 0 }
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
    Lt_windows.read_console_input term.incoming_fd >>= function
      | Lt_windows.Resize ->
          lwt size = get_size term in
          return (Lt_event.Resize size)
      | Lt_windows.Key key ->
          return (Lt_event.Key key)
      | Lt_windows.Mouse mouse ->
          let window = (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.window in
          return (Lt_event.Mouse {
                    mouse with
                      Lt_mouse.line = mouse.Lt_mouse.line - window.r_line;
                      Lt_mouse.column = mouse.Lt_mouse.column - window.r_column;
                  })
  else if term.size_changed then begin
    term.size_changed <- false;
    lwt size = get_size term in
    return (Lt_event.Resize size)
  end else
    pick [Lt_unix.parse_event term.incoming_cd term.input_stream >|= (fun ev -> `Event ev);
          Lwt_condition.wait term.size_changed_cond] >>= function
      | `Event ev ->
          return ev
      | `Resize ->
          term.size_changed <- false;
          lwt size = get_size term in
          return (Lt_event.Resize size)

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

let hline = UChar.of_char '-'
let vline = UChar.of_char '|'
let corner = UChar.of_char '+'

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
    | x when x >= 0x250c && x <= 0x254b -> corner
    | 0x254c -> hline
    | 0x254d -> hline
    | 0x254e -> vline
    | 0x254f -> vline
    | 0x2550 -> hline
    | 0x2551 -> vline
    | x when x >= 0x2552 && x <= 0x2570 -> corner
    | 0x2571 -> UChar.of_char '/'
    | 0x2572 -> UChar.of_char '\\'
    | 0x2573 -> UChar.of_char 'X'
    | _ -> char

let fprint term str =
  let str = if term.windows then Zed_utf8.map windows_map_char str else str in
  Lwt_io.fprint term.oc (Lt_iconv.recode_with term.outgoing_cd str)

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
  let open Lt_color_mappings in
  let map = term.color_map in
  (* The [String.unsafe_get] is safe because the private type
     [Lt_style.color] ensure that all components are in the range
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
  | Lt_style.Default ->
      ()
  | Lt_style.Index n ->
      add_index term buf base n
  | Lt_style.RGB(r, g, b) ->
      add_index term buf base (map_color term  r g b)

let expand term text =
  if Array.length text = 0 then
    ""
  else begin
    let open Lt_style in
    let buf = Buffer.create 256 in
    Buffer.add_string buf "\027[0m";
    let rec loop idx prev_style =
      if idx = Array.length text then begin
        Buffer.add_string buf "\027[0m";
        Buffer.contents buf
      end else begin
        let ch, style = Array.unsafe_get text idx in
        if not (Lt_style.equal style prev_style) then begin
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
  | Lt_style.Default -> 7
  | Lt_style.Index n -> n
  | Lt_style.RGB(r, g, b) -> map_color term r g b

let windows_bg_color term = function
  | Lt_style.Default -> 0
  | Lt_style.Index n -> n
  | Lt_style.RGB(r, g, b) -> map_color term r g b

let windows_default_attributes = { Lt_windows.foreground = 7; Lt_windows.background = 0 }

let fprints_windows term oc text =
  let open Lt_style in
  let rec loop idx prev_attr =
    if idx = Array.length text then begin
      lwt () = Lwt_io.flush oc in
      Lt_windows.set_console_text_attribute term.outgoing_fd windows_default_attributes;
      return ()
    end else begin
      let ch, style = Array.unsafe_get text idx in
      let attr =
        if style.reverse = Some true then {
          Lt_windows.foreground = (match style.background with Some color -> windows_bg_color term color | None -> 0);
          Lt_windows.background = (match style.foreground with Some color -> windows_fg_color term color | None -> 7);
        } else {
          Lt_windows.foreground = (match style.foreground with Some color -> windows_fg_color term color | None -> 7);
          Lt_windows.background = (match style.background with Some color -> windows_bg_color term color | None -> 0);
        }
      in
      lwt () =
        if attr <> prev_attr then
          lwt () = Lwt_io.flush oc in
          Lt_windows.set_console_text_attribute term.outgoing_fd attr;
          return ()
        else
          return ()
      in
      lwt () = Lwt_io.write oc (Zed_utf8.singleton ch) in
      loop (idx + 1) attr
    end
  in
  lwt () = Lwt_io.flush oc in
  Lt_windows.set_console_text_attribute term.outgoing_fd windows_default_attributes;
  loop 0 windows_default_attributes

let fprints term text =
  if term.outgoing_is_a_tty then
    if term.windows then
      Lwt_io.atomic (fun oc -> fprints_windows term oc text) term.oc
    else
      fprint term (expand term text)
  else
    fprint term (Lt_text.to_string text)

let fprintls term text =
  fprints term (Array.append text (Lt_text.of_string "\n"))

(* +-----------------------------------------------------------------+
   | Rendering                                                       |
   +-----------------------------------------------------------------+ *)

let same_style p1 p2 =
  let open Lt_draw in
  p1.bold = p2.bold &&
      p1.underline = p2.underline &&
      p1.blink = p2.blink &&
      p1.foreground = p2.foreground &&
      p1.background = p2.background

let render_point term buf old_point new_point =
  let open Lt_draw in
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

let render_update_unix term old_matrix matrix =
  let open Lt_draw in
  let buf = Buffer.create 16 in
  (* Go the the top-left and reset attributes *)
  Buffer.add_string buf "\027[H\027[0m";
  (* The last displayed point. *)
  let last_point = ref {
    char = UChar.of_char ' ';
    bold = false;
    underline = false;
    blink = false;
    reverse = false;
    foreground = Lt_style.default;
    background = Lt_style.default;
  } in
  for y = 0 to Array.length matrix - 1 do
    let line = Array.unsafe_get matrix y in
    if y < Array.length old_matrix && line = Array.unsafe_get old_matrix y then begin
      (* If the current line is equal to the displayed one, skip it *)
      if Array.length line > 0 then begin
        let point = Array.unsafe_get line 0 in
        render_point term buf !last_point point;
        last_point := point
      end
    end else begin
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

let render_windows term matrix =
  let open Lt_draw in
  (* Build the matrix of char infos *)
  let matrix =
    Array.map
      (fun line ->
         Array.map
           (fun point ->
              if point.reverse then {
                Lt_windows.ci_char = windows_map_char point.char;
                Lt_windows.ci_foreground = windows_bg_color term point.background;
                Lt_windows.ci_background = windows_fg_color term point.foreground;
              } else {
                Lt_windows.ci_char = windows_map_char point.char;
                Lt_windows.ci_foreground = windows_fg_color term point.foreground;
                Lt_windows.ci_background = windows_bg_color term point.background;
              })
           line)
      matrix
  in
  ignore (
    Lt_windows.write_console_output
      term.outgoing_fd
      matrix
      { lines = Array.length matrix; columns = if matrix = [||] then 0 else Array.length matrix.(0) }
      { line = 0; column = 0 }
      (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.window
  );
  return ()

let render_update term old_matrix matrix =
  if term.outgoing_is_a_tty then
    if term.windows then
      render_windows term matrix
    else
      render_update_unix term old_matrix matrix
  else
    raise_lwt Not_a_tty

let render term m = render_update term [||] m

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
