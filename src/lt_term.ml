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

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

exception Not_a_tty

type attributes =
  | Attr_none
  | Attr_unix of Unix.terminal_io
  | Attr_windows of Lt_windows.console_mode

type t = {
  model : string;
  colors : int;
  windows : bool;
  bold_is_bright : bool;
  color_map : Lt_color_mappings.map;
  (* Informations. *)

  mutable saved_attr : attributes;
  (* Saved terminal attributes. *)

  mutable raw_mode_count : int;
  (* How many functions are currently using the raw mode. *)

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

  incoming_is_a_tty : bool Lwt.t Lazy.t;
  outgoing_is_a_tty : bool Lwt.t Lazy.t;
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

let create ?(windows=Lwt_sys.windows) ?(model=default_model) ?incoming_encoding ?outgoing_encoding incoming_fd outgoing_fd =
  let incoming_encoding, outgoing_encoding =
    match windows with
      | true ->
          (Printf.sprintf "cp%d" (Lt_windows.get_console_cp ()),
           Printf.sprintf "cp%d" (Lt_windows.get_console_output_cp ()))
      | false ->
          (Lt_unix.system_encoding,
           Lt_unix.system_encoding)
  in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input incoming_fd and oc = Lwt_io.of_fd ~mode:Lwt_io.output outgoing_fd in
  let colors = colors_of_term model in
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
    saved_attr = Attr_none;
    raw_mode_count = 0;
    incoming_fd;
    outgoing_fd;
    ic;
    oc;
    incoming_cd = Lt_iconv.iconv_open ~to_code:"UCS-4BE" ~of_code:incoming_encoding;
    outgoing_cd = Lt_iconv.iconv_open ~to_code:(outgoing_encoding ^ "//TRANSLIT") ~of_code:"UTF-8";
    input_stream = Lwt_stream.from (fun () -> Lwt_io.read_char_opt ic);
    size_changed = false;
    size_changed_cond = Lwt_condition.create ();
    incoming_is_a_tty = lazy(Lwt_unix.isatty incoming_fd);
    outgoing_is_a_tty = lazy(Lwt_unix.isatty outgoing_fd);
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
  term

let model t = t.model
let colors t = t.colors
let windows t = t.windows
let is_a_tty t =
  lwt a = Lazy.force t.incoming_is_a_tty and b = Lazy.force t.outgoing_is_a_tty in
  return (a && b)
let incoming_is_a_tty t = Lazy.force t.incoming_is_a_tty
let outgoing_is_a_tty t = Lazy.force t.outgoing_is_a_tty

(* +-----------------------------------------------------------------+
   | Sizes                                                           |
   +-----------------------------------------------------------------+ *)

external get_size_from_fd : Unix.file_descr -> Lt_types.size = "lt_term_get_size_from_fd"
external set_size_from_fd : Unix.file_descr -> Lt_types.size -> unit = "lt_term_set_size_from_fd"

let get_size_from_fd fd =
  Lwt_unix.check_descriptor fd;
  return (get_size_from_fd (Lwt_unix.unix_file_descr fd))

let set_size_from_fd fd size =
  Lwt_unix.check_descriptor fd;
  return (set_size_from_fd (Lwt_unix.unix_file_descr fd) size)

let get_size term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        get_size_from_fd term.outgoing_fd
    | false ->
        raise_lwt Not_a_tty

let set_size term size =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        set_size_from_fd term.outgoing_fd size
    | false ->
        raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Modes                                                           |
   +-----------------------------------------------------------------+ *)

let enter_raw_mode term =
  match term.saved_attr with
    | Attr_unix _ | Attr_windows _ ->
        return ()
    | Attr_none ->
        if term.windows then begin
          let mode = Lt_windows.get_console_mode term.incoming_fd in
          term.saved_attr <- Attr_windows mode;
          Lt_windows.set_console_mode term.incoming_fd {
            mode with
              Lt_windows.cm_echo_input = false;
              Lt_windows.cm_line_input = false;
              Lt_windows.cm_mouse_input = true;
              Lt_windows.cm_processed_input = false;
              Lt_windows.cm_window_input = true;
          };
          return ()
        end else begin
          lwt attr = Lwt_unix.tcgetattr term.incoming_fd in
          term.saved_attr <- Attr_unix attr;
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
          return ()
        end

let leave_raw_mode term =
  match term.saved_attr with
    | Attr_none ->
        return ()
    | Attr_unix attr ->
        term.saved_attr <- Attr_none;
        Lwt_unix.tcsetattr term.incoming_fd Unix.TCSAFLUSH attr
    | Attr_windows mode ->
        term.saved_attr <- Attr_none;
        Lt_windows.set_console_mode term.incoming_fd mode;
        return ()

let with_raw_mode term f =
  Lazy.force term.incoming_is_a_tty >>= function
    | false ->
        raise_lwt Not_a_tty
    | true ->
        lwt () =
          if term.raw_mode_count > 0 then
            return ()
          else
            enter_raw_mode term
        in
        term.raw_mode_count <- term.raw_mode_count + 1;
        try_lwt
          f ()
        finally
          term.raw_mode_count <- term.raw_mode_count - 1;
          if term.raw_mode_count = 0 then
            leave_raw_mode term
          else
            return ()

let enable_mouse term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then
          return ()
        else
          Lwt_io.write term.oc "\027[?1000h"
    | false ->
        raise_lwt Not_a_tty

let disable_mouse term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then
          return ()
        else
          Lwt_io.write term.oc "\027[?1000l"
    | false ->
        raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Cursor                                                          |
   +-----------------------------------------------------------------+ *)

let show_cursor term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then begin
          let size, _ = Lt_windows.get_console_cursor_info term.outgoing_fd in
          Lt_windows.set_console_cursor_info term.outgoing_fd size true;
          return ()
        end else
          Lwt_io.write term.oc "\027[?25h"
    | false ->
        raise_lwt Not_a_tty

let hide_cursor term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then begin
          let size, _ = Lt_windows.get_console_cursor_info term.outgoing_fd in
          Lt_windows.set_console_cursor_info term.outgoing_fd size false;
          return ()
        end else
          Lwt_io.write term.oc "\027[?25l"
    | false ->
        raise_lwt Not_a_tty

let goto term coord =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then begin
          let window = (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.window in
          Lt_windows.set_console_cursor_position term.outgoing_fd {
            Lt_types.line = window.Lt_types.r_line + coord.Lt_types.line;
            Lt_types.column = window.Lt_types.r_column + coord.Lt_types.column;
          };
          return ()
        end else
          Lwt_io.fprintf term.oc "\027[H\027[%dB\027[%dC" coord.Lt_types.line coord.Lt_types.column
    | false ->
        raise_lwt Not_a_tty

let goto_bol term n =
  Lazy.force term.outgoing_is_a_tty >>= function
    | false ->
        raise_lwt Not_a_tty
    | true ->
        if term.windows then begin
          let pos = (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.cursor_position in
          Lt_windows.set_console_cursor_position term.outgoing_fd {
            Lt_types.line = pos.Lt_types.line + n;
            Lt_types.column = 0;
          };
          return ()
        end else
          match n with
            | n when n < 0 ->
                Lwt_io.fprintf term.oc "\r\027[%dA" (-n)
            | n when n > 0 ->
                Lwt_io.fprintf term.oc "\r\027[%dB" n
            | _ ->
                Lwt_io.write_char term.oc '\r'

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let save_state term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then
          return ()
        else
          Lwt_io.write term.oc "\027[?1049h"
    | false ->
        raise_lwt Not_a_tty

let load_state term =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then
          return ()
        else
          Lwt_io.write term.oc "\027[?1049l"
    | false ->
        raise_lwt Not_a_tty

(* +-----------------------------------------------------------------+
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

let read_event term =
  with_raw_mode term
    (fun () ->
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
                           Lt_mouse.line = mouse.Lt_mouse.line - window.Lt_types.r_line;
                           Lt_mouse.column = mouse.Lt_mouse.column - window.Lt_types.r_column;
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
               return (Lt_event.Resize size))

(* +-----------------------------------------------------------------+
   | Styled printing                                                 |
   +-----------------------------------------------------------------+ *)

module Codes = struct
  let reset = 0
  let bold = 1
  let underlined = 4
  let blink = 5
  let inverse = 7
  let hidden = 8
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

let queue_index term q base n =
  if n < 8 then
    Queue.add (base + n) q
  else if n < 16 && term.bold_is_bright then
    if base = Codes.foreground then begin
      Queue.add Codes.bold q;
      Queue.add (base + n - 8) q
    end else
      Queue.add (base + n - 8) q
  else begin
    Queue.add (base + 8) q;
    Queue.add 5 q;
    Queue.add n q
  end

let queue_color term q base = function
  | Lt_style.Default ->
      Queue.add (base + 9) q
  | Lt_style.Index n ->
      queue_index term q base n
  | Lt_style.RGB(r, g, b) ->
      queue_index term q base (map_color term  r g b)

let expand term text =
  let open Lt_style in

  let buf = Buffer.create 256 in

  (* Pendings style codes: *)
  let codes = Queue.create () in

  (* Output pending codes using only one escape sequence: *)
  let output_pendings () =
    Buffer.add_string buf "\027[";
    add_int buf (Queue.take codes);
    Queue.iter (fun code ->
                  Buffer.add_char buf ';';
                  add_int buf code) codes;
    Queue.clear codes;
    Buffer.add_char buf 'm';
  in

  let rec loop = function
    | [] ->
        if not (Queue.is_empty codes) then output_pendings ();
        Buffer.contents buf
    | instr :: rest ->
        match instr with
          | String str  ->
              if not (Queue.is_empty codes) then output_pendings ();
              Buffer.add_string buf str;
              loop rest
          | Reset ->
              Queue.add 0 codes;
              loop rest
          | Bold ->
              Queue.add Codes.bold codes;
              loop rest
          | Underlined ->
              Queue.add Codes.underlined codes;
              loop rest
          | Blink ->
              Queue.add Codes.blink codes;
              loop rest
          | Inverse ->
              Queue.add Codes.inverse codes;
              loop rest
          | Hidden ->
              Queue.add Codes.hidden codes;
              loop rest
          | Foreground col ->
              queue_color term codes Codes.foreground col;
              loop rest
          | Background col ->
              queue_color term codes Codes.background col;
              loop rest
  in
  loop text

let windows_fg_color term = function
  | Lt_style.Default -> 7
  | Lt_style.Index n -> n
  | Lt_style.RGB(r, g, b) -> map_color term r g b

let windows_bg_color term = function
  | Lt_style.Default -> 0
  | Lt_style.Index n -> n
  | Lt_style.RGB(r, g, b) -> map_color term r g b

let fprints_windows term oc text =
  let open Lt_style in
  let rec loop need_to_commit attrs text =
    match text with
      | [] ->
          if need_to_commit then begin
            lwt () = Lwt_io.flush oc in
            Lt_windows.set_console_text_attribute term.outgoing_fd attrs;
            return ()
          end else
            return ()
      | String str :: text ->
          lwt () =
            if need_to_commit then begin
              lwt () = Lwt_io.flush oc in
              Lt_windows.set_console_text_attribute term.outgoing_fd attrs;
              return ()
            end else
              return ()
          in
          lwt () = fprint term str in
          loop false attrs text
      | Reset :: text ->
          loop true { Lt_windows.foreground = 7; Lt_windows.background = 0 } text
      | (Bold | Underlined | Blink | Inverse | Hidden) :: text ->
          loop need_to_commit attrs text
      | Foreground col :: text ->
          loop true { attrs with Lt_windows.foreground = windows_fg_color term col } text
      | Background col :: text ->
          loop true { attrs with Lt_windows.background = windows_bg_color term col } text
  in
  loop false (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.attributes text

let fprints term text =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then
          Lwt_io.atomic (fun oc -> fprints_windows term oc text) term.oc
        else
          fprint term (expand term text)
    | false ->
        fprint term (Lt_style.strip text)

let fprintls term text =
  fprints term (text @ [Lt_style.String "\n"])

(* +-----------------------------------------------------------------+
   | Rendering                                                       |
   +-----------------------------------------------------------------+ *)

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

let same_style p1 p2 =
  let open Lt_draw in
  p1.bold = p2.bold &&
      p1.underlined = p2.underlined &&
      p1.blink = p2.blink &&
      p1.foreground = p2.foreground &&
      p1.background = p2.background

let render_point term buf old_point new_point =
  let open Lt_draw in
  if not (same_style new_point old_point) then begin
    (* Reset styles if they are different from the previous point. *)
    Buffer.add_string buf "\027[0";
    if new_point.bold then Buffer.add_string buf ";1";
    if new_point.underlined then Buffer.add_string buf ";4";
    if new_point.blink then Buffer.add_string buf ";5";
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
    underlined = false;
    blink = false;
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
           (fun point -> {
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
      { Lt_types.lines = Array.length matrix; Lt_types.columns = if matrix = [||] then 0 else Array.length matrix.(0) }
      { Lt_types.line = 0; Lt_types.column = 0 }
      (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.window
  );
  return ()

let render_update term old_matrix matrix =
  Lazy.force term.outgoing_is_a_tty >>= function
    | true ->
        if term.windows then
          render_windows term matrix
        else
          render_update_unix term old_matrix matrix
    | false ->
        raise_lwt Not_a_tty

let render term m = render_update term [||] m

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let flush term = Lwt_io.flush term.oc

(* +-----------------------------------------------------------------+
   | Standard terminals                                              |
   +-----------------------------------------------------------------+ *)

let stdout = create Lwt_unix.stdin Lwt_unix.stdout
let stderr = create Lwt_unix.stdin Lwt_unix.stderr

let print str = fprint stdout str
let printl str = fprintl stdout str
let printf fmt = fprintf stdout fmt
let prints str = fprints stdout str
let printlf fmt = fprintlf stdout fmt
let printls str = fprintls stdout str
let eprint str = fprint stderr str
let eprintl str = fprintl stderr str
let eprintf fmt = fprintf stderr fmt
let eprints str = fprints stderr str
let eprintlf fmt = fprintlf stderr fmt
let eprintls str = fprintls stderr str
