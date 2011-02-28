(*
 * lt_term.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open React

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

type t = {
  model : string;
  colors : int;
  windows : bool;
  bold_is_bright : bool;
  color_map : Lt_color_mappings.map;
  (* Informations. *)

  raw_mode : bool signal;
  set_raw_mode : bool -> unit;
  (* The current raw mode. *)

  mutable saved_attr : Unix.terminal_io option;
  (* Saved terminal attributes. *)

  mouse_mode : bool signal;
  set_mouse_mode : bool -> unit;
  (* The current mouse mode. *)

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
  let raw_mode, set_raw_mode = S.create false in
  let mouse_mode, set_mouse_mode = S.create false in
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
    raw_mode;
    set_raw_mode;
    saved_attr = None;
    mouse_mode;
    set_mouse_mode;
    incoming_fd;
    outgoing_fd;
    ic;
    oc;
    incoming_cd = Lt_iconv.iconv_open ~to_code:"UCS-4BE" ~of_code:incoming_encoding;
    outgoing_cd = Lt_iconv.iconv_open ~to_code:(outgoing_encoding ^ "//TRANSLIT") ~of_code:"UTF-8";
    input_stream = Lwt_stream.from (fun () -> Lwt_io.read_char_opt ic);
    size_changed = false;
    size_changed_cond = Lwt_condition.create ();
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

let get_size term = get_size_from_fd term.outgoing_fd
let set_size term size = set_size_from_fd term.outgoing_fd size

(* +-----------------------------------------------------------------+
   | Modes                                                           |
   +-----------------------------------------------------------------+ *)

let raw_mode term = term.raw_mode

let enter_raw_mode term =
  if term.windows || S.value term.raw_mode then
    return ()
  else begin
    lwt attr = Lwt_unix.tcgetattr term.incoming_fd in
    term.saved_attr <- Some attr;
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
    term.set_raw_mode true;
    return ()
  end

let leave_raw_mode term =
  if term.windows || not (S.value term.raw_mode) then
    return ()
  else
    match term.saved_attr with
      | Some attr ->
          term.saved_attr <- None;
          term.set_raw_mode false;
          Lwt_unix.tcsetattr term.incoming_fd Unix.TCSAFLUSH attr
      | None ->
          return ()

let mouse_mode term = term.mouse_mode

let enter_mouse_mode term =
  if term.windows || S.value term.mouse_mode then
    return ()
  else begin
    term.set_mouse_mode true;
    Lwt_io.write term.oc "\027[?1000h"
  end

let leave_mouse_mode term =
  if term.windows || not (S.value term.mouse_mode) then
    return ()
  else begin
    term.set_mouse_mode false;
    Lwt_io.write term.oc "\027[?1000l"
  end

(* +-----------------------------------------------------------------+
   | Cursor                                                          |
   +-----------------------------------------------------------------+ *)

let show_cursor term =
  if term.windows then begin
    let size, _ = Lt_windows.get_console_cursor_info term.outgoing_fd in
    Lt_windows.set_console_cursor_info term.outgoing_fd size true;
    return ()
  end else
    Lwt_io.write term.oc "\027[?25h"

let hide_cursor term =
  if term.windows then begin
    let size, _ = Lt_windows.get_console_cursor_info term.outgoing_fd in
    Lt_windows.set_console_cursor_info term.outgoing_fd size false;
    return ()
  end else
    Lwt_io.write term.oc "\027[?25l"

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let save term =
  if term.windows then
    return ()
  else
    Lwt_io.write term.oc "\027[?1049h"

let load term =
  if term.windows then
    return ()
  else
    Lwt_io.write term.oc "\027[?1049l"

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
          return (Lt_event.Resize size)

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

let fprint term str =
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

let windows_color term = function
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
          lwt () = Lwt_io.write oc (Lt_iconv.recode_with term.outgoing_cd str) in
          loop false attrs text
      | Reset :: text ->
          loop true { Lt_windows.foreground = 7; Lt_windows.background = 0 } text
      | (Bold | Underlined | Blink | Inverse | Hidden) :: text ->
          loop need_to_commit attrs text
      | Foreground col :: text ->
          loop true { attrs with Lt_windows.foreground = windows_color term col } text
      | Background col :: text ->
          loop true { attrs with Lt_windows.background = windows_color term col } text
  in
  loop false (Lt_windows.get_console_screen_buffer_info term.outgoing_fd).Lt_windows.attributes text

let fprints term text =
  Lwt_unix.isatty term.outgoing_fd >>= function
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

let render_update term old_matrix matrix =
  let open Lt_draw in
  let buf = Buffer.create 16 in
  (* Go the the top-left and reset attributes *)
  Buffer.add_string buf "\027[H\027[0m";
  (* The last displayed point. *)
  let last_point = ref {
    char = 32;
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
      if Array.length line > 0 then
        last_point := Array.unsafe_get line (Array.length line - 1)
    end else begin
      for x = 0 to Array.length line - 1 do
        let point = Array.unsafe_get line x in
        if not (same_style point !last_point) then begin
          (* Reset styles if they are different from the previous
             point. *)
          Buffer.add_string buf "\027[0";
          if point.bold then Buffer.add_string buf ";1";
          if point.underlined then Buffer.add_string buf ";4";
          if point.blink then Buffer.add_string buf ";5";
          add_color term buf Codes.foreground point.foreground;
          add_color term buf Codes.background point.background;
          Buffer.add_char buf 'm';
        end;
        Buffer.add_string buf (Lt_utf8.singleton point.char);
        last_point := point
      done
    end;
    if y < Array.length matrix - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf "\027[0m";
  fprint term (Buffer.contents buf)

let render term m = render_update term [||] m

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
