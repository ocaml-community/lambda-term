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
  let term = {
    model;
    colors = colors_of_term model;
    windows;
    bold_is_bright = model = "linux";
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

let read_event term =
  if term.windows then
    Lt_windows.read_console_input term.incoming_fd >>= function
      | Lt_windows.Resize ->
          lwt size = get_size term in
          return (Lt_event.Resize size)
      | Lt_windows.Key key ->
          return (Lt_event.Key key)
      | Lt_windows.Mouse mouse ->
          return (Lt_event.Mouse mouse)
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

let queue_color term q base = function
  | Lt_style.Default ->
      Queue.add (base + 9) q
  | Lt_style.Index n ->
      if n < 8 then
        Queue.add (base + n) q
      else begin
        Queue.add (base + 8) q;
        Queue.add 5 q;
        Queue.add n q
      end
  | Lt_style.Light n ->
      if n < 8 then
        if term.bold_is_bright then
          if base = Codes.foreground then begin
            Queue.add Codes.bold q;
            Queue.add (base + n) q
          end else
            Queue.add (base + n) q
        else begin
          Queue.add (base + 8) q;
          Queue.add 5 q;
          Queue.add (n + 8) q
        end
      else begin
        Queue.add (base + 8) q;
        Queue.add 5 q;
        Queue.add n q
      end
  | Lt_style.RGB _ ->
      assert false

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
  | Lt_style.Light n -> if n < 8 then n + 8 else n
  | Lt_style.RGB _ -> assert false

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
