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

type size = Lt_event.size = { lines : int; columns : int }

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

type t = {
  model : string;
  windows : bool;
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
    windows;
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
    outgoing_cd = Lt_iconv.iconv_open ~to_code:outgoing_encoding ~of_code:"UTF-8";
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
let windows t = t.windows

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
   | Standard terminals                                              |
   +-----------------------------------------------------------------+ *)

let stdout = create Lwt_unix.stdin Lwt_unix.stdout
let stderr = create Lwt_unix.stdin Lwt_unix.stderr
