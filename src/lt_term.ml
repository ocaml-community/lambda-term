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
   | Low-level functions                                             |
   +-----------------------------------------------------------------+ *)

external get_size : Unix.file_descr -> size = "lt_term_get_size"
external set_size : Unix.file_descr -> size -> unit = "lt_term_set_size"

let get_size fd =
  Lwt_unix.check_descriptor fd;
  return (get_size (Lwt_unix.unix_file_descr fd))

let set_size fd size =
  Lwt_unix.check_descriptor fd;
  return (set_size (Lwt_unix.unix_file_descr fd) size)

(* +-----------------------------------------------------------------+
   | Terminal class                                                  |
   +-----------------------------------------------------------------+ *)

let default_model =
  try
    Sys.getenv "TERM"
  with Not_found ->
    "dumb"

class t ?(model=default_model) ~input ~input_encoding ~output ~output_encoding ?(windows=Lwt_sys.windows) () =
  let raw_mode, set_raw_mode = S.create false in
  let ic = Lwt_text.make ~encoding:input_encoding (Lwt_io.of_fd ~mode:Lwt_io.input input)
  and oc = Lwt_text.make ~encoding:output_encoding (Lwt_io.of_fd ~mode:Lwt_io.output output) in
  let input_stream = Lwt_stream.from (fun () -> Lwt_text.read_char_opt ic) in
  let size_changed = ref false in
  let size_changed_cond = Lwt_condition.create () in
  let () =
    if not windows then
      match Lt_unix.sigwinch with
        | Some signum ->
            ignore
              (Lwt_unix.on_signal signum
                 (fun _ ->
                    size_changed := true;
                    Lwt_condition.signal size_changed_cond `Resize))
        | None ->
            ()
  in
object(self)

  method model = model
  method windows = windows

  method get_size = get_size output
  method set_size size = set_size output size

  method raw_mode = raw_mode
  val mutable saved_attr = None

  method enter_raw_mode =
    if windows || S.value raw_mode then
      return ()
    else begin
      lwt attr = Lwt_unix.tcgetattr input in
      saved_attr <- Some attr;
      lwt () = Lwt_unix.tcsetattr input Unix.TCSAFLUSH {
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
      set_raw_mode true;
      return ()
    end

  method leave_raw_mode =
    if windows || not (S.value raw_mode) then
      return ()
    else
      match saved_attr with
        | Some attr ->
            saved_attr <- None;
            set_raw_mode false;
            Lwt_unix.tcsetattr input Unix.TCSAFLUSH attr
        | None ->
            return ()

  method save =
    (*Icu_io.write oc u"\027[?1049h\027[?1h\027=\r"*)
    Lwt_text.write oc "\027[?1049h"

  method load =
    (*Icu_io.write oc u"\r\027[K\027[?1l\027>\027[r\027[?1049l"*)
    Lwt_text.write oc "\027[?1049l"

  method read_event =
    if windows then
      Lt_windows.read_console_input input >>= function
        | Lt_windows.Resize ->
            lwt size = self#get_size in
            return (Lt_event.Resize size)
        | Lt_windows.Key(mods, key) ->
            return (Lt_event.Key(mods, key))
    else if !size_changed then begin
      size_changed := false;
      lwt size = self#get_size in
      return (Lt_event.Resize size)
    end else
      pick [Lt_unix.get_sequence input_stream >|= (fun seq -> `Seq seq);
            Lwt_condition.wait size_changed_cond] >>= function
        | `Seq seq ->
            return (Lt_unix.parse_event seq)
        | `Resize ->
            size_changed := false;
            lwt size = self#get_size in
            return (Lt_event.Resize size)
end

let stdout =
  if Lwt_sys.windows then
    new t
      ~input:Lwt_unix.stdin
      ~input_encoding:(Printf.sprintf "cp%d" (Lt_windows.get_console_cp ()))
      ~output:Lwt_unix.stdout
      ~output_encoding:(Printf.sprintf "cp%d" (Lt_windows.get_console_output_cp ()))
      ()
  else
    new t
      ~input:Lwt_unix.stdin
      ~input_encoding:Encoding.system
      ~output:Lwt_unix.stdout
      ~output_encoding:Encoding.system
      ()

let stderr =
  if Lwt_sys.windows then
    new t
      ~input:Lwt_unix.stdin
      ~input_encoding:(Printf.sprintf "cp%d" (Lt_windows.get_console_cp ()))
      ~output:Lwt_unix.stderr
      ~output_encoding:(Printf.sprintf "cp%d" (Lt_windows.get_console_output_cp ()))
      ()
  else
    new t
      ~input:Lwt_unix.stdin
      ~input_encoding:Encoding.system
      ~output:Lwt_unix.stderr
      ~output_encoding:Encoding.system
      ()
