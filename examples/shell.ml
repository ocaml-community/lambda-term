(*
 * shell.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* A mini shell *)

open CamomileLibraryDyn.Camomile
open React
open Lwt
open Lt_style
open Lt_types

(* +-----------------------------------------------------------------+
   | Prompt creation                                                 |
   +-----------------------------------------------------------------+ *)

(* [size] is the size of the terminal and [exit_code] is the exit code
   of the last executed command. *)
let make_prompt size exit_code time =
  let tm = Unix.localtime time in
  let code = string_of_int exit_code in

  (* Replace the home directory by "~" in the current path. *)
  let path = Sys.getcwd () in
  let path =
    try
      let home = Sys.getenv "HOME" in
      if Zed_utf8.starts_with path home then
        Zed_utf8.replace path 0 (Zed_utf8.length home) "~"
      else
        path
    with Not_found ->
      path
  in

  (* Shorten the path if it is too large for the size of the
     terminal. *)
  let path_len = Zed_utf8.length path in
  let size_for_path = size.columns - 24 - Zed_utf8.length code in
  let path =
    if path_len > size_for_path then
      if size_for_path >= 2 then
        ".." ^ Zed_utf8.after path (path_len - size_for_path + 2)
      else
        path
    else
      path
  in

  [Bold;
   Foreground lblue; String "─( ";
   Foreground lmagenta; format "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
   Foreground lblue; String " )─< ";
   Foreground lyellow; String path;
   Foreground lblue; String " >─"; String(Zed_utf8.make (size.columns - 24 - Zed_utf8.length code - Zed_utf8.length path) (UChar.of_int 0x2500)); String "[ ";
   Foreground (if exit_code = 0 then lwhite else lred); String code;
   Foreground lblue; String " ]─";
   Foreground lred; String(try Sys.getenv "USER" with Not_found -> "");
   Foreground lgreen; String "@";
   Foreground lblue; String(Unix.gethostname ());
   Foreground lgreen; String " $ "]

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

(* Signal updated every second with the current time. *)
let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~history ~exit_code = object(self)
  inherit Lt_read_line.read_line ~history ()
  inherit [Zed_utf8.t] Lt_read_line.term Lt_term.stdout

  initializer
    prompt <- S.l2 (fun size time -> make_prompt size exit_code time) self#size time
end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec loop history exit_code =
  lwt size = Lt_term.get_size Lt_term.stdout in
  lwt command = (new read_line ~history ~exit_code)#run in
  lwt status = Lwt_process.exec (Lwt_process.shell command) in
  loop
    (Lt_read_line.add_entry command history)
    (match status with
       | Unix.WEXITED code -> code
       | Unix.WSIGNALED code -> code
       | Unix.WSTOPPED code -> code)

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () = loop [] 0
