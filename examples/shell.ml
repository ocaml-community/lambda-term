(*
 * shell.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* A mini shell *)

open React
open Lwt
open LTerm_style
open LTerm_text
open LTerm_geom

(* +-----------------------------------------------------------------+
   | Prompt creation                                                 |
   +-----------------------------------------------------------------+ *)

(* The function [make_prompt] creates the prompt. Parameters are:

   - size: the current size of the terminal.
   - exit_code: the exit code of the last executed command.
   - time: the current time. *)
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
  let size_for_path = size.cols - 24 - Zed_utf8.length code in
  let path =
    if path_len > size_for_path then
      if size_for_path >= 2 then
        ".." ^ Zed_utf8.after path (path_len - size_for_path + 2)
      else
        path
    else
      path
  in

  eval [
    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
    S" )─< ";
    B_fg lyellow; S path; E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 24 - Zed_utf8.length code - Zed_utf8.length path)
        (Uchar.of_int 0x2500));
    S"[ ";
    B_fg(if exit_code = 0 then lwhite else lred); S code; E_fg;
    S" ]─";
    E_fg;
    S"\n";

    B_fg lred; S(try Sys.getenv "USER" with Not_found -> ""); E_fg;
    B_fg lgreen; S"@"; E_fg;
    B_fg lblue; S(Unix.gethostname ()); E_fg;
    B_fg lgreen; S" $ "; E_fg;

    E_bold;
  ]

(* +-----------------------------------------------------------------+
   | Listing binaries of the path for completion                     |
   +-----------------------------------------------------------------+ *)

module String_set = Set.Make(String)

let colon_re = Str.regexp ":"
let get_paths () =
  try
    Str.split colon_re (Sys.getenv "PATH")
  with Not_found ->
    []

(* Get the set of all binaries with a name starting with [prefix]. *)
let get_binaries () =
  Lwt_list.fold_left_s
    (fun set dir ->
       Lwt.catch (fun () ->
         Lwt_stream.fold
           (fun file set ->
              if file <> "." && file <> ".." then
                String_set.add file set
              else
                set)
           (Lwt_unix.files_of_directory dir)
           set)
         (function
           | Unix.Unix_error _ -> return set
           | exn -> Lwt.fail exn))
    String_set.empty
    (get_paths ())
  >|= String_set.elements
  >|= List.map Zed_string.unsafe_of_utf8

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

(* Signal updated every second with the current time. *)
let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~history ~exit_code ~binaries = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! completion =
    let prefix  = Zed_rope.to_string self#input_prev in
    let binaries = List.filter (fun file -> Zed_string.starts_with ~prefix file) binaries in
    self#set_completion 0 (List.map (fun file -> (file, Zed_string.unsafe_of_utf8 " ")) binaries)

  initializer
    self#set_prompt (S.l2 (fun size time -> make_prompt size exit_code time) self#size time)
end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec loop term history exit_code =
  get_binaries ()
  >>= fun binaries ->
  Lwt.catch (fun () ->
    (new read_line ~term ~history:(LTerm_history.contents history)
      ~exit_code ~binaries)#run
    >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    let command_utf8= Zed_string.to_utf8 command in
    Lwt.catch (fun () -> Lwt_process.exec (Lwt_process.shell command_utf8))
      (function
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
          LTerm.fprintls term (eval [B_fg lred; S "command not found"])
          >>= fun () ->
          Lwt.return (Unix.WEXITED 127)
        | exn -> Lwt.fail exn)
    >>= fun status ->
    LTerm_history.add history command;
    loop
      term
      history
      (match status with
       | Unix.WEXITED code -> code
       | Unix.WSIGNALED code -> code
       | Unix.WSTOPPED code -> code)
  | None ->
    loop term history 130

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lwt.catch
    (fun () ->
       Lazy.force LTerm.stdout
       >>= fun term ->
       loop term (LTerm_history.create []) 0)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())
