(*
 * clock.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt_react
open Lwt
open Lt_widget

(* Format the time. *)
let format_time time =
  let localtime = Unix.localtime time in
  Printf.sprintf "%02u:%02u:%02u"
    localtime.Unix.tm_hour
    localtime.Unix.tm_min
    localtime.Unix.tm_sec

lwt () =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));

  let waiter, wakener = wait () in

  (* Create the UI. *)
  let ui =
    vbox [
      changeable (S.map ~eq:(==) (fun time -> label (format_time time)) time);
      button ~on_click:(wakeup wakener) "exit";
    ]
  in

  (* Run in the standard terminal. *)
  lwt term = Lazy.force Lt_term.stdout in
  run term ui waiter
