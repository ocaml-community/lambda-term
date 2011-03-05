(*
 * clock.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open React
open Lwt

(* Format the time. *)
let format_time time =
  let localtime = Unix.localtime time in
  Printf.sprintf "%02u:%02u:%02u"
    localtime.Unix.tm_hour
    localtime.Unix.tm_min
    localtime.Unix.tm_sec

lwt () =
  (* Create a thread waiting for escape to be pressed. *)
  let waiter, wakener = wait () in

  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));

  (* Create the label. *)
  let label = new Lt_widget.label (S.map format_time time) in

  (* Exit when escape is pressed. *)
  Lwt_event.always_notify
    (function
       | { Lt_key.code = Lt_key.Escape } -> wakeup wakener ()
       | _ -> ())
    label#key_pressed;

  (* Run. *)
  Lt_widget.run Lt_term.stdout label waiter
