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

  (* Create the exit button. *)
  let button = new Lt_widget.button (S.const "exit") in

  (* Create widgets. *)
  let widget =
    new Lt_widget.vbox
      (S.const [
         (new Lt_widget.label (S.map format_time time) :> Lt_widget.t);
         button#as_widget;
       ])
  in

  (* Run. *)
  lwt term = Lazy.force Lt_term.stdout in
  Lt_widget.run term widget (E.next button#clicked)
