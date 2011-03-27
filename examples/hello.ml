(*
 * hello.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt_react
open Lwt

lwt () =
  (* Create a thread waiting for escape to be pressed. *)
  let waiter, wakener = wait () in

  (* Create the UI. *)
  let ui =
    Lt_widget.event_box
      ~on_event:(function
                   | Lt_event.Key{ Lt_key.code = Lt_key.Escape } -> wakeup wakener ()
                   | _ -> ())
      (Lt_widget.vbox [
         Lt_widget.label "Hello, world!";
         Lt_widget.label "Press escape to exit.";
       ])
  in

  (* Run. *)
  lwt term = Lazy.force Lt_term.stdout in
  Lt_widget.run term ui waiter
