(*
 * hello.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open React
open Lwt

lwt () =
  (* Create a thread waiting for escape to be pressed. *)
  let waiter, wakener = wait () in

  (* Create the label. *)
  let label = new Lt_widget.label (S.const "Hello, world!") in

  (* Exit when escape is pressed. *)
  Lwt_event.always_notify
    (function
       | { Lt_key.code = Lt_key.Escape } -> wakeup wakener ()
       | _ -> ())
    label#key_pressed;

  (* Run. *)
  Lt_widget.run Lt_term.stdout label waiter
