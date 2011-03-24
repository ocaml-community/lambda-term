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

  (* Create the label. *)
  let widget =
    Lt_widget.vbox [
      Lt_widget.label "Hello, world!";
      Lt_widget.label "Press escape to exit.";
    ]
  in

  (* Exit when escape is pressed. *)
  E.keep
    (E.map
       (function
          | { Lt_key.code = Lt_key.Escape } -> wakeup wakener ()
          | _ -> ())
       widget#key_pressed);

  (* Run. *)
  lwt term = Lazy.force Lt_term.stdout in
  Lt_widget.run term widget waiter
