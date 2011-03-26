(*
 * buttons.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open Lwt_react
open Lt_widget

lwt () =
  (* Create the exit button. *)
  let button_exit = button "exit" in
  let widget =
    frame
      (vbox [
         button_exit;
         hline ();
         hbox [button "button1";
               vline ();
               button "button2";
               vline ();
               button "button3"];
         hline ();
         hbox [button "button4";
               vline ();
               button "button5";
               vline ();
               button "button6"];
         hline ();
         hbox [button "button7";
               vline ();
               button "button8";
               vline ();
               button "button9"];
       ])
  in
  lwt term = Lazy.force Lt_term.stdout in
  run term widget (E.next button_exit#clicked)
