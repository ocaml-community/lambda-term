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
  let waiter, wakener = wait () in
  let widget =
    frame
      (vbox [
         button ~on_click:(wakeup wakener) "exit";
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
  run term widget waiter
