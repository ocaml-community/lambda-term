(*
 * buttons.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt_react
open Lwt
open Lt_widget

lwt () =
  let button = new button (S.const "exit") in
  let widget =
    new vbox
      (S.const [
         button#as_widget;
         (new hbox (S.const [(new button (S.const "button1"))#as_widget;
                             (new button (S.const "button2"))#as_widget;
                             (new button (S.const "button3"))#as_widget]))#as_widget;
         (new hbox (S.const [(new button (S.const "button4"))#as_widget;
                             (new button (S.const "button5"))#as_widget;
                             (new button (S.const "button6"))#as_widget]))#as_widget;
         (new hbox (S.const [(new button (S.const "button7"))#as_widget;
                             (new button (S.const "button8"))#as_widget;
                             (new button (S.const "button9"))#as_widget]))#as_widget;
       ])
  in
  lwt term = Lazy.force Lt_term.stdout in
  run term widget (E.next button#clicked)
