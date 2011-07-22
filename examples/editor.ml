(*
 * editor.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt

lwt () =
  let waiter, wakener = wait () in

  let frame = new LTerm_widget.frame in
  let editor = new LTerm_edit.edit () in

  frame#set editor;

  (* Exit when the user presses Ctrl+X *)
  frame#on_event (function
                    | LTerm_event.Key { LTerm_key.control = true; meta = false; shift = false; code = LTerm_key.Char ch } when ch = UChar.of_char 'x'->
                        wakeup wakener ();
                        true
                    | _ ->
                        false);

  Zed_edit.insert editor#context
    (Zed_rope.of_string "\
This is a simple edition widget.

Type Control+X to exit.

");

  lwt term = Lazy.force LTerm.stdout in
  LTerm_widget.run term frame waiter
