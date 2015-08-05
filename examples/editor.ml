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

let main () =
  let waiter, wakener = wait () in

  let frame = new LTerm_widget.frame in
  let editor = new LTerm_edit.edit () in

  frame#set editor;

  (* Exit when the user presses C-x C-c *)
  editor#bind
    (let open LTerm_key in
     [ { control = true; meta = false; shift = false
       ; code = Char (UChar.of_char 'x') }
     ; { control = true; meta = false; shift = false
       ; code = Char (UChar.of_char 'c') }
     ])
    [ LTerm_edit.Custom (fun () -> wakeup wakener ()) ];

  Zed_edit.insert editor#context
    (Zed_rope.of_string "\
This is a simple edition widget.

Type C-x C-c to exit.

");

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm_widget.run term frame waiter

let () = Lwt_main.run (main ())
