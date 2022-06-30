(*
 * editor.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt

let main () =
  let waiter, wakener = wait () in

  let hbox = new LTerm_widget.hbox in
  let frame = new LTerm_widget.frame in
  let editor = new LTerm_edit.edit () in
  let vscroll = new LTerm_widget.vscrollbar ~width:1 editor#vscroll in
  frame#set editor;
  hbox#add frame;
  hbox#add ~expand:false vscroll;

  (* Exit when the user presses C-x C-c *)
  editor#bind
    (let open LTerm_key in
     [ { control = true; meta = false; shift = false
       ; code = Char (Uchar.of_char 'x') }
     ; { control = true; meta = false; shift = false
       ; code = Char (Uchar.of_char 'c') }
     ])
    [ LTerm_edit.Custom (fun () -> wakeup wakener ()) ];

  Zed_edit.insert editor#context
    (Zed_rope.of_string @@ Zed_string.of_utf8 "\
This is a simple edition widget.

Type C-x C-c to exit.

");

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term hbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
