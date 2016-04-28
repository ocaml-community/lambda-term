(*
 * editint.ml
 * ---------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt

let main () =
  let waiter, wakener = wait () in

  let vbox = new LTerm_widget.vbox in
  let frame = new LTerm_widget.frame in
  let edit = new LTerm_edit.edit_integer in
  let exit = new LTerm_widget.button "exit" in
  let label = new LTerm_widget.label "" in
  let hbox = new LTerm_widget.hbox in
  let incr = new LTerm_widget.button "incr" in
  let decr = new LTerm_widget.button "decr" in
  frame#set edit;
  hbox#add ~expand:false decr;
  hbox#add frame;
  hbox#add ~expand:false incr;
  vbox#add ~expand:false hbox;
  vbox#add label;
  vbox#add ~expand:false exit;

  let set_value f = 
    let x = 
      match edit#value with
      | None -> 0
      | Some x -> x
    in
    edit#set_value (f x)
  in
  let set_label () = 
    match edit#value with 
    | None -> label#set_text "none"
    | Some(x) -> label#set_text (string_of_int x)
  in
  incr#on_click (fun () -> set_value ((+)1); set_label ());
  decr#on_click (fun () -> set_value ((+)(-1)); set_label ());

  let open LTerm_key in
  edit#on_event 
    (function LTerm_event.Key { code=Enter } -> set_label (); true
            | _ -> false);
  vbox#on_event 
    (function LTerm_event.Key { code=Escape } -> wakeup wakener (); true
            | _ -> false);
  exit#on_click (wakeup wakener);

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

