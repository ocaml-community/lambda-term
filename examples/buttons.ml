(*
 * buttons.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open LTerm_widget

let main () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let button = new button
    ~brackets:("[ ", " ]")
    "exit退出"
  in
  let label = new label "_" in
  button#on_click (wakeup wakener);
  vbox#add button;
  vbox#add label;

  for i = 0 to 2 do
    let hbox = new hbox in
    let button i = 
      let button = new button ("button按钮" ^ string_of_int i) in
      button#on_click (fun () -> label#set_text (string_of_int i));
      button
    in
    hbox#add (button (i * 3 + 1));
    hbox#add ~expand:false (new vline);
    hbox#add (button (i * 3 + 2));
    hbox#add ~expand:false (new vline);
    hbox#add (button (i * 3 + 3));
    vbox#add ~expand:false (new hline);
    vbox#add hbox
  done;

  let frame = new frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center "Button test按钮测试";

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term frame waiter)
    (fun () -> LTerm.disable_mouse term)


let () = Lwt_main.run (main ())
