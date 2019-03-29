(*
 * checkbuttons.ml
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
  let checked_label = new label "" in
  let create_button n = new checkbutton ("button按钮" ^ (string_of_int n)) false in
  let checkbuttons = Array.init 9 create_button in
  let callback () =
    let new_label = ref "" in
    for i = 0 to 8 do
      if checkbuttons.(i)#state
        then new_label := !new_label ^ " " ^ string_of_int i
        else ()
    done;
    checked_label#set_text !new_label
  in
  let button = new button "exit退出" in
  button#on_click (wakeup wakener);
  vbox#add ~expand:false button;
  vbox#add ~expand:false (new hline);


  let use_checkbutton n =
    let cb = checkbuttons.(n) in
    cb#on_click callback;
    cb in
  for i = 0 to 2 do
    let hbox = new hbox in
    hbox#add (use_checkbutton i);
    hbox#add ~expand:false (new vline);
    hbox#add (use_checkbutton (i + 3));
    hbox#add ~expand:false (new vline);
    hbox#add (use_checkbutton (i + 6));

    vbox#add ~expand:false hbox
  done;

  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false checked_label;

  vbox#add (new t "glue") ;

  let frame = new frame in
  frame#set vbox;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term frame waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
