open Lwt
open LTerm_widget

let main () =
  let do_run, push_layer, pop_layer, exit = prepare_simple_run () in

  let vbox = new vbox in

  let button = new button "exit" in
  button#on_click exit;
  vbox#add button;

  let change = new button "change counter" in
  vbox#add change;

  let counter = ref 42 in
  let label = new label "" in
  let update_label () = label#set_text ("Counter: " ^ (string_of_int !counter)) in
  vbox#add label;
  update_label ();

  let change_counter d = fun () ->
    counter := !counter + d;
    update_label ()
  in

  let frame = new frame in
  frame#set vbox;

  (* Layer 2 *)
  let layer2 = new modal_frame in

  let vbox' = new vbox in
  layer2#set vbox';

  let message = new label "This is a new modal layer.\nPress 'close' to close it." in
  vbox'#add message;

  vbox'#add (new hline);

  let increment = new button "increment counter" in
  let decrement = new button "decrement counter" in
  increment#on_click (change_counter 1);
  decrement#on_click (change_counter (-1));
  vbox'#add increment;
  vbox'#add decrement;

  vbox'#add (new hline);

  let close = new button "close" in
  close#on_click pop_layer;
  vbox'#add close;

  (* set 'change' button to open modal layer *)
  change#on_click (push_layer layer2);

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
    Lwt.finalize
      (fun () -> do_run frame)
      (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
