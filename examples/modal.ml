open Lwt
open Lwt_react
open LTerm_widget

lwt () =
  let waiter, wakener = wait () in
  let push_ev, push_layer = E.create () in
  let pop_ev, pop_layer = E.create () in

  let vbox = new vbox in

  let button = new button "exit" in
  button#on_click (wakeup wakener);
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
  change#on_click (fun () -> push_layer layer2);

  lwt term = Lazy.force LTerm.stdout in
  run_modal term push_ev pop_ev frame waiter
