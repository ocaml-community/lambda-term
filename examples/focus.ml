(*
 * focus.ml
 * ----------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open Lwt
open LTerm_widget

let mode = try Sys.argv.(1) with _ -> "none"

let main () = 
  let waiter, wakener = wait () in
  
  let vbox = new vbox in

  let top = new button mode in

  let leftright = new hbox in
  let left = new button "left" in
  let right = new button "right" in
  let glue = new t "glue" in
  leftright#add ~expand:false left;
  leftright#add glue;
  leftright#add ~expand:false right;

  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  vbox#add top;
  vbox#add ~expand:false leftright;
  vbox#add ~expand:false exit;

  (* we have a layout like

    [      top        ]
    [l][...........][r]
    [      exit       ]

    Focus will start in 'top'.  With no focus specifications when we press down
    focus will move to exit.  There's no way to get to the 'left'/'right' buttons.
    This is because lambda-term will search in a line down from the centre of top,
    through the 'glue' and hit exit.

    We can fix this two ways.  In the "set" mode when 'top' is focussed and down is 
    pressed we jump to 'left'.  In glue mode when we search down though the glue 
    widget it points to the 'right' button and we jump there. *)
  begin
    match mode with
    | "set" -> top#set_focus ~down:(left :> t) ()
    | "glue" -> glue#set_focus ~down:(right :> t) ()
    | _ -> ()
  end;

  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let () = Lwt_main.run (main ())



