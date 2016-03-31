(*
 * scroll_debug.ml
 * ----------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open LTerm_widget
open LTerm_geom

class scroll_label scroll = object(self)
  inherit label "scroll"
  method can_focus = false
  method size_request = { rows=1; cols=0 }
  method draw ctx focused = 
    let alloc = size_of_rect scroll#allocation in
    LTerm_draw.draw_string ctx 0 0 
      (Printf.sprintf "%i/%i | %ix%i | %i/%i/%i" 
        scroll#offset scroll#range
        alloc.rows alloc.cols
        scroll#debug_offset scroll#debug_size scroll#debug_steps
      )

end

let main () = 
  let waiter, wakener = wait () in
  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let vbox = new vbox in

  let add_scroll (vbox : vbox) ~range ~size = 
    let hscroll = new hscrollbar ~default_scroll_bar_size:size () in
    let label = new scroll_label hscroll in
    hscroll#set_range range;
    vbox#add ~expand:false (label :> t);
    vbox#add ~expand:false (new hline);
    vbox#add ~expand:false (hscroll :> t);
    vbox#add ~expand:false (new hline);
    hscroll
  in

  let scrolls = List.map (fun (range,size) -> add_scroll vbox ~range ~size)
    [ 10,10;  100, 10;  1000, 10;
      10,100; 100, 100; 1000, 100; ]
  in

  vbox#add ~expand:true (new spacing ());
  let mouse_mode = new radiogroup in
  mouse_mode#on_state_change (function
    | None -> () 
    | Some(m) -> List.iter (fun h -> h#set_mouse_mode m) scrolls);
  vbox#add ~expand:false (new radiobutton mouse_mode "ratio" `ratio);
  vbox#add ~expand:false (new radiobutton mouse_mode "middle" `middle);
  vbox#add ~expand:false (new radiobutton mouse_mode "left" `left);
  vbox#add ~expand:false (new radiobutton mouse_mode "right" `right);
  vbox#add ~expand:false exit;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

