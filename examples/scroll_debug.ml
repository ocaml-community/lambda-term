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

class scroll_label scroll = object
  inherit label "scroll"
  method! can_focus = false
  method! size_request = { rows=1; cols=0 }
  val style = LTerm_style.{none with foreground = Some red;
                                     background = Some green };
  method! draw ctx _focused =
    LTerm_draw.fill_style ctx style;
    LTerm_draw.draw_string_aligned ctx 0 H_align_center ~style
      (Zed_string.of_utf8 (Printf.sprintf "%i/%i" scroll#offset scroll#range))

end

let main () =
  let waiter, wakener = wait () in
  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let vbox = new vbox in

  let add_scroll (vbox : vbox) ~range ~size =
    let adj = new scrollable in
    let hscroll = new hscrollbar adj in
    let label = new scroll_label adj in
    adj#set_range range;
    adj#set_mouse_mode `middle;
    adj#set_scroll_bar_mode (`fixed size);
    vbox#add ~expand:false (label :> t);
    vbox#add ~expand:false (new hline);
    vbox#add ~expand:false (hscroll :> t);
    vbox#add ~expand:false (new hline);
    adj
  in

  let scrolls = List.map
    (fun range -> add_scroll vbox ~range ~size:1)
    [ 0; 10; 30; 60; 100; 200; 1000 ]
  in

  let mouse_mode =
    let vbox = new vbox in
    let mouse_mode = new radiogroup in
    mouse_mode#on_state_change (function
      | None -> ()
      | Some(m) -> List.iter (fun h -> h#set_mouse_mode m) scrolls);
    vbox#add ~expand:false (new label "mouse mode");
    vbox#add ~expand:false (new radiobutton mouse_mode "middle" `middle);
    vbox#add ~expand:false (new radiobutton mouse_mode "ratio" `ratio);
    vbox#add ~expand:false (new radiobutton mouse_mode "auto" `auto);
    vbox#add ~expand:true (new spacing ());
    vbox
  in

  let scroll_mode =
    let vbox = new vbox in
    let scroll_mode = new radiogroup in

    let ranged_widget group name value range =
      let button = new radiobutton group name value in
      let scroll = new hslider range in
      button, scroll
    in

    vbox#add ~expand:false (new label "scroll mode");
    let f,fr = ranged_widget scroll_mode "fixed " `fixed 10 in
    let d,dr = ranged_widget scroll_mode "dynamic " `dynamic 10 in
    let sbox =
      let in_frame w = let f = new frame in f#set w; f in
      let v1 = new vbox in
      v1#add ~expand:true f;
      v1#add ~expand:true d;
      let v2 = new vbox in
      v2#add ~expand:false (in_frame fr);
      v2#add ~expand:false (in_frame dr);
      let h = new hbox in
      h#add ~expand:false v1;
      h#add ~expand:false v2;
      h
    in
    vbox#add ~expand:false sbox;

    let set_mode f o = List.iter (fun h -> h#set_scroll_bar_mode (f o)) scrolls in
    let fixed o = `fixed ((o*5)+1) in
    let dynamic o = `dynamic (o*50) in

    scroll_mode#on_state_change (function
      | None -> ()
      | Some(`fixed) -> set_mode fixed fr#offset
      | Some(`dynamic) -> set_mode dynamic dr#offset
    );

    fr#on_offset_change (fun o -> if f#state then set_mode fixed o);
    dr#on_offset_change (fun o -> if d#state then set_mode dynamic o);

    vbox
  in

  let hbox = new hbox in
  hbox#add (new spacing ());
  hbox#add ~expand:false mouse_mode;
  hbox#add (new spacing ());
  hbox#add ~expand:false scroll_mode;
  hbox#add (new spacing ());

  vbox#add ~expand:true (new spacing ());
  vbox#add ~expand:false hbox;
  vbox#add ~expand:true (new spacing ());
  vbox#add ~expand:false exit;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
