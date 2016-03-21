(*
 * scroll.ml
 * ----------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open Lwt
open LTerm_widget
open LTerm_geom

class scrollable_nums = object(self)
  inherit t "nums"

  method can_focus = false

  method full_size = { rows = 199; cols=40 }

  val mutable offset = { row = 0; col = 0 }
  method offset = offset
  method set_offset o = offset <- o; self#queue_draw

  method draw ctx focused = 
    let { rows; cols } = LTerm_draw.size ctx in

    for row=0 to rows-1 do
      LTerm_draw.draw_string ctx row 0 (string_of_int (row + offset.row))
    done

end

let main () = 
  let waiter, wakener = wait () in

  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let nums = new scrollable_nums in
  let scroll = new vscrollbar (nums :> scrollable) in
  let hbox = new hbox in
  hbox#add ~expand:true nums;
  hbox#add ~expand:false scroll;

  let decr = new button "decr" in
  let set_offset f = 
    let o = nums#offset in 
    nums#set_offset { o with row = f o.row } 
  in
  decr#on_click (fun () -> set_offset ((+)(-1)));
  let incr = new button "incr" in
  incr#on_click (fun () -> set_offset ((+)1));

  let vbox = new vbox in
  vbox#add hbox;
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false decr;
  vbox#add ~expand:false incr;
  vbox#add ~expand:false exit;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

