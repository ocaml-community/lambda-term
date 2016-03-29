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

(* a simple widget with scrollbar support *)
class scrollable_nums scroll = object(self)
  inherit t "nums" as super

  initializer scroll#set_range 197

  method can_focus = false

  method draw ctx focused = 
    let { rows; cols } = LTerm_draw.size ctx in

    for row=0 to rows-1 do
      LTerm_draw.draw_string ctx row 0 (string_of_int (row + scroll#offset))
    done

end

let main () = 
  let waiter, wakener = wait () in

  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let scroll = new vscrollbar () in
  let nums = new scrollable_nums scroll in

  let hbox = new hbox in
  hbox#add ~expand:true nums;
  hbox#add ~expand:false scroll;

  (* buttons to set scroll offset *)
  let prev = new button "prev" in
  prev#on_click (fun () -> scroll#set_offset (scroll#offset-1));
  let next = new button "next" in
  next#on_click (fun () -> scroll#set_offset (scroll#offset+1));
  let decr = new button "decr" in
  decr#on_click (fun () -> scroll#decr);
  let incr = new button "incr" in
  incr#on_click (fun () -> scroll#incr);

  let vbox = new vbox in
  vbox#add hbox;
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false prev;
  vbox#add ~expand:false next;
  vbox#add ~expand:false decr;
  vbox#add ~expand:false incr;
  vbox#add ~expand:false exit;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

