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
class scrollable_nums (scroll : scrollable) = object
  inherit t "nums"

  initializer scroll#set_range 197

  method! can_focus = false

  method! draw ctx _focused = 
    let { rows; _ } = LTerm_draw.size ctx in

    for row=0 to rows-1 do
      LTerm_draw.draw_string ctx row 0 (Zed_string.of_utf8 (string_of_int (row + scroll#offset)))
    done

end

let main () = 
  let waiter, wakener = wait () in

  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let adj = new scrollable in
  let scroll = new vscrollbar adj in
  let nums = new scrollable_nums adj in

  let hbox = new hbox in
  hbox#add ~expand:true nums;
  hbox#add ~expand:false scroll;

  (* buttons to set scroll offset *)
  let prev = new button "prev" in
  prev#on_click (fun () -> adj#set_offset (adj#offset-1));
  let next = new button "next" in
  next#on_click (fun () -> adj#set_offset (adj#offset+1));
  let decr = new button "decr" in
  decr#on_click (fun () -> adj#set_offset adj#decr);
  let incr = new button "incr" in
  incr#on_click (fun () -> adj#set_offset adj#incr);

  adj#on_offset_change (fun _ -> scroll#queue_draw);

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

