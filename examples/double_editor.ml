(*
 * double_editor.ml
 * ----------
 * Copyright : (c) 2016, Fabian Bonk <fabian.bonk@bonkii.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)
open LTerm_geom

let ( >>= ) = Lwt.( >>= )

(* helper functions *)
let make_key ?(ctrl = false) ?(meta = false) ?(shift = false) c =
  let code =
    match c with
    | `Char c -> LTerm_key.Char (Uchar.of_char c)
    | `Other key -> key in
  { LTerm_key.control = ctrl; meta; shift; code }

let frame widget =
  let frame = new LTerm_widget.frame in
  frame#set widget;
  frame

let main () =
  let waiter, wakener = Lwt.wait () in

  let ctrl_c = [make_key ~ctrl:true @@ `Char 'c'] in
  let tab = [make_key @@ `Other LTerm_key.Tab] in
  let quit = [LTerm_edit.Custom (Lwt.wakeup wakener)] in

  let vbox = new LTerm_widget.vbox in

  let top_editor = new LTerm_edit.edit () in
  let top_frame = frame top_editor in

  (* make bottom editor a fixed 10 rows in size *)
  let bottom_editor = new LTerm_edit.edit ~size:{ rows = 10; cols = 1 } () in
  (* changed my mind: make it 5 rows smaller *)
  bottom_editor#set_allocation
    { bottom_editor#allocation with row1 = bottom_editor#allocation.row1 - 5 };
  let bottom_frame = frame bottom_editor in

  vbox#add top_frame;
  (* in versions before PR#42 this would either crash or make the bottom editor unusable *)
  vbox#add ~expand:false bottom_frame;

  (* exit on C-c *)
  top_editor#bind ctrl_c quit;
  bottom_editor#bind ctrl_c quit;

  let send_key key =
    LTerm_edit.Custom (fun () -> vbox#send_event @@ LTerm_event.Key (make_key key)) in

  (* switch editors on Tab *)
  top_editor#bind tab [send_key @@ `Other LTerm_key.Down];
  bottom_editor#bind tab [send_key @@ `Other LTerm_key.Up];

  let label = new LTerm_widget.label "Press Tab to switch between editors.\nPress C-c to exit." in
  vbox#add ~expand:false label;

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term ~save_state:false ~load_resources:false vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
