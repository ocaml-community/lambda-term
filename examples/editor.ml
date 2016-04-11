(*
 * editor.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt

let main () =
  let waiter, wakener = wait () in

  let hbox = new LTerm_widget.hbox in
  let frame = new LTerm_widget.frame in
  let editor = new LTerm_edit.edit () in
  let vscroll = new LTerm_widget.vscrollbar editor#vscroll in
  frame#set editor;
  hbox#add frame;
  hbox#add vscroll;

  (* Exit when the user presses C-x C-c *)
  editor#bind
    (let open LTerm_key in
     [ { control = true; meta = false; shift = false
       ; code = Char (UChar.of_char 'x') }
     ; { control = true; meta = false; shift = false
       ; code = Char (UChar.of_char 'c') }
     ])
    [ LTerm_edit.Custom (fun () -> wakeup wakener ()) ];

  let txt = String.concat "\n" @@ Array.to_list @@ 
    Array.init 150 (fun i -> Printf.sprintf "%.9i" i) in
  Zed_edit.insert editor#context
(*    (Zed_rope.of_string "\
This is a simple edition widget.

Type C-x C-c to exit.

");*)
    (Zed_rope.of_string txt);

  let show f = 
    let open LTerm_geom in
    let open Printf in
    let engine = editor#engine in
    let line_set = Zed_edit.lines engine in
    let start_line = Zed_lines.line_index line_set editor#start in
    let cursor_offset = Zed_cursor.get_position editor#cursor in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    ksprintf f "page=%i doc=%i offset=%i range=%i \
                start=%i shift=%i start_line=%i/%i offsets=%i \
                cursor=%i/%i delta=%i"
      editor#vscroll#page_size
      editor#vscroll#document_size
      editor#vscroll#offset
      editor#vscroll#range
      editor#start editor#shift 
      editor#start_line start_line
      editor#offset_count
      cursor_offset cursor_line
      editor#delta
  in

  let vbox = new LTerm_widget.vbox in
  let debug = new LTerm_widget.label "_" in
  vbox#add hbox;
  vbox#add debug;
  ignore (Lwt_engine.on_timer 0.1 true (fun _ -> show debug#set_text));

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
