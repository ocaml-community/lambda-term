(*
 * move.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key

let rec loop ui coord =
  LTerm_ui.wait ui >>= function
    | LTerm_event.Key{ code = Up; _ } ->
        coord := { !coord with row = !coord.row - 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Down; _ } ->
        coord := { !coord with row = !coord.row + 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Left; _ } ->
        coord := { !coord with col = !coord.col - 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Right; _ } ->
        coord := { !coord with col = !coord.col + 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Escape; _ } ->
        return ()
    | _ ->
        loop ui coord

let draw ui matrix coord =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame_labelled ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } ~alignment:H_align_center (Zed_string.of_utf8 "Use arrow keys to move text 文字") LTerm_draw.Light;
  if size.rows > 2 && size.cols > 2 then begin
    let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 } in
    LTerm_draw.draw_styled ctx coord.row coord.col (eval [B_fg LTerm_style.lblue; S "Move m̀é 囧"; E_fg])
  end

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->

  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in

  LTerm_ui.create term (fun matrix size -> draw matrix size !coord)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui coord) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
