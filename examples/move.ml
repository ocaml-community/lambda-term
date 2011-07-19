(*
 * move.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open Lwt_react
open LTerm_geom
open LTerm_text
open LTerm_key

let rec loop ui coord =
  LTerm_ui.loop ui >>= function
    | LTerm_event.Key{ code = Up } ->
        coord := { !coord with row = !coord.row - 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Down } ->
        coord := { !coord with row = !coord.row + 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Left } ->
        coord := { !coord with col = !coord.col - 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Right } ->
        coord := { !coord with col = !coord.col + 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Escape } ->
        return ()
    | ev ->
        loop ui coord

let draw ui matrix coord =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } LTerm_draw.Light;
  if size.rows > 2 && size.cols > 2 then begin
    let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 } in
    LTerm_draw.draw_styled ctx coord.row coord.col (eval [B_fg LTerm_style.lblue; S"Move me"; E_fg])
  end

lwt () =
  lwt term = Lazy.force LTerm.stdout in

  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in

  lwt ui = LTerm_ui.create term (fun matrix size -> draw matrix size !coord) in
  try_lwt
    loop ui coord
  finally
    LTerm_ui.quit ui
