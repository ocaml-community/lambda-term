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
open Lt_geom
open Lt_text
open Lt_key

let rec loop ui coord =
  Lt_ui.loop ui >>= function
    | Lt_event.Key{ code = Up } ->
        coord := { !coord with line = !coord.line - 1 };
        Lt_ui.draw ui;
        loop ui coord
    | Lt_event.Key{ code = Down } ->
        coord := { !coord with line = !coord.line + 1 };
        Lt_ui.draw ui;
        loop ui coord
    | Lt_event.Key{ code = Left } ->
        coord := { !coord with column = !coord.column - 1 };
        Lt_ui.draw ui;
        loop ui coord
    | Lt_event.Key{ code = Right } ->
        coord := { !coord with column = !coord.column + 1 };
        Lt_ui.draw ui;
        loop ui coord
    | Lt_event.Key{ code = Escape } ->
        return ()
    | ev ->
        loop ui coord

let draw ui matrix coord =
  let size = Lt_ui.size ui in
  let ctx = Lt_draw.context matrix size in
  Lt_draw.clear ctx;
  Lt_draw.draw_frame ctx { r_line = 0; r_column = 0; r_lines = size.lines; r_columns = size.columns } Lt_draw.Light;
  if size.lines > 2 && size.columns > 2 then begin
    let ctx = Lt_draw.sub ctx { r_line = 1; r_column = 1; r_lines = size.lines - 2; r_columns = size.columns - 2 } in
    Lt_draw.draw_styled ctx coord.line coord.column (eval [B_fg Lt_style.lblue; S"Move me"; E_fg])
  end

lwt () =
  lwt term = Lazy.force Lt_term.stdout in

  (* Coordinates of the message. *)
  let coord = ref { line = 0; column = 0 } in

  lwt ui = Lt_ui.create term (fun matrix size -> draw matrix size !coord) in
  try_lwt
    loop ui coord
  finally
    Lt_ui.quit ui
