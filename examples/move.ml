(*
 * move.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt
open Lwt_react
open Lt_geom
open Lt_text
open Lt_key

lwt () =
  let waiter, wakener = wait () in
  let coord, set_coord = S.create { line = 0; column = 0 } in

  let draw ctx focused =
    let coord = S.value coord in
    Lt_draw.draw_styled ctx coord.line coord.column (eval [B_fg Lt_style.lblue; S"Move me"; E_fg]);
    None
  in

  let on_event ev =
    let coord = S.value coord in
    match ev with
      | Lt_event.Key{ code = Up } ->
          set_coord { coord with line = coord.line - 1 }
      | Lt_event.Key{ code = Down } ->
          set_coord { coord with line = coord.line + 1 }
      | Lt_event.Key{ code = Left } ->
          set_coord { coord with column = coord.column - 1 }
      | Lt_event.Key{ code = Right } ->
          set_coord { coord with column = coord.column + 1 }
      | Lt_event.Key{ code = Escape } ->
          wakeup wakener ()
      | _ ->
          ()
  in

  let widget = Lt_widget.make ~can_focus:true ~need_redraw:(E.stamp (S.changes coord) ()) ~draw ~on_event () in

  lwt term = Lazy.force Lt_term.stdout in
  Lt_widget.run term (Lt_widget.frame widget) waiter
