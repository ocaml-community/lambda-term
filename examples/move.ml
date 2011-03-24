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

class widget =
  let coord, set_coord = S.create { line = 0; column = 0 } in
object(self)
  inherit Lt_widget.t as super

  val need_redraw = E.stamp (S.changes coord) ()
  method need_redraw = need_redraw

  method draw ctx focused =
    let coord = S.value coord in
    Lt_draw.draw_styled ctx coord.line coord.column (eval [B_fg Lt_style.lblue; S"Move me"; E_fg]);
    None

  method handle_event ev =
    super#handle_event ev;
    let coord = S.value coord and size = S.value self#size in
    match ev with
      | Lt_event.Resize size ->
          set_coord {
            line = if coord.line >= size.lines && size.lines > 0 then size.lines - 1 else coord.line;
            column = if coord.column >= size.columns && size.columns > 0 then size.columns - 1 else coord.column;
          }
      | Lt_event.Key{ code = Up } ->
          set_coord { coord with line = if coord.line > 0 then coord.line - 1 else coord.line }
      | Lt_event.Key{ code = Down } ->
          set_coord { coord with line = if coord.line < size.lines - 1 then coord.line + 1 else coord.line }
      | Lt_event.Key{ code = Left } ->
          set_coord { coord with column = if coord.column > 0 then coord.column - 1 else coord.column }
      | Lt_event.Key{ code = Right } ->
          set_coord { coord with column = if coord.column < size.columns - 1 then coord.column + 1 else coord.column }
      | _ ->
          ()

  initializer
    self#set_can_focus (S.const true)
end

lwt () =
  lwt term = Lazy.force Lt_term.stdout in
  let widget = new widget in
  Lt_widget.run
    term
    (Lt_widget.frame widget)
    (E.next (E.fmap (function { code = Escape } -> Some () | _ -> None) widget#key_pressed))
