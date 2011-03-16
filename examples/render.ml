(*
 * render.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt
open Lt_geom
open Lt_draw
open Lt_key

let render term old_matrix size coord =
  let m = make_matrix size in
  let ctx = context m size in
  draw_frame ctx { r_line = 0; r_column = 0; r_lines = size.lines; r_columns = size.columns };
  m.(coord.line).(coord.column).char <- UChar.of_char 'O';
  lwt () = Lt_term.render_update term old_matrix m in
  return m

let rec loop term coord size matrix =
  Lt_term.read_event term >>= function
    | Lt_event.Resize size ->
        let coord = {
          line = if coord.line >= size.lines - 1 then size.lines - 2 else coord.line;
          column = if coord.column >= size.columns - 1 then size.columns - 2 else coord.column;
        } in
        lwt matrix = render term [||] size coord in
        loop term coord size matrix
    | Lt_event.Key{ code = Escape } ->
        return ()
    | Lt_event.Key{ code = Up } ->
        let coord = { coord with line = if coord.line > 1 then coord.line - 1 else coord.line } in
        lwt matrix = render term matrix size coord in
        loop term coord size matrix
    | Lt_event.Key{ code = Down } ->
        let coord = { coord with line = if coord.line < size.lines - 2 then coord.line + 1 else coord.line } in
        lwt matrix = render term matrix size coord in
        loop term coord size matrix
    | Lt_event.Key{ code = Left } ->
        let coord = { coord with column = if coord.column > 1 then coord.column - 1 else coord.column } in
        lwt matrix = render term matrix size coord in
        loop term coord size matrix
    | Lt_event.Key{ code = Right } ->
        let coord = { coord with column = if coord.column < size.columns - 2 then coord.column + 1 else coord.column } in
        lwt matrix = render term matrix size coord in
        loop term coord size matrix
    | _ ->
        loop term coord size matrix

lwt () =
  lwt term = Lazy.force Lt_term.stdout in
  lwt () = Lt_term.save_state term in
  lwt () = Lt_term.hide_cursor term in
  lwt mode = Lt_term.enter_raw_mode term in
  try_lwt
    lwt size = Lt_term.get_size term in
    let coord = { line = size.lines / 2; column = size.columns / 2 } in
    lwt matrix = render term [||] size coord in
    loop term coord size matrix
  finally
    lwt () = Lt_term.leave_raw_mode term mode in
    lwt () = Lt_term.show_cursor term in
    Lt_term.load_state term
