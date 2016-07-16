(*
 * move.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lambda_term

let draw ctx (pos : Geom.coord) =
  let size = Draw.size ctx in
  Draw.clear ctx;
  Draw.draw_frame ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } Light;
  if size.rows > 2 && size.cols > 2 then begin
    let ctx = Draw.sub ctx { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 } in
    Draw.Text.draw ctx ~row:pos.row ~col:pos.col
      (Text.tprintf "@{<lblue>Move me@}")
  end;
  None

let () =
  let app = LTerm_full_screen.create draw in
  let refresh () = LTerm_full_screen.refresh app in
  let _ : Geom.coord =
    LTerm_full_screen.run_sync app ~init:{ Geom. row = 0; col = 0 } ~f:(fun pos ev ->
      match ev with
      | Key (N, Up   ) -> refresh (); { pos with row = pos.row - 1 }
      | Key (N, Down ) -> refresh (); { pos with row = pos.row + 1 }
      | Key (N, Left ) -> refresh (); { pos with col = pos.col - 1 }
      | Key (N, Right) -> refresh (); { pos with col = pos.col + 1 }
      | Key (N, Escape) -> LTerm_full_screen.quit app; pos
      | _ -> pos
    )
  in
  ()
