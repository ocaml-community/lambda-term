(*
 * mini_game.ml
 * ------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 *)

open StdLabels
open Lambda_term

type state =
  { bricks         : bool array
  ; bullets        : int option array (* bullets columns *)
  ; mutable pos    : int
  ; mutable firing : bool
  }

let render ctx state =
  Draw.clear ctx;
  let { Geom. cols; rows } = Draw.size ctx in
  Array.iteri state.bricks ~f:(fun i present ->
    Draw.set ctx ~row:0 ~col:i
      (if present then
         Uchar.of_int 0x2588
       else
         Uchar.of_char ' '));
  Array.iteri state.bullets ~f:(fun y -> function
    | None -> ()
    | Some x ->
      Draw.set ctx ~row:y ~col:x (Uchar.of_char 'o'));
  let len = 7 in
  let row = rows - 1 in
  let pos = state.pos in
  Draw.set ctx ~row ~col:(pos - len/2 - 1) (Uchar.of_char '<');
  Draw.set ctx ~row ~col:(pos + len/2 + 1) (Uchar.of_char '>');
  Draw.draw_hline ctx ~row ~col:(pos - len/2) ~len Light;
  Draw.draw_piece ctx ~row ~col:pos
    (Draw.Piece.make ~top:Light ~bottom:Blank ~left:Light ~right:Light);
  None
;;

type Event.User.t += Timeout

let send_timeouts app =
  while true do
    Thread.delay 0.05;
    Full_screen.send_event app (User Timeout)
  done
;;

let () =
  let app = Full_screen.create render ~mouse_events:Any in
  let size = Full_screen.size app in
  let t =
    { bricks  = Array.make size.cols true
    ; bullets = Array.make (max 0 (size.rows - 1)) None
    ; pos     = size.cols / 2
    ; firing  = false
    }
  in
  let fire t =
    let len = Array.length t.bullets in
    if len > 0 then t.bullets.(len - 1) <- Some t.pos;
    Full_screen.refresh app;
  in
  ignore (Thread.create send_timeouts app : Thread.t);
  let _ : state =
    Full_screen.run_sync app ~init:t ~f:(fun t ev ->
      match ev with
      | User Timeout ->
        let len = Array.length t.bullets in
        if len > 0 then begin
          Array.blit ~src:t.bullets ~dst:t.bullets ~src_pos:1 ~dst_pos:0 ~len:(len - 1);
          (match t.bullets.(0) with
           | None -> ()
           | Some n -> if n >= 0 && n < Array.length t.bricks then t.bricks.(n) <- false);
          t.bullets.(len - 1) <- None;
        end;
        if t.firing then fire t else Full_screen.refresh app;
        t
      | Button_down (N, 1, { col; _ }) ->
        t.pos <- col;
        t.firing <- true;
        fire t;
        t
      | Button_up (N, 1, { col; _ }) ->
        t.pos <- col;
        t.firing <- false;
        t
      | Mouse_motion (N, { col; _ }) | Button_motion (N, _, { col; _ }) ->
        t.pos <- col;
        Full_screen.refresh app;
        t
      | Key (N, Escape) ->
        Full_screen.quit app;
        t
      | _ -> t)
  in
  ()
