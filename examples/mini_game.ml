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

let n = ref 0

type state =
  { bricks      : bool array
  ; bullets     : int option array (* bullets columns *)
  ; mutable pos : int
  }

let render ctx state =
  incr n;
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
      Draw.set ctx ~row:(1 + y) ~col:x (Uchar.of_char 'o'));
  let len = 7 in
  let row = rows - 1 in
  let pos = state.pos in
  Draw.set ctx ~row ~col:(pos - len/2 - 1) (Uchar.of_char '<');
  Draw.set ctx ~row ~col:(pos + len/2 + 1) (Uchar.of_char '>');
  Draw.draw_hline ctx ~row ~col:(pos - len/2) ~len Light;
  Draw.draw_piece ctx ~row ~col:pos
    (Draw.Piece.make ~top:Light ~bottom:Blank ~left:Light ~right:Light);
  Draw.UTF8.draw ctx ~row:2 ~col:2 (string_of_int !n);
  None
;;

type Event.User.t += Timeout

let send_timeouts app =
  while true do
    Thread.delay 0.3;
    Full_screen.send_event app (User Timeout)
  done
;;

let () =
  let app = Full_screen.create render ~mouse_events:Any in
  let size = Full_screen.size app in
  let t =
    { bricks  = Array.make size.cols true
    ; bullets = Array.make (max 0 (size.rows - 2)) None
    ; pos     = size.cols / 2
    }
  in
  ignore (Thread.create send_timeouts app : Thread.t);
  let _ : state =
    Full_screen.run_sync app ~init:t ~f:(fun t ev ->
      match ev with
      | User Timeout ->
        let len = Array.length t.bullets in
        if len > 0 then begin
          (match t.bullets.(0) with
           | None -> ()
           | Some n -> t.bricks.(n) <- false);
          Array.blit ~src:t.bullets ~dst:t.bullets ~src_pos:1 ~dst_pos:0 ~len:(len - 1);
          t.bullets.(len - 1) <- None;
        end;
        Full_screen.refresh app;
        t
      | Button_down (N, 1, { col; _ }) ->
        t.pos <- col;
        let len = Array.length t.bullets in
        if len > 0 then
          t.bullets.(len - 1) <- Some col;
        Full_screen.refresh app;
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
