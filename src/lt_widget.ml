(*
 * lt_widget.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open React
open Lwt
open Lt_types
open Lt_draw

(* +-----------------------------------------------------------------+
   | Definitions                                                     |
   +-----------------------------------------------------------------+ *)

class t =
  let key_pressed, send_key_pressed = E.create () in
object
  method need_redraw : unit event = E.never
  method draw (ctx : Lt_draw.context) = ()
  method key_pressed = key_pressed

  method handle_event = function
    | Lt_event.Key key ->
        send_key_pressed key
    | ev ->
        ()
end

(* +-----------------------------------------------------------------+
   | Simple widgets                                                  |
   +-----------------------------------------------------------------+ *)

class label text = object
  inherit t

  val need_redraw = E.stamp (S.changes text) ()
  method need_redraw = need_redraw

  method draw ctx =
    let { lines; columns } = Lt_draw.size ctx in
    let text = S.value text in
    Lt_draw.draw_string ctx (lines / 2) ((columns - Zed_utf8.length text) / 2) text
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

type 'a event =
  | Value of 'a
  | Event of Lt_event.t

let run term ?(save_state=true) widget waiter =
  (* The two matrices used for the rendering. *)
  let matrix_a = ref [||] and matrix_b = ref [||] in

  (* Whether a draw operation has already been requested. *)
  let draw_queued = ref false in

  (* The current size of the terminal. *)
  lwt size = Lt_term.get_size term in
  let size = ref size in

  (* Draw the screen. *)
  let draw () =
    if !draw_queued then
      (* If a draw operation is already queued, do nothing. *)
      return ()
    else begin
      draw_queued := true;

      (* Wait a bit in order not to redraw too often. *)
      lwt () = pause () in
      draw_queued := false;

      (* Allocate the first matrix if needed. *)
      if !matrix_a = [||] then matrix_a := Lt_draw.make_matrix !size;

      (* Draw the widget. *)
      widget#draw (Lt_draw.context !matrix_a !size);

      (* Rendering. *)
      lwt () = Lt_term.render_update term !matrix_b !matrix_a in
      lwt () = Lt_term.flush term in

      (* Swap the two matrices. *)
      let a = !matrix_a and b = !matrix_b in
      matrix_a := b;
      matrix_b := a;

      return ()
    end
  in

  (* Loop handling events. *)
  let waiter = waiter >|= fun x -> Value x in
  let rec loop () =
    let thread = Lt_term.read_event term >|= fun x -> Event x in
    choose [thread; waiter] >>= function
      | Event(Lt_event.Resize new_size) ->
          (* New size, discard current matrices. *)
          matrix_a := [||];
          matrix_b := [||];
          size := new_size;
          (* Redraw with the new size. *)
          lwt () = draw () in
          loop ()
      | Event ev ->
          widget#handle_event ev;
          loop ()
      | Value value ->
          cancel thread;
          return value
  in

  (* Save the state if requested. *)
  lwt () =
    if save_state then
      Lt_term.save_state term
    else
      return ()
  in

  (* Redraw the screen when the widget needs it. *)
  let id = Lwt_event.notify_s draw widget#need_redraw in

  try_lwt
    (* Initial drawing. *)
    lwt () = draw () in
    (* Loop forever. *)
    Lt_term.with_raw_mode term loop
  finally
    (* Disable redrawing if something went wrong. *)
    Lwt_event.disable id;
    (* Restore the state of the terminal if previously saved. *)
    if save_state then
      Lt_term.load_state term
    else
      return ()
