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
  let event, set_can_focus = E.create () in
  let can_focus = S.switch (S.const false) event in
object(self)
  method as_widget = (self :> t)
  method children : t list signal = S.const []
  method can_focus = can_focus
  method set_can_focus = set_can_focus
  method need_redraw : unit event = E.never
  method draw (ctx : Lt_draw.context) (focused : t) : coord option = None
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

class label text =
  let event, set_text = E.create () in
  let text = S.switch text event in
object
  inherit t

  method text = text
  method set_text = set_text

  val need_redraw = E.stamp (S.changes text) ()
  method need_redraw = need_redraw

  method draw ctx focused =
    let { lines; columns } = Lt_draw.size ctx in
    let text = S.value text in
    Lt_draw.draw_string ctx (lines / 2) ((columns - Zed_utf8.length text) / 2) text;
    None
end

class box (children : t list signal) =
  let event, set_children = E.create () in
  let children = S.switch children event in
object
  inherit t

  method children = children
  method set_children = set_children

  val need_redraw =
    let signal =
      S.map ~eq:(==)
        (fun l -> E.select (List.map (fun child -> child#need_redraw) l))
        children
    in
    E.switch (S.value signal) (S.changes signal)
  method need_redraw = need_redraw
end

let choose_cursor c1 c2 =
  match c1, c2 with
    | Some _, _ -> c1
    | _, Some _ -> c2
    | None, None -> None

class hbox children =
object(self)
  inherit box children

  method draw ctx focused =
    let { lines; columns } = Lt_draw.size ctx in
    let children = S.value self#children in
    let n = List.length children in
    let widthf = float columns /. float n in
    let rec loop columnf children cursor =
      match children with
        | [] ->
            None
        | [child] ->
            let column = truncate columnf in
            let cursor' =
              child#draw (Lt_draw.sub ctx {
                            r_line = 0;
                            r_column = column;
                            r_lines = lines;
                            r_columns = columns - column;
                          }) focused
            in
            choose_cursor cursor cursor'
        | child :: rest ->
            let column = truncate columnf in
            let width = truncate (columnf +. widthf) - column in
            let cursor' =
              child#draw (Lt_draw.sub ctx {
                            r_line = 0;
                            r_column = column;
                            r_lines = lines;
                            r_columns = width;
                          }) focused
            in
            loop (columnf +. widthf) rest (choose_cursor cursor cursor')
    in
    loop 0.0 children None
end

class vbox children =
object(self)
  inherit box children

  method draw ctx focused =
    let { lines; columns } = Lt_draw.size ctx in
    let children = S.value self#children in
    let n = List.length children in
    let heightf = float lines /. float n in
    let rec loop linef children cursor =
      match children with
        | [] ->
            None
        | [child] ->
            let line = truncate linef in
            let cursor' =
              child#draw (Lt_draw.sub ctx {
                            r_line = line;
                            r_column = 0;
                            r_lines = lines - line;
                            r_columns = columns;
                          }) focused
            in
            choose_cursor cursor cursor'
        | child :: rest ->
            let line = truncate linef in
            let height = truncate (linef +. heightf) - line in
            let cursor' =
              child#draw (Lt_draw.sub ctx {
                            r_line = line;
                            r_column = 0;
                            r_lines = height;
                            r_columns = columns;
                          }) focused
            in
            loop (linef +. heightf) rest (choose_cursor cursor cursor')
    in
    loop 0.0 children None
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

  (* The currently focused widget. *)
  let focused = ref (widget :> t) in

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

      (* Create the context for drawing. *)
      let ctx = Lt_draw.context !matrix_a !size in

      (* Clear the context. *)
      clear ctx;

      (* Draw the widget. *)
      let cursor_position = widget#draw ctx !focused in

      (* Rendering. *)
      lwt () = Lt_term.hide_cursor term in
      lwt () = Lt_term.render_update term !matrix_b !matrix_a in
      lwt () =
        match cursor_position with
          | Some coord ->
              lwt () = Lt_term.goto term coord in
              Lt_term.show_cursor term
          | None ->
              return ()
      in
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
