(*
 * lt_widget.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open Lwt
open Lt_geom
open Lt_draw
open Lt_key
open Lt_style
open Lt_text

(* +-----------------------------------------------------------------+
   | Definitions                                                     |
   +-----------------------------------------------------------------+ *)

class _t () =
  let key_pressed, send_key_pressed = E.create () in
  let event, set_can_focus = E.create () in
  let can_focus = S.switch (S.const false) event in
object(self)
  method as_widget = (self :> _t)
  method children : _t list signal = S.const []
  method can_focus = can_focus
  method set_can_focus = set_can_focus
  method need_redraw : unit event = E.never
  method draw (ctx : Lt_draw.context) (focused : _t) : coord option = None
  method key_pressed = key_pressed

  method handle_event = function
    | Lt_event.Key key ->
        send_key_pressed key
    | ev ->
        ()
end

class t = object inherit _t () end

(* +-----------------------------------------------------------------+
   | Labels                                                          |
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

(* +-----------------------------------------------------------------+
   | Boxes                                                           |
   +-----------------------------------------------------------------+ *)

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
   | Buttons                                                         |
   +-----------------------------------------------------------------+ *)

class button text =
  let event, set_text = E.create () in
  let text = S.switch text event in
  let clicked, send_clicked = E.create () in
object(self)
  inherit t as super

  method text = text
  method set_text = set_text

  method clicked = clicked

  method handle_event ev =
    super#handle_event ev;
    match ev with
      | Lt_event.Key { control = false; meta = false; shift = false; code = Enter } ->
          send_clicked ()
      | _ ->
          ()

  val need_redraw = E.stamp (S.changes text) ()
  method need_redraw = need_redraw

  method draw ctx focused =
    let { lines; columns } = Lt_draw.size ctx in
    let text = S.value text in
    let len = Zed_utf8.length text in
    if focused = (self :> t) then
      Lt_draw.draw_styled ctx (lines / 2) ((columns - len - 4) / 2)
        (eval [B_bold true; B_fg white; B_bg blue; S"< "; S text; S" >"])
    else
      Lt_draw.draw_styled ctx (lines / 2) ((columns - len - 4) / 2)
        (eval [S"< "; S text; S" >"]);
    None

  initializer
    self#set_can_focus (S.const true)
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let rec find_focusable widget =
  if S.value widget#can_focus then
    Some widget
  else
    find_focusable_in_list (S.value widget#children)

and find_focusable_in_list = function
  | [] ->
      None
  | child :: rest ->
      match find_focusable child with
        | Some _ as some -> some
        | None -> find_focusable_in_list rest

type search_result =
  | Sr_some of t
  | Sr_last
  | Sr_none

let rec next_focusable widget current =
  if widget == current then
    Sr_last
  else
    next_focusable_in_list (S.value widget#children) current

and next_focusable_in_list widgets current =
  match widgets with
    | [] ->
        Sr_none
    | child :: rest ->
        match next_focusable child current with
          | Sr_some _ as result ->
              result
          | Sr_none ->
              next_focusable_in_list rest current
          | Sr_last ->
              match find_focusable_in_list rest with
                | Some widget -> Sr_some widget
                | None -> Sr_last

  (* An event for the main loop. *)
type 'a event =
  | Value of 'a
      (* A value from the waiter thread. *)
  | Event of Lt_event.t
      (* A event from the terminal. *)

let run term ?(save_state=true) widget waiter =
  let widget = (widget :> t) in

  (* The two matrices used for the rendering. *)
  let matrix_a = ref [||] and matrix_b = ref [||] in

  (* Whether a draw operation has already been requested. *)
  let draw_queued = ref false in

  (* The current size of the terminal. *)
  lwt size = Lt_term.get_size term in
  let size = ref size in

  (* The currently focused widget. *)
  let focused =
    ref(match find_focusable widget with
          | Some widget -> widget
          | None -> widget)
  in

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
      | Event(Lt_event.Key { control = false; meta = false; shift = false; code = Tab }) -> begin
          (* Cycle focus. *)
          focused :=
            (match next_focusable widget !focused with
               | Sr_some widget ->
                   widget
               | Sr_none ->
                   widget
               | Sr_last ->
                   match find_focusable widget with
                     | Some widget -> widget
                     | None -> widget);
          lwt () = draw () in
          loop ()
        end
      | Event ev ->
          !focused#handle_event ev;
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

  (* Put the terminal in raw mode. *)
  lwt mode = Lt_term.enter_raw_mode term in

  (* Redraw the screen when the widget needs it. *)
  let ev_redraw = E.map_s draw widget#need_redraw in

  try_lwt
    (* Initial drawing. *)
    lwt () = draw () in
    (* Loop forever. *)
    loop ()
  finally
    (* Disable redrawing. *)
    E.stop ev_redraw;
    (* Restore the terminal mode. *)
    lwt () = Lt_term.leave_raw_mode term mode in
    (* Restore the state of the terminal if previously saved. *)
    if save_state then
      Lt_term.load_state term
    else
      return ()
