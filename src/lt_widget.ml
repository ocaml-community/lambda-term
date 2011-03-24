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
  let event, set_expand_horz = E.create () in
  let size, set_size = S.create { lines = 0; columns = 0 } in
  let expand_horz = S.switch (S.const false) event in
  let event, set_expand_vert = E.create () in
  let expand_vert = S.switch (S.const false) event in
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
    | Lt_event.Resize size ->
        set_size size
    | ev ->
        ()

  method size = size

  method requested_size = S.const { lines = 0; columns = 0 }

  method expand_horz = expand_horz
  method set_expand_horz = set_expand_horz
  method expand_vert = expand_vert
  method set_expand_vert = set_expand_vert
end

class t = object inherit _t () end

(* +-----------------------------------------------------------------+
   | Labels                                                          |
   +-----------------------------------------------------------------+ *)

let newline = UChar.of_char '\n'

let text_size str =
  let rec loop ofs lines columns max_columns =
    if ofs = String.length str then
      { lines; columns = max columns max_columns }
    else
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      if chr = newline then
        if ofs = String.length str then
          { lines; columns = max columns max_columns }
        else
          loop ofs (lines + 1) 0 (max columns max_columns)
      else
        loop ofs lines (columns + 1) max_columns
  in
  loop 0 1 0 0

class label ?(horz_align=S.const H_align_center) ?(vert_align=S.const V_align_center) text =
  let event, set_text = E.create () in
  let text = S.switch text event in
  let event, set_horz_align = E.create () in
  let horz_align = S.switch horz_align event in
  let event, set_vert_align = E.create () in
  let vert_align = S.switch vert_align event in
object(self)
  inherit t

  method text = text
  method set_text = set_text

  method horz_align = horz_align
  method set_horz_align = set_horz_align

  method vert_align = vert_align
  method set_vert_align = set_vert_align

  val need_redraw = E.select [
    E.stamp (S.changes text) ();
    E.stamp (S.changes horz_align) ();
    E.stamp (S.changes vert_align) ();
  ]
  method need_redraw = need_redraw

  val requested_size = S.map text_size text
  method requested_size = requested_size

  method draw ctx focused =
    let { lines } = Lt_draw.size ctx in
    let text = S.value text in
    let { lines = height } = S.value requested_size in
    let line =
      match S.value vert_align with
        | V_align_top ->
            0
        | V_align_center ->
            (lines - height) / 2
        | V_align_bottom ->
            lines - height
    in
    Lt_draw.draw_string_aligned ctx line (S.value horz_align) text;
    None

  initializer
    self#set_expand_horz (S.const true);
    self#set_expand_vert (S.const true)
end

let label ?(horz_align=H_align_center) ?(vert_align=V_align_center) text =
  (new label
     ~horz_align:(S.const horz_align)
     ~vert_align:(S.const vert_align)
     (S.const text))#as_widget

class title ?(horz_align=S.const H_align_center) text =
  let event, set_text = E.create () in
  let text = S.switch text event in
  let event, set_horz_align = E.create () in
  let horz_align = S.switch horz_align event in
object(self)
  inherit t

  method text = text
  method set_text = set_text

  method horz_align = horz_align
  method set_horz_align = set_horz_align

  val need_redraw = E.select [
    E.stamp (S.changes text) ();
    E.stamp (S.changes horz_align) ();
  ]
  method need_redraw = need_redraw

  val requested_size = S.map (fun text -> { lines = 1; columns = 4 + Zed_utf8.length text }) text
  method requested_size = requested_size

  method draw ctx focused =
    Lt_draw.draw_string_aligned ctx 0 (S.value horz_align) ("[ " ^ S.value text ^ " ]");
    None

  initializer
    self#set_expand_horz (S.const true)
end

let title ?(horz_align=H_align_center) text =
  (new title ~horz_align:(S.const horz_align) (S.const text))#as_widget

(* +-----------------------------------------------------------------+
   | Boxes                                                           |
   +-----------------------------------------------------------------+ *)

class box (children : t list signal) =
  let event, set_children = E.create () in
  let children = S.switch children event in
object(self)
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

  val mutable children_infos = S.const []
    (* A signal holding a list of elements of the form [(child,
       expand_horz, expand_vert, requested_size)] for all children of the
       box. *)

  initializer
    let s =
      S.map
        (List.rev_map
           (fun child ->
              S.l3 ~eq:(==)
                (fun expand_horz expand_vert size -> (child, expand_horz, expand_vert, size))
                child#expand_horz
                child#expand_vert
                child#requested_size))
        self#children
    in
    let s = S.map (S.merge ~eq:(==) (fun l x -> x :: l) []) s in
    children_infos <- (S.switch (S.value s) (S.changes s))
end

let choose_cursor c1 c2 =
  match c1, c2 with
    | Some _, _ -> c1
    | _, Some _ -> c2
    | None, None -> None

class hbox children =
object(self)
  inherit box children

  val mutable requested_size = S.const { lines = 0; columns = 0 }
  method requested_size = requested_size

  val mutable children_columns = S.const []
    (* Signal holding the list of columns of children. *)

  initializer
    self#set_expand_horz (S.map (List.exists (fun (child, expand_horz, expand_vert, req_size) -> expand_horz)) children_infos);
    self#set_expand_vert (S.map (List.for_all (fun (child, expand_horz, expand_vert, req_size) -> expand_vert)) children_infos);

    requested_size <- (
      S.map
        (List.fold_left
           (fun acc (child, expand_horz, expand_vert, req_size) ->
              { lines = max acc.lines req_size.lines; columns = acc.columns + req_size.columns })
           { lines = 0; columns = 0 })
        children_infos
    );

    children_columns <- (
      S.l2
        (fun infos size ->
           let total_requested_columns =
             List.fold_left
               (fun acc (child, expand_horz, expand_vert, req_size) -> acc + req_size.columns)
               0 infos
           in
           if total_requested_columns <= size.columns then
             (* There is enough space for everybody, we split free
                space between children that can expand. *)
             (* Count the number of children that can expand. *)
             let count_can_expand =
               List.fold_left
                 (fun acc (child, expand_horz, expand_vert, req_size) ->
                    if expand_horz then acc + 1 else acc)
                 0 infos
             in
             (* Divide free space between these children. *)
             let widthf = if count_can_expand = 0 then 0. else float (size.columns - total_requested_columns) /. float count_can_expand in
             (* Compute widths of children. *)
             let rec loop columnf = function
               | [] ->
                   []
               | [(child, expand_horz, expand_vert, req_size)] ->
                   let width = size.columns - truncate columnf in
                   child#handle_event (Lt_event.Resize { lines = size.lines; columns = width });
                   [(child, width)]
               | (child, expand_horz, expand_vert, req_size) :: rest ->
                   if expand_horz then begin
                     let column = truncate columnf in
                     let width = req_size.columns + truncate (columnf +. widthf) - column in
                     child#handle_event (Lt_event.Resize { lines = size.lines; columns = width });
                     (child, width) :: loop (columnf +. float req_size.columns +. widthf) rest
                   end else begin
                     child#handle_event (Lt_event.Resize { lines = size.lines; columns = req_size.columns });
                     (child, req_size.columns) :: loop (columnf +. float req_size.columns) rest
                   end
             in
             loop 0. infos
           else
             (* There is not enough space for everybody. *)
             if total_requested_columns = 0 then
               List.map (fun (child, _, _, _) -> (child, 0)) infos
             else
               let rec loop column = function
                 | [] ->
                     []
                 | [(child, _, _, _)] ->
                     let width = size.columns - column in
                     child#handle_event (Lt_event.Resize { lines = size.lines; columns = width });
                     [(child, width)]
                 | (child, _, _, req_size) :: rest ->
                     let width = req_size.columns * size.columns / total_requested_columns in
                     child#handle_event (Lt_event.Resize { lines = size.lines; columns = width });
                     (child, width) :: loop (column + width) rest
               in
               loop 0 infos)
        children_infos self#size
    )

  method draw ctx focused =
    let size = Lt_draw.size ctx in
    let rec loop column children cursor =
      match children with
        | [] ->
            None
        | (child, columns) :: rest ->
            let cursor' =
              child#draw (Lt_draw.sub ctx {
                            r_line = 0;
                            r_column = column;
                            r_lines = size.lines;
                            r_columns = columns;
                          }) focused
            in
            loop (column + columns) rest (choose_cursor cursor cursor')
    in
    loop 0 (S.value children_columns) None
end

let hbox children = (new hbox (S.const children))#as_widget

class vbox children =
object(self)
  inherit box children

  val mutable requested_size = S.const { lines = 0; columns = 0 }
  method requested_size = requested_size

  val mutable children_lines = S.const []
    (* Signal holding the list of lines of children. *)

  initializer
    self#set_expand_horz (S.map (List.for_all (fun (child, expand_horz, expand_vert, req_size) -> expand_horz)) children_infos);
    self#set_expand_vert (S.map (List.exists (fun (child, expand_horz, expand_vert, req_size) -> expand_vert)) children_infos);

    requested_size <- (
      S.map
        (List.fold_left
           (fun acc (child, expand_horz, expand_vert, req_size) ->
              { lines = acc.lines + req_size.lines; columns = max acc.columns req_size.columns })
           { lines = 0; columns = 0 })
        children_infos
    );

    children_lines <- (
      S.l2
        (fun infos size ->
           let total_requested_lines =
             List.fold_left
               (fun acc (child, expand_horz, expand_vert, req_size) -> acc + req_size.lines)
               0 infos
           in
           if total_requested_lines <= size.lines then
             (* There is enough space for everybody, we split free
                space between children that can expand. *)
             (* Count the number of children that can expand. *)
             let count_can_expand =
               List.fold_left
                 (fun acc (child, expand_horz, expand_vert, req_size) ->
                    if expand_vert then acc + 1 else acc)
                 0 infos
             in
             (* Divide free space between these children. *)
             let heightf = if count_can_expand = 0 then 0. else float (size.lines - total_requested_lines) /. float count_can_expand in
             (* Compute heights of children. *)
             let rec loop linef = function
               | [] ->
                   []
               | [(child, expand_horz, expand_vert, req_size)] ->
                   let height = size.lines - truncate linef in
                   child#handle_event (Lt_event.Resize { lines = height; columns = size.columns });
                   [(child, height)]
               | (child, expand_horz, expand_vert, req_size) :: rest ->
                   if expand_vert then begin
                     let line = truncate linef in
                     let height = req_size.lines + truncate (linef +. heightf) - line in
                     child#handle_event (Lt_event.Resize { lines = height; columns = size.columns });
                     (child, height) :: loop (linef +. float req_size.lines +. heightf) rest
                   end else begin
                     child#handle_event (Lt_event.Resize { lines = req_size.lines; columns = size.columns });
                     (child, req_size.lines) :: loop (linef +. float req_size.lines) rest
                   end
             in
             loop 0. infos
           else
             (* There is not enough space for everybody. *)
             if total_requested_lines = 0 then
               List.map (fun (child, _, _, _) -> (child, 0)) infos
             else
               let rec loop line = function
                 | [] ->
                     []
                 | [(child, _, _, _)] ->
                     let height = size.lines - line in
                     child#handle_event (Lt_event.Resize { lines = height; columns = size.columns });
                     [(child, height)]
                 | (child, _, _, req_size) :: rest ->
                     let height = req_size.lines * size.lines / total_requested_lines in
                     child#handle_event (Lt_event.Resize { lines = height; columns = size.columns });
                     (child, height) :: loop (line + height) rest
               in
               loop 0 infos)
        children_infos self#size
    )

  method draw ctx focused =
    let size = Lt_draw.size ctx in
    let rec loop line children cursor =
      match children with
        | [] ->
            None
        | (child, lines) :: rest ->
            let cursor' =
              child#draw (Lt_draw.sub ctx {
                            r_line = line;
                            r_column = 0;
                            r_lines = lines;
                            r_columns = size.columns;
                          }) focused
            in
            loop (line + lines) rest (choose_cursor cursor cursor')
    in
    loop 0 (S.value children_lines) None
end

let vbox children = (new vbox (S.const children))#as_widget

class frame child =
  let event, set_child = E.create () in
  let child = S.switch child event in
object(self)
  inherit t

  method child = child
  method set_child = set_child
  val children = S.map (fun x -> [x]) child
  method children = children

  val need_redraw =
    let s = S.map (fun child -> child#need_redraw) child in
    E.switch (S.value s) (S.changes s)
  method need_redraw = need_redraw

  val mutable handle_size_changes = S.const ()
  initializer
    handle_size_changes <- (
      S.map
        (fun size ->
           (S.value child)#handle_event
             (Lt_event.Resize { lines = max 0 (size.lines - 2);
                                columns = max 0 (size.columns - 2) }))
        self#size
    )

  method draw ctx focused =
    let size = size ctx in
    if size.lines >= 1 && size.columns >= 1 then
      draw_frame ctx { r_line = 0;
                       r_column = 0;
                       r_lines = size.lines;
                       r_columns = size.columns };
    (S.value child)#draw
      (sub ctx { r_line = min 1 size.lines;
                 r_column = min 1 size.columns;
                 r_lines = max 0 (size.lines - 2);
                 r_columns = max 0 (size.columns - 2) })
      focused

  initializer
  let s = S.map ~eq:(==) (fun child -> child#expand_horz) child in
  self#set_expand_horz (S.switch (S.value s) (S.changes s));
  let s = S.map ~eq:(==) (fun child -> child#expand_vert) child in
  self#set_expand_vert (S.switch (S.value s) (S.changes s))
end

let frame widget = (new frame (S.const widget))#as_widget

class hline = object(self)
  inherit t

  method requested_size = S.const { lines = 1; columns = 0 }

  method draw ctx focused =
    let size = size ctx in
    draw_hline ctx 0 0 size.columns;
    None

  initializer
    self#set_expand_horz (S.const true)
end

let hline () = new hline

class vline = object(self)
  inherit t

  method requested_size = S.const { lines = 0; columns = 1 }

  method draw ctx focused =
    let size = size ctx in
    draw_vline ctx 0 0 size.lines;
    None

  initializer
    self#set_expand_vert (S.const true)
end

let vline () = new vline

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

  val requested_size = S.map (fun text -> { lines = 1; columns = 4 + Zed_utf8.length text }) text
  method requested_size = requested_size

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
    self#set_expand_horz (S.const true);
    self#set_expand_vert (S.const true);
    self#set_can_focus (S.const true)
end

let button text = (new button (S.const text))#as_widget

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
      | Event(Lt_event.Resize new_size as ev) ->
          (* New size, discard current matrices. *)
          matrix_a := [||];
          matrix_b := [||];
          size := new_size;
          widget#handle_event ev;
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
    (* Initialises the size of widgets. *)
    lwt size = Lt_term.get_size term in
    widget#handle_event (Lt_event.Resize size);
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
