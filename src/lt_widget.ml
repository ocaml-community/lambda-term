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
   | The widget type                                                 |
   +-----------------------------------------------------------------+ *)

type t = {
  children : t list signal;
  can_focus : bool;
  need_redraw : unit event;
  draw : Lt_draw.context -> t -> coord option;
  on_event : Lt_event.t -> unit;
  size_request : size signal;
  expand_horz : bool signal;
  expand_vert : bool signal;
}

let make
    ?(children=S.const [])
    ?(can_focus=false)
    ?(need_redraw=E.never)
    ?(draw=fun _ _ -> None)
    ?(on_event=ignore)
    ?(size_request=S.const { lines = 0; columns = 0 })
    ?(expand_horz=S.const false)
    ?(expand_vert=S.const false)
    () =
  {
    children;
    can_focus;
    need_redraw;
    draw;
    on_event;
    size_request;
    expand_horz;
    expand_vert;
  }

let children w = w.children
let can_focus w = w.can_focus
let need_redraw w = w.need_redraw
let draw w ctx focused = w.draw ctx focused
let send_event w e = w.on_event e
let size_request w = w.size_request
let expand_horz w = w.expand_horz
let expand_vert w = w.expand_vert

(* +-----------------------------------------------------------------+
   | Transformers                                                    |
   +-----------------------------------------------------------------+ *)

let changeable s = {
  children = S.map ~eq:(==) (fun c -> [c]) s;
  can_focus = false;
  need_redraw =
    (let se = S.map ~eq:(==) (fun c -> c.need_redraw) s in
     E.select [
       (* Redraw when the child need to be redrawn. *)
       E.switch (S.value se) (S.changes se);
       (* Or when the child changes. *)
       E.stamp (S.changes s) ();
     ]);
  draw = (fun ctx focused -> (S.value s).draw ctx focused);
  on_event = ignore;
  size_request =
    (let s = S.map ~eq:(==) (fun c -> c.size_request) s in
     S.switch (S.value s) (S.changes s));
  expand_horz =
    (let s = S.map ~eq:(==) (fun c -> c.expand_horz) s in
     S.switch (S.value s) (S.changes s));
  expand_vert =
    (let s = S.map ~eq:(==) (fun c -> c.expand_vert) s in
     S.switch (S.value s) (S.changes s));
}

let event_box ?(on_event=ignore) w = {
  w with
    children = S.const [w];
    can_focus = true;
    on_event;
}

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

let label ?(expand_horz=true) ?(expand_vert=true) ?(horz_align=H_align_center) ?(vert_align=V_align_center) text =
  let size_request = text_size text in
  make
    ~draw:(fun ctx focused ->
             let { lines } = Lt_draw.size ctx in
             let line =
               match vert_align with
                 | V_align_top ->
                     0
                 | V_align_center ->
                     (lines - size_request.lines) / 2
                 | V_align_bottom ->
                     lines - size_request.lines
             in
             Lt_draw.draw_string_aligned ctx line horz_align text;
             None)
    ~size_request:(S.const size_request)
    ~expand_horz:(S.const expand_horz)
    ~expand_vert:(S.const expand_vert)
    ()

let title ?(expand_horz=true) ?(expand_vert=false) ?(horz_align=H_align_center) ?(vert_align=V_align_center) ?(left=Light) ?(middle=Light) ?(right=Light) text =
  make
    ~size_request:(S.const { lines = 1; columns = 6 + Zed_utf8.length text })
    ~expand_horz:(S.const expand_horz)
    ~expand_vert:(S.const expand_vert)
    ~draw:(fun ctx focused ->
             let { lines; columns } = Lt_draw.size ctx in
             let line =
               match vert_align with
                 | V_align_top ->
                     0
                 | V_align_center ->
                     lines / 2
                 | V_align_bottom ->
                     lines - 1
             in
             Lt_draw.draw_hline ctx line 0 columns left middle right;
             Lt_draw.draw_string_aligned ctx line horz_align ("[ " ^ text ^ " ]");
             None)
    ()

(* +-----------------------------------------------------------------+
   | Boxes                                                           |
   +-----------------------------------------------------------------+ *)

let choose_cursor c1 c2 =
  match c1, c2 with
    | Some _, _ -> c1
    | _, Some _ -> c2
    | None, None -> None

let hbox children =
  let draw ctx focused =
    let size = Lt_draw.size ctx in
    let total_requested_columns = List.fold_left (fun acc child -> acc + (S.value child.size_request).columns) 0 children in
    let children_with_widths =
      if total_requested_columns <= size.columns then
        (* There is enough space for everybody, we split free space
           between children that can expand. *)
        (* Count the number of children that can expand. *)
        let count_can_expand = List.fold_left (fun acc child -> if S.value child.expand_horz then acc + 1 else acc) 0 children in
        (* Divide free space between these children. *)
        let widthf = if count_can_expand = 0 then 0. else float (size.columns - total_requested_columns) /. float count_can_expand in
        let rec loop columnf = function
          | [] ->
              []
          | [child] ->
              let width = size.columns - truncate columnf in
              [(child, width)]
          | child :: rest ->
              let req_columns = (S.value child.size_request).columns in
              if S.value child.expand_horz then
                let column = truncate columnf in
                let width = req_columns + truncate (columnf +. widthf) - column in
                (child, width) :: loop (columnf +. float req_columns +. widthf) rest
              else
                (child, req_columns) :: loop (columnf +. float req_columns) rest
        in
        loop 0. children
      else
        (* There is not enough space for everybody. *)
        if total_requested_columns = 0 then
          List.map (fun child -> (child, 0)) children
        else
          let rec loop column = function
            | [] ->
                []
            | [child] ->
                let width = size.columns - column in
                [(child, width)]
            | child :: rest ->
                let width = (S.value child.size_request).columns * size.columns / total_requested_columns in
                (child, width) :: loop (column + width) rest
          in
          loop 0 children
    in
    let rec loop column children cursor =
      match children with
        | [] ->
            None
        | (child, columns) :: rest ->
            let cursor' =
              child.draw
                (Lt_draw.sub ctx {
                   r_line = 0;
                   r_column = column;
                   r_lines = size.lines;
                   r_columns = columns;
                 })
                focused
            in
            loop (column + columns) rest (choose_cursor cursor cursor')
    in
    loop 0 children_with_widths None
  in
  make
    ~children:(S.const children)
    ~need_redraw:(E.select (List.rev_map (fun c -> c.need_redraw) children))
    ~size_request:(S.merge
                       (fun acc size -> { lines = max acc.lines size.lines; columns = acc.columns + size.columns })
                       { lines = 0; columns = 0 }
                       (List.rev_map (fun c -> c.size_request) children))
    ~expand_horz:(S.merge (||) false (List.rev_map (fun c -> c.expand_horz) children))
    ~expand_vert:(S.merge (&&) true (List.rev_map (fun c -> c.expand_vert) children))
    ~draw
    ()

let vbox children =
  let draw ctx focused =
    let size = Lt_draw.size ctx in
    let total_requested_lines = List.fold_left (fun acc child -> acc + (S.value child.size_request).lines) 0 children in
    let children_with_heights =
      if total_requested_lines <= size.lines then
        (* There is enough space for everybody, we split free space
           between children that can expand. *)
        (* Count the number of children that can expand. *)
        let count_can_expand = List.fold_left (fun acc child -> if S.value child.expand_vert then acc + 1 else acc) 0 children in
        (* Divide free space between these children. *)
        let heightf = if count_can_expand = 0 then 0. else float (size.lines - total_requested_lines) /. float count_can_expand in
        let rec loop linef = function
          | [] ->
              []
          | [child] ->
              let height = size.lines - truncate linef in
              [(child, height)]
          | child :: rest ->
              let req_lines = (S.value child.size_request).lines in
              if S.value child.expand_vert then
                let line = truncate linef in
                let height = req_lines + truncate (linef +. heightf) - line in
                (child, height) :: loop (linef +. float req_lines +. heightf) rest
              else
                (child, req_lines) :: loop (linef +. float req_lines) rest
        in
        loop 0. children
      else
        (* There is not enough space for everybody. *)
        if total_requested_lines = 0 then
          List.map (fun child -> (child, 0)) children
        else
          let rec loop line = function
            | [] ->
                []
            | [child] ->
                let height = size.lines - line in
                [(child, height)]
            | child :: rest ->
                let height = (S.value child.size_request).lines * size.lines / total_requested_lines in
                (child, height) :: loop (line + height) rest
          in
          loop 0 children
    in
    let rec loop line children cursor =
      match children with
        | [] ->
            None
        | (child, lines) :: rest ->
            let cursor' =
              child.draw
                (Lt_draw.sub ctx {
                   r_line = line;
                   r_column = 0;
                   r_lines = lines;
                   r_columns = size.columns;
                 })
                focused
            in
            loop (line + lines) rest (choose_cursor cursor cursor')
    in
    loop 0 children_with_heights None
  in
  make
    ~children:(S.const children)
    ~need_redraw:(E.select (List.rev_map (fun c -> c.need_redraw) children))
    ~size_request:(S.merge
                       (fun acc size -> { lines = acc.lines  + size.lines; columns = max acc.columns size.columns })
                       { lines = 0; columns = 0 }
                       (List.rev_map (fun c -> c.size_request) children))
    ~expand_horz:(S.merge (&&) true (List.rev_map (fun c -> c.expand_horz) children))
    ~expand_vert:(S.merge (||) false (List.rev_map (fun c -> c.expand_vert) children))
    ~draw
    ()

let frame ?(connections=Light) child =
  let draw ctx focused =
    let size = size ctx in
    if size.lines >= 1 && size.columns >= 1 then
      draw_frame
        ctx
        { r_line = 0;
          r_column = 0;
          r_lines = size.lines;
          r_columns = size.columns }
        connections;
    child.draw
      (sub ctx { r_line = min 1 size.lines;
                 r_column = min 1 size.columns;
                 r_lines = max 0 (size.lines - 2);
                 r_columns = max 0 (size.columns - 2) })
      focused
  in
  make
    ~children:(S.const [child])
    ~need_redraw:child.need_redraw
    ~draw
    ~size_request:(S.map (fun size -> { lines = size.lines + 2; columns = size.columns + 2 }) child.size_request)
    ~expand_horz:child.expand_horz
    ~expand_vert:child.expand_vert
    ()

let hline ?(left=Light) ?(middle=Light) ?(right=Light) () =
  make
    ~size_request:(S.const { lines = 1; columns = 0 })
    ~draw:(fun ctx focused ->
             draw_hline ctx 0 0 (size ctx).columns left middle right;
             None)
    ~expand_horz:(S.const true)
    ()

let vline ?(top=Light) ?(middle=Light) ?(bottom=Light) () =
  make
    ~size_request:(S.const { lines = 0; columns = 1 })
    ~draw:(fun ctx focused ->
             draw_vline ctx 0 0 (size ctx).lines top middle bottom;
             None)
    ~expand_vert:(S.const true)
    ()

(* +-----------------------------------------------------------------+
   | Buttons                                                         |
   +-----------------------------------------------------------------+ *)

let button ?(expand_horz=true) ?(expand_vert=true) ?(horz_align=H_align_center) ?(vert_align=V_align_center) ?(on_click=ignore) text =
  let self = ref (make ()) in
  let draw ctx focused =
    let { lines; columns } = Lt_draw.size ctx in
    let line =
      match vert_align with
        | V_align_top ->
            0
        | V_align_center ->
            lines / 2
        | V_align_bottom ->
            lines - 1
    in
    let len = Zed_utf8.length text in
    if focused == !self then
      Lt_draw.draw_styled ctx line ((columns - len - 4) / 2)
        (eval [B_bold true; B_fg white; B_bg blue; S"< "; S text; S" >"])
    else
      Lt_draw.draw_styled ctx line ((columns - len - 4) / 2)
        (eval [S"< "; S text; S" >"]);
    None
  and on_event = function
    | Lt_event.Key{ control = false; meta = false; shift = false; code = Enter } -> on_click ()
    | _ -> ()
  in
  let w =
    make
      ~expand_horz:(S.const expand_horz)
      ~expand_vert:(S.const expand_vert)
      ~size_request:(S.const { lines = 1; columns = 4 + Zed_utf8.length text })
      ~can_focus:true
      ~draw
      ~on_event
      ()
  in
  self := w;
  w

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let rec find_focusable widget =
  if widget.can_focus then
    Some widget
  else
    find_focusable_in_list (S.value widget.children)

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
    next_focusable_in_list (S.value widget.children) current

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

let run term ?save_state widget waiter =
  (* The currently focused widget. *)
  let focused =
    ref(match find_focusable widget with
          | Some widget -> widget
          | None -> widget)
  in

  let draw ui matrix =
    let ctx = Lt_draw.context matrix (Lt_ui.size ui) in
    Lt_draw.clear ctx;
    match widget.draw ctx !focused with
      | Some coord ->
          Lt_ui.set_cursor_visible ui true;
          Lt_ui.set_cursor_position ui coord
      | None ->
          Lt_ui.set_cursor_visible ui false
  in

  lwt ui = Lt_ui.create term ?save_state draw in

  (* Loop handling events. *)
  let waiter = waiter >|= fun x -> Value x in
  let rec loop () =
    let thread = Lt_ui.loop ui >|= fun x -> Event x in
    choose [thread; waiter] >>= function
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
          Lt_ui.draw ui;
          loop ()
        end
      | Event ev ->
          !focused.on_event ev;
          loop ()
      | Value value ->
          cancel thread;
          return value
  in

  (* Redraw the screen when the widget needs it. *)
  let ev_redraw = E.map (fun () -> Lt_ui.draw ui) widget.need_redraw in

  try_lwt
    loop ()
  finally
    (* Disable redrawing. *)
    E.stop ev_redraw;
    (* Stop the UI. *)
    Lt_ui.quit ui
