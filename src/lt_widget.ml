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

let section = Lwt_log.Section.make "lambda-term(widget)"

(* +-----------------------------------------------------------------+
   | Callbacks                                                       |
   +-----------------------------------------------------------------+ *)

type switch = { mutable switch_state : (unit -> unit) list option }

let register switch_opt seq f =
  match switch_opt with
    | None ->
        ignore (Lwt_sequence.add_l f seq)
    | Some switch ->
        match switch.switch_state with
          | Some l ->
              let node = Lwt_sequence.add_l f seq in
              switch.switch_state <- Some ((fun () -> Lwt_sequence.remove node) :: l)
          | None ->
              ()

let stop switch =
  match switch.switch_state with
    | Some l ->
        switch.switch_state <- None;
        List.iter (fun f -> f ()) l
    | None ->
        ()

let exec_callbacks seq x =
  Lwt_sequence.iter_l
    (fun f ->
       try
         f x
       with exn ->
         ignore (Lwt_log.error ~section ~exn "callback failed with"))
    seq

let exec_filters seq x =
  Lwt_sequence.fold_l
    (fun f acc ->
       if acc then
         true
       else begin
         try
           f x
         with exn ->
           ignore (Lwt_log.error ~section ~exn "filter failed with");
           false
       end)
    seq false

(* +-----------------------------------------------------------------+
   | The widget class                                                |
   +-----------------------------------------------------------------+ *)

class t : object
  method children : t list
  method parent : t option
  method set_parent : t option -> unit
  method ui : Lt_ui.t option
  method set_ui : Lt_ui.t option -> unit
  method can_focus : bool
  method queue_draw : unit
  method draw : Lt_draw.context -> t -> coord option
  method allocation : rect
  method set_allocation : rect -> unit
  method send_event : Lt_event.t -> unit
  method on_event : ?switch : switch -> (Lt_event.t -> bool) -> unit
  method size_request : size
end = object(self)

  method children : t list = []

  method can_focus = false

  val mutable ui : Lt_ui.t option = None
  method ui = ui
  method set_ui opt =
    ui <- opt;
    List.iter (fun child -> child#set_ui opt) self#children

  val mutable parent : t option = None
  method parent = parent
  method set_parent opt =
    parent <- opt;
    match opt with
      | Some w ->
          self#set_ui w#ui
      | None ->
          self#set_ui None

  method queue_draw =
    match ui with
      | Some ui ->
          Lt_ui.draw ui
      | None ->
          ()

  method draw (ctx : Lt_draw.context) (focused : t) : coord option = None

  val mutable allocation = { r_line = 0; r_column = 0; r_lines = 0; r_columns = 0 }
  method allocation = allocation
  method set_allocation rect = allocation <- rect

  val event_filters = Lwt_sequence.create ()

  method send_event ev =
    if not (exec_filters event_filters ev) then
      match parent with
        | Some widget ->
            widget#send_event ev
        | None ->
            ()

  method on_event ?switch f = register switch event_filters f

  val size_request = { lines = 0; columns = 0 }
  method size_request = size_request
end

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

class label initial_text = object(self)
  inherit t
  val mutable text = initial_text
  val mutable size_request = text_size initial_text
  method text = text
  method set_text t =
    text <- t;
    size_request <- text_size t;
    self#queue_draw
  method size_request = size_request
  method draw ctx focused =
    let { lines } = Lt_draw.size ctx in
    let line = (lines - size_request.lines) / 2 in
    Lt_draw.draw_string_aligned ctx line H_align_center text;
    None
end

(* +-----------------------------------------------------------------+
   | Boxes                                                           |
   +-----------------------------------------------------------------+ *)

let choose_cursor c1 c2 =
  match c1, c2 with
    | Some _, _ -> c1
    | _, Some _ -> c2
    | None, None -> None

exception Out_of_range

let rec insert x l n =
  if n < 0 then
    raise Out_of_range
  else if n = 0 then
    x :: l
  else
    match l with
      | [] ->
          raise Out_of_range
      | y :: l ->
          y :: insert x l (n - 1)

type box_child = {
  widget : t;
  expand : bool;
  mutable length : int;
}

class type box = object
  inherit t
  method add : ?position : int -> ?expand : bool -> #t -> unit
  method remove : #t -> unit
end

class virtual abox = object(self)
  inherit t as super

  val mutable children = []
  method children = List.map (fun child -> child.widget) children

  val mutable size_request = { lines = 0; columns = 0 }
  method size_request = size_request

  method private virtual compute_allocations : unit
    (* Compute sizes of children. *)

  method private virtual compute_size_request : unit
    (* Compute the size request. *)

  method set_allocation rect =
    super#set_allocation rect;
    self#compute_allocations

  method add : 'a. ?position : int -> ?expand : bool -> (#t as 'a) -> unit = fun ?position ?(expand = true) widget ->
    let child = {
      widget = (widget :> t);
      expand = expand;
      length = 0;
    } in
    (match position with
       | Some n ->
           children <- insert child children n
       | None ->
           children <- children @ [child]);
    widget#set_parent (Some (self :> t));
    self#compute_size_request;
    self#compute_allocations;
    self#queue_draw

  method remove : 'a. (#t as 'a) -> unit = fun widget ->
    children <- List.filter (fun child -> if child.widget = (widget :> t) then (child.widget#set_parent None; false) else true) children;
    self#compute_size_request;
    self#compute_allocations;
    self#queue_draw
end

class hbox = object(self)
  inherit abox

  method private compute_size_request =
    size_request <- (
      List.fold_left
        (fun acc child ->
           let size = child.widget#size_request in
           { lines = max acc.lines size.lines; columns = acc.columns + size.columns })
        { lines = 0; columns = 0 }
        children
    )

  method private compute_allocations =
    let rect = self#allocation in
    let total_requested_columns = List.fold_left (fun acc child -> acc + child.widget#size_request.columns) 0 children in
    if total_requested_columns <= rect.r_columns then begin
      (* There is enough space for everybody, we split free space
         between children that can expand. *)
      (* Count the number of children that can expand. *)
      let count_can_expand = List.fold_left (fun acc child -> if child.expand then acc + 1 else acc) 0 children in
      (* Divide free space between these children. *)
      let widthf = if count_can_expand = 0 then 0. else float (rect.r_columns - total_requested_columns) /. float count_can_expand in
      let rec loop columnf = function
        | [] ->
            ()
        | [child] ->
            let width = rect.r_columns - truncate columnf in
            child.length <- width
        | child :: rest ->
            let req_columns = child.widget#size_request.columns in
            if child.expand then begin
              let column = truncate columnf in
              let width = req_columns + truncate (columnf +. widthf) - column in
              child.length <- width;
              loop (columnf +. float req_columns +. widthf) rest
            end else begin
              child.length <- req_columns;
              loop (columnf +. float req_columns) rest
            end
      in
      loop 0. children
    end else begin
      (* There is not enough space for everybody. *)
      if total_requested_columns = 0 then
        List.iter (fun child -> child.length <- 0) children
      else
        let rec loop column = function
          | [] ->
              ()
          | [child] ->
              let width = rect.r_columns - column in
              child.length <- width
          | child :: rest ->
              let width = child.widget#size_request.columns * rect.r_columns / total_requested_columns in
              child.length <- width;
              loop (column + width) rest
        in
        loop 0 children
    end;
    ignore (
      List.fold_left
        (fun column child ->
           child.widget#set_allocation {
             r_line = rect.r_line;
             r_column = column;
             r_lines = rect.r_lines;
             r_columns = child.length;
           };
           column + child.length)
        rect.r_column children
    )

  method draw ctx focused =
    let rect = self#allocation in
    let rec loop column children cursor =
      match children with
        | [] ->
            cursor
        | child :: rest ->
            let cursor' =
              child.widget#draw
                (Lt_draw.sub ctx {
                   r_line = 0;
                   r_column = column;
                   r_lines = rect.r_lines;
                   r_columns = child.length;
                 })
                focused
            in
            loop (column + child.length) rest (choose_cursor cursor cursor')
    in
    loop 0 children None
end

class vbox = object(self)
  inherit abox

  method private compute_size_request =
    size_request <- (
      List.fold_left
        (fun acc child ->
           let size = child.widget#size_request in
           { lines = acc.lines  + size.lines; columns = max acc.columns size.columns })
        { lines = 0; columns = 0 }
        children
    )

  method private compute_allocations =
    let rect = self#allocation in
    let total_requested_lines = List.fold_left (fun acc child -> acc + child.widget#size_request.lines) 0 children in
    if total_requested_lines <= rect.r_lines then begin
      (* There is enough space for everybody, we split free space
         between children that can expand. *)
      (* Count the number of children that can expand. *)
      let count_can_expand = List.fold_left (fun acc child -> if child.expand then acc + 1 else acc) 0 children in
      (* Divide free space between these children. *)
      let heightf = if count_can_expand = 0 then 0. else float (rect.r_lines - total_requested_lines) /. float count_can_expand in
      let rec loop linef = function
        | [] ->
            ()
        | [child] ->
            let height = rect.r_lines - truncate linef in
            child.length <- height
        | child :: rest ->
            let req_lines = child.widget#size_request.lines in
            if child.expand then begin
              let line = truncate linef in
              let height = req_lines + truncate (linef +. heightf) - line in
              child.length <- height;
              loop (linef +. float req_lines +. heightf) rest
            end else begin
              child.length <- req_lines;
              loop (linef +. float req_lines) rest
            end
      in
      loop 0. children
    end else begin
      (* There is not enough space for everybody. *)
      if total_requested_lines = 0 then
        List.iter (fun child -> child.length <- 0) children
      else
        let rec loop line = function
          | [] ->
              ()
          | [child] ->
              let height = rect.r_lines - line in
              child.length <- height
          | child :: rest ->
              let height = child.widget#size_request.lines * rect.r_lines / total_requested_lines in
              child.length <- height;
              loop (line + height) rest
        in
        loop 0 children
    end;
    ignore (
      List.fold_left
        (fun line child ->
           child.widget#set_allocation {
             r_line = line;
             r_column = rect.r_column;
             r_lines = child.length;
             r_columns = rect.r_columns;
           };
           line + child.length)
        rect.r_line children
    )

  method draw ctx focused =
    let rect = self#allocation in
    let rec loop line children cursor =
      match children with
        | [] ->
            cursor
        | child :: rest ->
            let cursor' =
              child.widget#draw
                (Lt_draw.sub ctx {
                   r_line = line;
                   r_column = 0;
                   r_lines = child.length;
                   r_columns = rect.r_columns;
                 })
                focused
            in
            loop (line + child.length) rest (choose_cursor cursor cursor')
    in
    loop 0 children None
end

class frame ?(connections = Light) () =
  let initial_connections = connections in
object(self)
  inherit t as super

  val mutable child = None
  method children =
    match child with
      | Some widget -> [widget]
      | None -> []

  val mutable size_request = { lines = 2; columns = 2 }
  method size_request = size_request

  method private compute_size_request =
    match child with
      | Some widget ->
          let size = widget#size_request in
          size_request <- { lines = size.lines + 2; columns = size.columns + 2 }
      | None ->
          size_request <- { lines = 2; columns = 2 }

  method private compute_allocation =
    match child with
      | Some widget ->
          let rect = self#allocation in
          widget#set_allocation {
            r_line = min (rect.r_line + rect.r_lines) (rect.r_line + 1);
            r_column = min (rect.r_column + rect.r_columns) (rect.r_column + 1);
            r_lines = max 0 (rect.r_lines - 2);
            r_columns = max 0 (rect.r_columns - 2);
          }
      | None ->
          ()

  method set_allocation rect =
    super#set_allocation rect;
    self#compute_allocation

  method set : 'a. (#t as 'a) -> unit = fun widget ->
    child <- Some(widget :> t);
    widget#set_parent (Some (self :> t));
    self#compute_size_request;
    self#compute_allocation;
    self#queue_draw

  method empty =
    match child with
      | Some widget ->
          widget#set_parent None;
          child <- None;
          self#compute_size_request;
          self#queue_draw
      | None ->
          ()

  val mutable connections = initial_connections

  method connections = connections

  method set_connections c =
    connections <- c;
    self#queue_draw

  method draw ctx focused =
    let size = Lt_draw.size ctx in
    if size.lines >= 1 && size.columns >= 1 then
      draw_frame
        ctx
        { r_line = 0;
          r_column = 0;
          r_lines = size.lines;
          r_columns = size.columns }
        connections;
    match child with
      | Some widget ->
          widget#draw
            (sub ctx { r_line = min 1 size.lines;
                       r_column = min 1 size.columns;
                       r_lines = max 0 (size.lines - 2);
                       r_columns = max 0 (size.columns - 2) })
            focused
      | None ->
          None
end

(* +-----------------------------------------------------------------+
   | Lines                                                           |
   +-----------------------------------------------------------------+ *)

class type line = object
  inherit t
  method connections : Lt_draw.connection * Lt_draw.connection * Lt_draw.connection
  method set_connections : Lt_draw.connection * Lt_draw.connection * Lt_draw.connection -> unit
end

class hline ?(connections = (Light, Light, Light)) () =
  let initial_connections = connections in
object(self)
  inherit t

  val size_request = { lines = 1; columns = 0 }
  method size_request = size_request

  val mutable connections = initial_connections

  method connections = connections

  method set_connections c =
    connections <- c;
    self#queue_draw

  method draw ctx focused =
    let { lines } = Lt_draw.size ctx in
    let a, b, c = connections in
    draw_hline ctx (lines / 2) 0 (Lt_draw.size ctx).columns a b c;
    None
end

class vline ?(connections = (Light, Light, Light)) () =
  let initial_connections = connections in
object(self)
  inherit t

  val size_request = { lines = 0; columns = 1 }
  method size_request = size_request

  val mutable connections = initial_connections

  method connections = connections

  method set_connections c =
    connections <- c;
    self#queue_draw

  method draw ctx focused =
    let { columns } = Lt_draw.size ctx in
    let a, b, c = connections in
    draw_vline ctx 0 (columns / 2) (Lt_draw.size ctx).lines a b c;
    None
end

(* +-----------------------------------------------------------------+
   | Buttons                                                         |
   +-----------------------------------------------------------------+ *)

class button initial_label = object(self)
  inherit t as super

  method can_focus = true

  val click_callbacks = Lwt_sequence.create ()

  method on_click ?switch f =
    register switch click_callbacks f

  val mutable size_request = { lines = 1; columns = 4 + Zed_utf8.length initial_label }
  method size_request = size_request

  val mutable label = initial_label

  method label = label

  method set_label text =
    label <- text;
    size_request <- { lines = 1; columns = 4 + Zed_utf8.length text };
    self#queue_draw

  initializer
    self#on_event
      (function
         | Lt_event.Key { control = false; meta = false; shift = false; code = Enter } ->
             exec_callbacks click_callbacks ();
             true
         | _ ->
             false)

  method draw ctx focused =
    let { lines; columns } = Lt_draw.size ctx in
    let len = Zed_utf8.length label in
    if focused = (self :> t) then
      Lt_draw.draw_styled ctx (lines / 2) ((columns - len - 4) / 2)
        (eval [B_bold true; B_fg white; B_bg blue; S"< "; S label; S" >"])
    else
      Lt_draw.draw_styled ctx (lines / 2) ((columns - len - 4) / 2)
        (eval [S"< "; S label; S" >"]);
    None
end

(* +-----------------------------------------------------------------+
   | Focus cycling                                                   |
   +-----------------------------------------------------------------+ *)

let make_widget_matrix root =
  let { r_lines = lines; r_columns = columns } = root#allocation in
  let m = Array.make_matrix lines columns None in
  let rec loop widget =
    if widget#can_focus then begin
      let rect = widget#allocation in
      for l = 0 to rect.r_lines - 1 do
        for c = 0 to rect.r_columns - 1 do
          m.(rect.r_line + l).(rect.r_column + c) <- Some widget
        done
      done
    end;
    List.iter loop widget#children
  in
  loop root;
  m

let focus_left root focused coord =
  let { r_lines = lines; r_columns = columns } = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.line < 0 || coord.line >= lines || coord.column < 0 || coord.column >= columns then
      None
    else
      match m.(coord.line).(coord.column) with
        | None ->
            loop { coord with column = coord.column - 1 }
        | Some widget when widget = focused ->
            loop { coord with column = coord.column - 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with column = rect.r_column + rect.r_columns / 2 })
  in
  loop coord

let focus_right root focused coord =
  let { r_lines = lines; r_columns = columns } = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.line < 0 || coord.line >= lines || coord.column < 0 || coord.column >= columns then
      None
    else
      match m.(coord.line).(coord.column) with
        | None ->
            loop { coord with column = coord.column + 1 }
        | Some widget when widget = focused ->
            loop { coord with column = coord.column + 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with column = rect.r_column + rect.r_columns / 2 })
  in
  loop coord

let focus_up root focused coord =
  let { r_lines = lines; r_columns = columns } = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.line < 0 || coord.line >= lines || coord.column < 0 || coord.column >= columns then
      None
    else
      match m.(coord.line).(coord.column) with
        | None ->
            loop { coord with line = coord.line - 1 }
        | Some widget when widget = focused ->
            loop { coord with line = coord.line - 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with line = rect.r_line + rect.r_lines / 2 })
  in
  loop coord

let focus_down root focused coord =
  let { r_lines = lines; r_columns = columns } = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.line < 0 || coord.line >= lines || coord.column < 0 || coord.column >= columns then
      None
    else
      match m.(coord.line).(coord.column) with
        | None ->
            loop { coord with line = coord.line + 1 }
        | Some widget when widget = focused ->
            loop { coord with line = coord.line + 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with line = rect.r_line + rect.r_lines / 2 })
  in
  loop coord

let rec find_focusable widget =
  if widget#can_focus then
    Some widget
  else
    find_focusable_in_list widget#children

and find_focusable_in_list = function
  | [] ->
      None
  | child :: rest ->
      match find_focusable child with
        | Some _ as some -> some
        | None -> find_focusable_in_list rest

(* +-----------------------------------------------------------------+
   | The toplevel widget                                             |
   +-----------------------------------------------------------------+ *)

class toplevel focused widget = object(self)
  inherit t as super
  val children = [widget]
  method children = children
  method draw ctx focused = widget#draw ctx focused

  val mutable coord = { line = 0; column = 0 }
    (* Coordinates of the cursor inside the screen. *)

  method set_allocation rect =
    super#set_allocation rect;
    widget#set_allocation rect;
    let rect = !focused#allocation in
    coord <- { line = rect.r_line + rect.r_lines / 2;
               column = rect.r_column + rect.r_columns / 2 }

  initializer
    widget#set_parent (Some (self :> t));
    self#on_event
      (function
         | Lt_event.Key { control = false; meta = false; shift = false; code = Left } ->
             (match focus_left (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | Lt_event.Key { control = false; meta = false; shift = false; code = Right } ->
             (match focus_right (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | Lt_event.Key { control = false; meta = false; shift = false; code = Up } ->
             (match focus_up (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | Lt_event.Key { control = false; meta = false; shift = false; code = Down } ->
             (match focus_down (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | ev ->
             false)
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

(* An event for the main loop. *)
type 'a event =
  | Value of 'a
      (* A value from the waiter thread. *)
  | Event of Lt_event.t
      (* A event from the terminal. *)

let run term ?save_state widget waiter =
  let widget = (widget :> t) in

  (* The currently focused widget. *)
  let focused =
    ref(match find_focusable widget with
          | Some widget -> widget
          | None -> widget)
  in

  (* Create a toplevel widget. *)
  let toplevel = new toplevel focused widget in

  let draw ui matrix =
    let ctx = Lt_draw.context matrix (Lt_ui.size ui) in
    Lt_draw.clear ctx;
    match toplevel#draw ctx !focused with
      | Some coord ->
          Lt_ui.set_cursor_visible ui true;
          Lt_ui.set_cursor_position ui coord
      | None ->
          Lt_ui.set_cursor_visible ui false
  in

  lwt ui = Lt_ui.create term ?save_state draw in
  toplevel#set_ui (Some ui);
  let size = Lt_ui.size ui in
  toplevel#set_allocation {
    r_line = 0;
    r_column = 0;
    r_lines = size.lines;
    r_columns = size.columns;
  };

  (* Loop handling events. *)
  let waiter = waiter >|= fun x -> Value x in
  let rec loop () =
    let thread = Lt_ui.loop ui >|= fun x -> Event x in
    choose [thread; waiter] >>= function
      | Event(Lt_event.Resize size) ->
          toplevel#set_allocation {
            r_line = 0;
            r_column = 0;
            r_lines = size.lines;
            r_columns = size.columns;
          };
          loop ()
      | Event ev ->
          !focused#send_event ev;
          loop ()
      | Value value ->
          cancel thread;
          return value
  in

  try_lwt
    loop ()
  finally
    Lt_ui.quit ui
