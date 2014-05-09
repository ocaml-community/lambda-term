(*
 * lTerm_widget.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open Lwt
open LTerm_geom
open LTerm_draw
open LTerm_key
open LTerm_style
open LTerm_text

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

class t initial_resource_class : object
  method children : t list
  method parent : t option
  method set_parent : t option -> unit
  method can_focus : bool
  method queue_draw : unit
  method set_queue_draw : (unit -> unit) -> unit
  method draw : LTerm_draw.context -> t -> unit
  method cursor_position : coord option
  method allocation : rect
  method set_allocation : rect -> unit
  method send_event : LTerm_event.t -> unit
  method on_event : ?switch : switch -> (LTerm_event.t -> bool) -> unit
  method size_request : size
  method resources : LTerm_resources.t
  method set_resources : LTerm_resources.t -> unit
  method resource_class : string
  method set_resource_class : string -> unit
  method update_resources : unit
end = object(self)

  method children : t list = []

  method can_focus = false

  val mutable parent : t option = None
  method parent = parent
  method set_parent opt = parent <- opt

  val mutable queue_draw = ignore
  method queue_draw = queue_draw ()
  method set_queue_draw f =
    queue_draw <- f;
    List.iter (fun w -> w#set_queue_draw f) self#children

  method draw (ctx : LTerm_draw.context) (focused : t) = ()
  method cursor_position = None

  val mutable allocation = { row1 = 0; col1 = 0; row2 = 0; col2 = 0 }
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

  val size_request = { rows = 0; cols = 0 }
  method size_request = size_request

  val mutable resource_class = initial_resource_class
  method resource_class = resource_class
  method set_resource_class rc =
    resource_class <- rc;
    self#update_resources

  val mutable resources = LTerm_resources.empty
  method resources = resources
  method set_resources res =
    resources <- res;
    self#update_resources;
    List.iter (fun w -> w#set_resources res) self#children

  method update_resources = ()
end

(* +-----------------------------------------------------------------+
   | Labels                                                          |
   +-----------------------------------------------------------------+ *)

let newline = UChar.of_char '\n'

let text_size str =
  let rec loop ofs rows cols max_cols =
    if ofs = String.length str then
      { rows; cols = max cols max_cols }
    else
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      if chr = newline then
        if ofs = String.length str then
          { rows; cols = max cols max_cols }
        else
          loop ofs (rows + 1) 0 (max cols max_cols)
      else
        loop ofs rows (cols + 1) max_cols
  in
  loop 0 1 0 0

class label initial_text = object(self)
  inherit t "label"
  val mutable text = initial_text

  val mutable size_request = text_size initial_text
  method size_request = size_request

  val mutable style = LTerm_style.none
  method update_resources =
    style <- LTerm_resources.get_style self#resource_class self#resources

  method text = text
  method set_text t =
    text <- t;
    size_request <- text_size t;
    self#queue_draw

  method draw ctx focused =
    let { rows } = LTerm_draw.size ctx in
    let row = (rows - size_request.rows) / 2 in
    LTerm_draw.fill_style ctx style;
    LTerm_draw.draw_string_aligned ctx row H_align_center text
end

(* +-----------------------------------------------------------------+
   | Boxes                                                           |
   +-----------------------------------------------------------------+ *)

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

class virtual abox rc = object(self)
  inherit t rc as super

  val mutable children = []
  method children = List.map (fun child -> child.widget) children

  val mutable size_request = { rows = 0; cols = 0 }
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
  inherit abox "hbox"

  method private compute_size_request =
    size_request <- (
      List.fold_left
        (fun acc child ->
           let size = child.widget#size_request in
           { rows = max acc.rows size.rows; cols = acc.cols + size.cols })
        { rows = 0; cols = 0 }
        children
    )

  method private compute_allocations =
    let rect = self#allocation in
    let cols = rect.col2 - rect.col1 in
    let total_requested_cols = List.fold_left (fun acc child -> acc + child.widget#size_request.cols) 0 children in
    if total_requested_cols <= cols then begin
      (* There is enough space for everybody, we split free space
         between children that can expand. *)
      (* Count the number of children that can expand. *)
      let count_can_expand = List.fold_left (fun acc child -> if child.expand then acc + 1 else acc) 0 children in
      (* Divide free space between these children. *)
      let widthf = if count_can_expand = 0 then 0. else float (cols - total_requested_cols) /. float count_can_expand in
      let rec loop colf = function
        | [] ->
            ()
        | [child] ->
            let width = cols - truncate colf in
            child.length <- width
        | child :: rest ->
            let req_cols = child.widget#size_request.cols in
            if child.expand then begin
              let col = truncate colf in
              let width = req_cols + truncate (colf +. widthf) - col in
              child.length <- width;
              loop (colf +. float req_cols +. widthf) rest
            end else begin
              child.length <- req_cols;
              loop (colf +. float req_cols) rest
            end
      in
      loop 0. children
    end else begin
      (* There is not enough space for everybody. *)
      if total_requested_cols = 0 then
        List.iter (fun child -> child.length <- 0) children
      else
        let rec loop col = function
          | [] ->
              ()
          | [child] ->
              let width = cols - col in
              child.length <- width
          | child :: rest ->
              let width = child.widget#size_request.cols * cols / total_requested_cols in
              child.length <- width;
              loop (col + width) rest
        in
        loop 0 children
    end;
    ignore (
      List.fold_left
        (fun col child ->
           child.widget#set_allocation {
             row1 = rect.row1;
             col1 = col;
             row2 = rect.row2;
             col2 = col + child.length;
           };
           col + child.length)
        rect.col1 children
    )

  method draw ctx focused =
    let rect = self#allocation in
    let rec loop col children =
      match children with
        | [] ->
            ()
        | child :: rest ->
            child.widget#draw
              (LTerm_draw.sub ctx {
                 row1 = 0;
                 col1 = col;
                 row2 = rect.row2 - rect.row1;
                 col2 = col + child.length;
               })
              focused;
            loop (col + child.length) rest
    in
    loop 0 children
end

class vbox = object(self)
  inherit abox "vbox"

  method private compute_size_request =
    size_request <- (
      List.fold_left
        (fun acc child ->
           let size = child.widget#size_request in
           { rows = acc.rows  + size.rows; cols = max acc.cols size.cols })
        { rows = 0; cols = 0 }
        children
    )

  method private compute_allocations =
    let rect = self#allocation in
    let rows = rect.row2 - rect.row1 in
    let total_requested_rows = List.fold_left (fun acc child -> acc + child.widget#size_request.rows) 0 children in
    if total_requested_rows <= rows then begin
      (* There is enough space for everybody, we split free space
         between children that can expand. *)
      (* Count the number of children that can expand. *)
      let count_can_expand = List.fold_left (fun acc child -> if child.expand then acc + 1 else acc) 0 children in
      (* Divide free space between these children. *)
      let heightf = if count_can_expand = 0 then 0. else float (rows - total_requested_rows) /. float count_can_expand in
      let rec loop rowf = function
        | [] ->
            ()
        | [child] ->
            let height = rows - truncate rowf in
            child.length <- height
        | child :: rest ->
            let req_rows = child.widget#size_request.rows in
            if child.expand then begin
              let row = truncate rowf in
              let height = req_rows + truncate (rowf +. heightf) - row in
              child.length <- height;
              loop (rowf +. float req_rows +. heightf) rest
            end else begin
              child.length <- req_rows;
              loop (rowf +. float req_rows) rest
            end
      in
      loop 0. children
    end else begin
      (* There is not enough space for everybody. *)
      if total_requested_rows = 0 then
        List.iter (fun child -> child.length <- 0) children
      else
        let rec loop row = function
          | [] ->
              ()
          | [child] ->
              let height = rows - row in
              child.length <- height
          | child :: rest ->
              let height = child.widget#size_request.rows * rows / total_requested_rows in
              child.length <- height;
              loop (row + height) rest
        in
        loop 0 children
    end;
    ignore (
      List.fold_left
        (fun row child ->
           child.widget#set_allocation {
             row1 = row;
             col1 = rect.col1;
             row2 = row + child.length;
             col2 = rect.col2;
           };
           row + child.length)
        rect.row1 children
    )

  method draw ctx focused =
    let rect = self#allocation in
    let rec loop row children =
      match children with
        | [] ->
            ()
        | child :: rest ->
            child.widget#draw
              (LTerm_draw.sub ctx {
                 row1 = row;
                 col1 = 0;
                 row2 = row + child.length;
                 col2 = rect.col2 - rect.col1;
               })
              focused;
            loop (row + child.length) rest
    in
    loop 0 children
end

class frame = object(self)
  inherit t "frame" as super

  val mutable child = None
  method children =
    match child with
      | Some widget -> [widget]
      | None -> []

  val mutable size_request = { rows = 2; cols = 2 }
  method size_request = size_request

  val mutable style = LTerm_style.none
  val mutable connection = LTerm_draw.Light
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    connection <- LTerm_resources.get_connection (rc ^ ".connection") resources

  method private compute_size_request =
    match child with
      | Some widget ->
          let size = widget#size_request in
          size_request <- { rows = size.rows + 2; cols = size.cols + 2 }
      | None ->
          size_request <- { rows = 2; cols = 2 }

  method private compute_allocation =
    match child with
      | Some widget ->
          let rect = self#allocation in
          let row1 = min rect.row2 (rect.row1 + 1) and col1 = min rect.col2 (rect.col1 + 1) in
          widget#set_allocation {
            row1 = row1;
            col1 = col1;
            row2 = max row1 (rect.row2 - 1);
            col2 = max col1 (rect.col2 - 1);
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

  method draw ctx focused =
    let size = LTerm_draw.size ctx in
    LTerm_draw.fill_style ctx style;
    if size.rows >= 1 && size.cols >= 1 then begin
      draw_frame
        ctx
        { row1 = 0;
          col1 = 0;
          row2 = size.rows;
          col2 = size.cols }
        connection;
      if size.rows > 2 && size.cols > 2 then
        match child with
          | Some widget ->
              widget#draw
                (sub ctx { row1 = 1;
                           col1 = 1;
                           row2 = size.rows - 1;
                           col2 = size.cols - 1 })
                focused
          | None ->
              ()
    end
end

(* +-----------------------------------------------------------------+
   | Lines                                                           |
   +-----------------------------------------------------------------+ *)

class hline = object(self)
  inherit t "hline"

  val size_request = { rows = 1; cols = 0 }
  method size_request = size_request

  val mutable style = LTerm_style.none
  val mutable connection = LTerm_draw.Light
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    connection <- LTerm_resources.get_connection (rc ^ ".connection") resources

  method draw ctx focused =
    let { rows } = LTerm_draw.size ctx in
    LTerm_draw.fill_style ctx style;
    draw_hline ctx (rows / 2) 0 (LTerm_draw.size ctx).cols connection
end

class vline = object(self)
  inherit t "vline"

  val size_request = { rows = 0; cols = 1 }
  method size_request = size_request

  val mutable style = LTerm_style.none
  val mutable connection = LTerm_draw.Light
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    connection <- LTerm_resources.get_connection (rc ^ ".connection") resources

  method draw ctx focused =
    let { cols } = LTerm_draw.size ctx in
    LTerm_draw.fill_style ctx style;
    draw_vline ctx 0 (cols / 2) (LTerm_draw.size ctx).rows connection
end

(* +-----------------------------------------------------------------+
   | Buttons                                                         |
   +-----------------------------------------------------------------+ *)

class button initial_label = object(self)
  inherit t "button" as super

  method can_focus = true

  val click_callbacks = Lwt_sequence.create ()

  method on_click ?switch f =
    register switch click_callbacks f

  val mutable size_request = { rows = 1; cols = 4 + Zed_utf8.length initial_label }
  method size_request = size_request

  val mutable label = initial_label

  method label = label

  method set_label text =
    label <- text;
    size_request <- { rows = 1; cols = 4 + Zed_utf8.length text };
    self#queue_draw

  initializer
    self#on_event
      (function
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Enter } ->
             exec_callbacks click_callbacks ();
             true
         | _ ->
             false)

  val mutable focused_style = LTerm_style.none
  val mutable unfocused_style = LTerm_style.none
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
    unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources

  method draw ctx focused =
    let { rows; cols } = LTerm_draw.size ctx in
    let len = Zed_utf8.length label in
    if focused = (self :> t) then begin
      LTerm_draw.fill_style ctx focused_style;
      LTerm_draw.draw_string ctx (rows / 2) ((cols - len - 4) / 2) (Printf.sprintf "< %s >" label)
    end else begin
      LTerm_draw.fill_style ctx unfocused_style;
      LTerm_draw.draw_string ctx (rows / 2) ((cols - len - 4) / 2) (Printf.sprintf "< %s >" label)
    end
end

class checkbutton initial_label initial_state = object(self)
  inherit button initial_label

  val mutable state = initial_state

  initializer
    self#on_event
    (function
      | LTerm_event.Key { control = false; meta = false; shift = false; code }
        when (code = Enter || code = Char(UChar.of_char ' ')) ->
          state <- not state;
          (* checkbutton changes the state when clicked, so has to be redrawn *)
          self#queue_draw;
          exec_callbacks click_callbacks ();
          true
      | _ ->
          false);
    self#set_resource_class "checkbutton"

  method state = state

  method draw ctx focused =
    let { rows; cols } = LTerm_draw.size ctx in
    let style = if focused = (self :> t) then focused_style else unfocused_style in
    let checked = if state then "[x]" else "[ ]" in
    begin
      LTerm_draw.fill_style ctx style;
      LTerm_draw.draw_string ctx (rows / 2) 0 (checked ^ label);
    end

end


(* +-----------------------------------------------------------------+
   | Focus cycling                                                   |
   +-----------------------------------------------------------------+ *)

let make_widget_matrix root =
  let { rows; cols } = LTerm_geom.size_of_rect root#allocation in
  let m = Array.make_matrix rows cols None in
  let rec loop widget =
    if widget#can_focus then begin
      let rect = widget#allocation in
      for r = rect.row1 to rect.row2 - 1 do
        for c = rect.col1 to rect.col2 - 1 do
          m.(r).(c) <- Some widget
        done
      done
    end;
    List.iter loop widget#children
  in
  loop root;
  m

let focus_left root focused coord =
  let rect = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.row < rect.row1 || coord.row >= rect.row2 || coord.col < rect.col1 || coord.col >= rect.col2 then
      None
    else
      match m.(coord.row).(coord.col) with
        | None ->
            loop { coord with col = coord.col - 1 }
        | Some widget when widget = focused ->
            loop { coord with col = coord.col - 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with col = (rect.col1 + rect.col2) / 2 })
  in
  loop coord

let focus_right root focused coord =
  let rect = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.row < rect.row1 || coord.row >= rect.row2 || coord.col < rect.col1 || coord.col >= rect.col2 then
      None
    else
      match m.(coord.row).(coord.col) with
        | None ->
            loop { coord with col = coord.col + 1 }
        | Some widget when widget = focused ->
            loop { coord with col = coord.col + 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with col = (rect.col1 + rect.col2) / 2 })
  in
  loop coord

let focus_up root focused coord =
  let rect = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.row < rect.row1 || coord.row >= rect.row2 || coord.col < rect.col1 || coord.col >= rect.col2 then
      None
    else
      match m.(coord.row).(coord.col) with
        | None ->
            loop { coord with row = coord.row - 1 }
        | Some widget when widget = focused ->
            loop { coord with row = coord.row - 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with row = (rect.row1 + rect.row2) / 2 })
  in
  loop coord

let focus_down root focused coord =
  let rect = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.row < rect.row1 || coord.row >= rect.row2 || coord.col < rect.col1 || coord.col >= rect.col2 then
      None
    else
      match m.(coord.row).(coord.col) with
        | None ->
            loop { coord with row = coord.row + 1 }
        | Some widget when widget = focused ->
            loop { coord with row = coord.row + 1 }
        | Some widget ->
            let rect = widget#allocation in
            Some(widget, { coord with row = (rect.row1 + rect.row2) / 2 })
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
  inherit t "toplevel" as super
  val children = [widget]
  method children = children
  method draw ctx focused = widget#draw ctx focused

  val mutable coord = { row = 0; col = 0 }
    (* Coordinates of the cursor inside the screen. *)

  method set_allocation rect =
    super#set_allocation rect;
    widget#set_allocation rect;
    let rect = !focused#allocation in
    coord <- { row = (rect.row1 + rect.row2) / 2;
               col = (rect.col1 + rect.col2) / 2 }

  initializer
    widget#set_parent (Some (self :> t));
    self#on_event
      (function
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Left } ->
             (match focus_left (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Right } ->
             (match focus_right (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Up } ->
             (match focus_up (self :> t) !focused coord with
                | Some(widget, c) ->
                    coord <- c;
                    focused := widget;
                    self#queue_draw
                | None ->
                    ());
             true
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Down } ->
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
  | Event of LTerm_event.t
      (* A event from the terminal. *)

let lambda_termrc =
  Filename.concat LTerm_resources.home ".lambda-termrc"

let file_exists file =
  try_lwt
    lwt () = Lwt_unix.access file [Unix.R_OK] in
    return true
  with Unix.Unix_error _ ->
    return false

let run term ?save_state ?(load_resources = true) ?(resources_file = lambda_termrc) widget waiter =
  let widget = (widget :> t) in

  lwt () =
    if load_resources then
      match_lwt file_exists resources_file with
        | true ->
            lwt resources = LTerm_resources.load resources_file in
            widget#set_resources resources;
            return ()
        | false ->
            return ()
    else
      return ()
  in

  (* The currently focused widget. *)
  let focused =
    ref(match find_focusable widget with
          | Some widget -> widget
          | None -> widget)
  in

  (* Create a toplevel widget. *)
  let toplevel = new toplevel focused widget in

  let draw ui matrix =
    let ctx = LTerm_draw.context matrix (LTerm_ui.size ui) in
    LTerm_draw.clear ctx;
    toplevel#draw ctx !focused;
    match !focused#cursor_position with
      | Some coord ->
          let rect = !focused#allocation in
          LTerm_ui.set_cursor_visible ui true;
          LTerm_ui.set_cursor_position ui { row = rect.row1 + coord.row; col = rect.col1 + coord.col }
      | None ->
          LTerm_ui.set_cursor_visible ui false
  in

  lwt ui = LTerm_ui.create term ?save_state draw in
  toplevel#set_queue_draw (fun () -> LTerm_ui.draw ui);
  let size = LTerm_ui.size ui in
  toplevel#set_allocation {
    row1 = 0;
    col1 = 0;
    row2 = size.rows;
    col2 = size.cols;
  };

  (* Loop handling events. *)
  let waiter = waiter >|= fun x -> Value x in
  let rec loop () =
    let thread = LTerm_ui.wait ui >|= fun x -> Event x in
    choose [thread; waiter] >>= function
      | Event(LTerm_event.Resize size) ->
          toplevel#set_allocation {
            row1 = 0;
            col1 = 0;
            row2 = size.rows;
            col2 = size.cols;
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
    LTerm_ui.quit ui
