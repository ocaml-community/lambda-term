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
open LTerm_widget_callbacks

(* +-----------------------------------------------------------------+
   | The widget class                                                |
   +-----------------------------------------------------------------+ *)

class t = LTerm_widget_base_impl.t

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

class button = LTerm_buttons_impl.button
class checkbutton = LTerm_buttons_impl.checkbutton
class type ['a] radio = ['a] LTerm_buttons_impl.radio
class ['a] radiogroup = ['a] LTerm_buttons_impl.radiogroup
class ['a] radiobutton = ['a] LTerm_buttons_impl.radiobutton

(* +-----------------------------------------------------------------+
   | Focus cycling                                                   |
   +-----------------------------------------------------------------+ *)

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

class toplevel = LTerm_toplevel_impl.toplevel

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

let apply_resources ?cache load_resources resources_file widget =
  if load_resources then
    match_lwt file_exists resources_file with
    | true -> lwt resources = LTerm_resources.load resources_file in
        widget#set_resources resources;
        begin
          match cache with
          | None -> ()
          | Some c -> c := resources
        end;
        return ()
    | false ->
        return ()
  else
    return ()

let ref_focus widget =
  ref (match find_focusable widget with
        | Some w -> w
        | None -> widget)

let run term ?save_state ?(load_resources = true) ?(resources_file = lambda_termrc) widget waiter =
  let widget = (widget :> t) in

  lwt () = apply_resources load_resources resources_file widget in

  (* The currently focused widget. *)
  let focused = ref_focus widget in

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

let nothing a b = ()

let run_modal term ?save_state ?(load_resources = true) ?(resources_file = lambda_termrc) push_layer pop_layer widget waiter =
  let widget = (widget :> t) in
  let resources_cache = ref LTerm_resources.empty in

  lwt () = apply_resources ~cache:resources_cache load_resources resources_file widget in

  (* The currently focused widget. *)
  let focused = ref_focus widget in

  (* Create a toplevel widget. *)
  let toplevel = new toplevel focused widget in

  (* Drawing function for toplevels. *)
  let draw_toplevel = ref (fun () -> ()) in

  (* Size for toplevels. *)
  let size_ref = ref { row1 = 0; col1 = 0; row2 = 0; col2 = 0 } in

  let layers = ref [toplevel] in
  let focuses = ref [focused] in

  (* Layer signal handlers. *)
  let push_layer_handler w =
    let w = (w :> t) in
    let new_focus = ref_focus w in
    let new_top = new toplevel new_focus w in
    new_top#set_queue_draw !draw_toplevel;
    new_top#set_allocation !size_ref;
    focuses := new_focus :: !focuses;
    layers := new_top :: !layers;
    new_top#set_resources !resources_cache;
    new_top#queue_draw
  in
  let pop_layer_handler () =
    match !layers with
    | [_] -> failwith "Internal error: trying to destroy non-modal layer."
    | _ :: tl ->
        layers := tl;
        focuses := List.tl !focuses;
        (List.hd !layers)#queue_draw
    | [] -> failwith "Internal error: no idea how it happened."
  in

  (* Handle layer creation/deletion. *)
  let push_handler = E.map push_layer_handler push_layer in
  let pop_handler = E.map pop_layer_handler pop_layer in

  let draw ui matrix =
    let ctx = LTerm_draw.context matrix (LTerm_ui.size ui) in
    LTerm_draw.clear ctx;
    (* Draw the layers starting from the bottom *)
    let layers_rev = List.rev !layers in
    let focuses_rev = List.rev !focuses in
    List.iter2 (fun top focus -> top#draw ctx !focus) layers_rev focuses_rev;
    let current_focus = List.hd !focuses in
    match !current_focus#cursor_position with
    | Some coord ->
        let rect = !current_focus#allocation in
        LTerm_ui.set_cursor_visible ui true;
        LTerm_ui.set_cursor_position ui { row = rect.row1 + coord.row; col = rect.col1 + coord.col }
    | None ->
        LTerm_ui.set_cursor_visible ui false
  in

  lwt ui = LTerm_ui.create term ?save_state draw in
  draw_toplevel := (fun () -> LTerm_ui.draw ui);
  toplevel#set_queue_draw !draw_toplevel;
  let size = LTerm_ui.size ui in
  size_ref := { !size_ref with row2 = size.rows; col2 = size.cols};
  toplevel#set_allocation !size_ref;

  (* Loop handling events. *)
  let waiter = waiter >|= fun x -> Value x in
  let rec loop () =
    let thread = LTerm_ui.wait ui >|= fun x -> Event x in
    choose [thread; waiter] >>= function
      | Event (LTerm_event.Resize size) ->
          size_ref := { !size_ref with row2 = size.rows; col2 = size.cols};
          List.iter (fun top -> top#set_allocation !size_ref) !layers;
          loop ()
      | Event ev ->
          !(List.hd !focuses)#send_event ev;
          loop ()
      | Value value ->
          cancel thread;
          return value
  in

  try_lwt
    loop ()
  finally
    nothing push_handler pop_handler;
    LTerm_ui.quit ui

