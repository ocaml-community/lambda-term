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

exception Out_of_range = LTerm_containers_impl.Out_of_range
class type box = LTerm_containers_impl.box
class hbox = LTerm_containers_impl.hbox
class vbox = LTerm_containers_impl.vbox
class frame = LTerm_containers_impl.frame
class modal_frame = LTerm_containers_impl.modal_frame

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
  (* Handle layer creation/deletion. *)
  toplevel#set_layer_handlers push_layer push_layer_handler pop_layer pop_layer_handler;
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
    LTerm_ui.quit ui

