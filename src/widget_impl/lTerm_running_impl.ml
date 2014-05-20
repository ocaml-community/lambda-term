open Lwt
open LTerm_geom

type t = LTerm_widget_base_impl.t

class toplevel = LTerm_toplevel_impl.toplevel

(* for focus cycling *)
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

let run_modal term ?save_state ?(load_resources = true) ?(resources_file = lambda_termrc) push_event pop_event widget waiter =
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

  (* Layer event handlers. *)
  let push_layer w =
    let new_focus = ref_focus w in
    let new_top = new toplevel new_focus w in
    new_top#set_queue_draw !draw_toplevel;
    new_top#set_allocation !size_ref;
    focuses := new_focus :: !focuses;
    layers := new_top :: !layers;
    new_top#set_resources !resources_cache;
    new_top#queue_draw
  in
  let pop_layer () =
    match !layers with
    | [_] ->
        failwith "Trying to destroy the only existing layer."
    | _ :: tl ->
        layers := tl;
        focuses := List.tl !focuses;
        (List.hd !layers)#queue_draw
    | [] ->
        failwith "Internal error: no idea how it happened."
  in
  (* Arm layer event handlers. *)
  toplevel#arm_layer_handlers push_event push_layer pop_event pop_layer;

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
        LTerm_ui.set_cursor_position ui { row = rect.row1 + coord.row;
                                          col = rect.col1 + coord.col }
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
    LTerm_ui.quit ui

let run term ?save_state ?load_resources ?resources_file widget waiter =
  run_modal term ?save_state ?load_resources ?resources_file Lwt_react.E.never Lwt_react.E.never widget waiter

let prepare_simple_run () =
  let waiter, wakener = wait () in
  let push_ev, push_ev_send = Lwt_react.E.create () in
  let pop_ev, pop_ev_send = Lwt_react.E.create () in
  let exit = wakeup wakener in
  let push_layer w = fun () -> push_ev_send (w :> t) in
  let pop_layer = pop_ev_send in
  let do_run w =
    lwt term = Lazy.force LTerm.stdout in
    run_modal term push_ev pop_ev w waiter
  in
  (do_run, push_layer, pop_layer, exit)
