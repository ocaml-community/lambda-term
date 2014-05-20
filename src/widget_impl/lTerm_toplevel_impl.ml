open LTerm_geom
open LTerm_key

class t = LTerm_widget_base_impl.t

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

let left coord = { coord with col = pred coord.col }
let right coord = { coord with col = succ coord.col }
let up coord = { coord with row = pred coord.row }
let down coord = { coord with row = succ coord.row }

let focus_to dir f root focused coord =
  let rect = root#allocation in
  let m = make_widget_matrix root in
  let rec loop coord =
    if coord.row < rect.row1 || coord.row >= rect.row2 || coord.col < rect.col1 || coord.col >= rect.col2 then
      None
    else
      match m.(coord.row).(coord.col) with
      | None ->
          loop (dir coord)
      | Some widget when widget = focused ->
          loop (dir coord)
      | Some widget ->
          let rect = widget#allocation in
          Some (widget, f rect coord)
  in
  loop coord

let avg_col rect coord = { coord with col = (rect.col1 + rect.col2) / 2 }
let avg_row rect coord = { coord with row = (rect.row1 + rect.row2) / 2 }

let focus_left (* root focused coord *) = focus_to left avg_col
let focus_right (* root focused coord *) = focus_to right avg_col
let focus_up (* root focused coord *) = focus_to up avg_row
let focus_down (* root focused coord *) = focus_to down avg_row

class toplevel focused widget = object(self)
  inherit t "toplevel" as super
  val children = [widget]
  method children = children
  method draw ctx focused = widget#draw ctx focused

  val mutable coord = { row = 0; col = 0 }
    (* Coordinates of the cursor inside the screen. *)

  val mutable push_layer_handler = Lwt_react.E.never;
  val mutable pop_layer_handler = Lwt_react.E.never;

  method arm_layer_handlers (push_event : t Lwt_react.event)
                            (push_handler : t -> unit)
                            (pop_event : unit Lwt_react.event)
                            (pop_handler : unit -> unit) =
    let open Lwt_react in
    push_layer_handler <- E.map push_handler push_event;
    pop_layer_handler <- E.map pop_handler pop_event

  method set_allocation rect =
    super#set_allocation rect;
    widget#set_allocation rect;
    let rect = !focused#allocation in
    coord <- { row = (rect.row1 + rect.row2) / 2;
               col = (rect.col1 + rect.col2) / 2 }

  method private move_focus direction =
    match direction (self :> t) !focused coord with
    | Some (widget, c) ->
      coord <- c;
      focused := widget;
      self#queue_draw
    | None ->
      ()

  method private process_arrows = function
    | LTerm_event.Key { control = false; meta = false; shift = false; code = Left } ->
        self#move_focus focus_left;
        true
    | LTerm_event.Key { control = false; meta = false; shift = false; code = Right } ->
        self#move_focus focus_right;
        true
    | LTerm_event.Key { control = false; meta = false; shift = false; code = Up } ->
        self#move_focus focus_up;
        true
    | LTerm_event.Key { control = false; meta = false; shift = false; code = Down } ->
        self#move_focus focus_down;
        true
    | other_event ->
        false

  initializer
    widget#set_parent (Some (self :> t));
    self#on_event self#process_arrows

end
