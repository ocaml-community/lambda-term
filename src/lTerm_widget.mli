(*
 * lTerm_widget.mli
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Widgets for creating applications *)

(** {6 Base class} *)

(** The base class. The parameter is the initial resource class. The
    resource class is the first part of all resource keys used by the
    widget.

    For examples, buttons use the resources
    ["button.focused.foreground"], ["button.unfocused.bold"], ... so
    their resource class is ["button"].
*)
class t : string -> object
  method children : t list
    (** The children of the widget. *)

  method parent : t option
    (** The parent of the widget, if any. *)

  method set_parent : t option -> unit
    (** Sets the parent of the widget. This also affect
        {!queue_draw}. *)

  method can_focus : bool
    (** Whether the widget can receive the focus or not. *)

  method focus : t option LTerm_geom.directions
    (** Specify a target widget to the left, right, up and/or down 
        when changing focus. *)

  method set_focus : t option LTerm_geom.directions -> unit
    (** Sets the target widgets when changing focus. *)

  method queue_draw : unit
    (** Enqueue a redraw operation. If the widget has a parent, this
        is the same as calling the {!queue_draw} method of the parent,
        otherwise this does nothing. *)

  method set_queue_draw : (unit -> unit) -> unit
    (** [set_queue_draw f] sets the function called when the
        {!queue_draw} method is invoked, for this widget and all its
        children. *)

  method draw : LTerm_draw.context -> t -> unit
    (** [draw ctx focused] draws the widget on the given
        context. [focused] is the focused widget. *)

  method cursor_position : LTerm_geom.coord option
    (** Method invoked when the widget has the focus, it returns the
        position of the cursor inside the widget if it should be
        displayed. *)

  method allocation : LTerm_geom.rect
    (** The zone occuped by the widget. *)

  method set_allocation : LTerm_geom.rect -> unit
    (** Sets the zone occuped by the widget. *)

  method send_event : LTerm_event.t -> unit
    (** Send an event to the widget. If the widget cannot process the
        event, it is sent to the parent and so on. *)

  method on_event : ?switch : LTerm_widget_callbacks.switch -> (LTerm_event.t -> bool) -> unit
    (** [on_event ?switch f] calls [f] each time an event is
        received. If [f] returns [true], the event is not passed to
        other callbacks. *)

  method size_request : LTerm_geom.size
    (** The size wanted by the widget. *)

  method resources : LTerm_resources.t
    (** The set of resources used by the widget. *)

  method set_resources : LTerm_resources.t -> unit
    (** Sets the resources of the widget and of all its children. *)

  method resource_class : string
    (** The resource class of the widget. *)

  method set_resource_class : string -> unit
    (** Sets the resource class of the widget. This can be used to set
        an alternative style for the widget. *)

  method update_resources : unit
    (** Method invoked when the resources or the resource class of the
        widget change. The default function does nothing. *)
end

(** {6 Labels} *)

(** A widget displaying a text. *)
class label : string -> object
  inherit t

  method text : string
    (** The text of  the label. *)

  method set_text : string -> unit
end

(** {6 Containers} *)

exception Out_of_range

(** Type of widgets displaying a list of widget. *)
class type box = object
  inherit t

  method add : ?position : int -> ?expand : bool -> #t -> unit
    (** [add ?position ?expand widget] adds a widget to the box. If
        [expand] is [true] (the default) then [widget] will occupy as
        much space as possible. If [position] is not specified then
        the widget is appended to the end of the widget list. It
        raises {!Out_of_range} if the given position is negative or
        exceed the number of widgets. *)

  method remove : #t -> unit
    (** [remove widget] remove a widget from the box. *)
end

(** A widget displaying a list of widgets, listed horizontally. *)
class hbox : box

(** A widget displaying a list of widgets, listed vertically. *)
class vbox : box

(** A widget displayiing another widget in a box. *)
class frame : object
  inherit t

  method set : #t -> unit
    (** Set the widget that is inside the frame. *)

  method empty : unit
    (** Remove the child of the frame. *)
end

(** A widget displaying a frame around child widget. Unlike {!frame}, the child
    widget is not expanded to take all available space; instead the child is
    centered and frame is drawn around it. This is a utility class for creation
    of modal dialogs and similar widgets. *)
class modal_frame : object
  inherit frame
end

(** {6 Lines} *)

(** A horizontal line. *)
class hline : t

(** A vertical line. *)
class vline : t

(** {6 Buttons} *)

(** Normal button. *)
class button : string -> object
  inherit t

  method label : string
    (** The text displayed on the button. *)

  method set_label : string -> unit

  method on_click : ?switch : LTerm_widget_callbacks.switch -> (unit -> unit) -> unit
    (** [on_click ?switch f] calls [f] when the button is clicked. *)
end

(** Checkbutton. A button that can be in active or inactive state. *)
class checkbutton : string -> bool -> object
  inherit t

  method label : string
    (** The text displayed on the checkbutton. *)

  method state : bool
    (** The state of checkbutton; [true] means checked and [false] means unchecked. *)

  method set_label : string -> unit

  method on_click : ?switch : LTerm_widget_callbacks.switch -> (unit -> unit) -> unit
  (** [on_click ?switch f] calls [f] when the button state is changed. *)
end

class type ['a] radio = object
  method on : unit
  method off : unit
  method id : 'a
end

(** Radio group.

 Radio group governs the set of {!radio} objects. At each given moment of time only one
 of the objects in the "on" state and the rest are in the "off" state. *)
class ['a] radiogroup : object

  method on_state_change : ?switch : LTerm_widget_callbacks.switch -> ('a option -> unit) -> unit
  (** [on_state_change ?switch f] calls [f] when the state of the group is changed. *)

  method state : 'a option
  (** The state of the group. Contains [Some id] with the id of "on" object
   in the group or None if no objects were added to the group yet. *)

  method register_object : 'a radio -> unit
  (** Adds radio object to the group *)

  method switch_to : 'a -> unit
  (** [switch_to id] switches radio group to the state [Some id], calls {!radio.on}
  method of the object with the given id and {!radio.off} method of all other objects
  added to the group. *)

end

(** Radiobutton. The button which implements {!radio} object contract, so can be
 added to {!radiogroup}. *)
class ['a] radiobutton : 'a radiogroup -> string -> 'a -> object
  inherit t

  method state : bool
  (** The state of the button; [true] if button is "on" and [false] if the button
   is "off". *)

  method on : unit
  (** Switches the button state to "on". Affects only how the button is drawn,
   does not change the state of the group the button is added to.
   Use {!radiogroup.switch_to} instead. *)

  method off : unit
  (** Switches the button state to "off". Affects only how the button is drawn,
   does not change the state of the group the button is added to.
   Use {!radiogroup.switch_to} instead. *)

  method label : string
  (** The text displayed on the radiobutton. *)

  method set_label : string -> unit

  method id : 'a
  (** The id of the button. *)

  method on_click : ?switch:LTerm_widget_callbacks.switch -> (unit -> unit) -> unit
  (** [on_click ?switch f] calls [f] when the button is clicked. You probably want
   to use {!radiogroup.on_state_change} instead. *)

end

(** {6 Running in a terminal} *)

val run : LTerm.t -> ?save_state : bool -> ?load_resources : bool -> ?resources_file : string -> #t -> 'a Lwt.t -> 'a Lwt.t
  (** [run term ?save_state widget w] runs on the given terminal using
      [widget] as main widget. It returns when [w] terminates. If
      [save_state] is [true] (the default) then the state of the
      terminal is saved and restored when [w] terminates.

      If [load_resources] is [true] (the default) then
      [resources_file] (which default to ".lambda-termrc" in the home
      directory) is loaded and the result is set to [w]. *)

val run_modal : LTerm.t -> ?save_state : bool -> ?load_resources : bool -> ?resources_file : string -> t Lwt_react.event -> unit Lwt_react.event -> #t -> 'a Lwt.t -> 'a Lwt.t
  (** This function works in the same way as {!run} but also takes two
   {!Lwt_react.event} parameters. The first one should contain
   {!LTerm_widget.t} widget and makes it new topmost layer in UI. The second
   message removes the topmost level from UI. All layers are redrawn, from
   bottom to up, but only the topmost layer gets keyboard events delivered to
   it. This allows to implement things like modal dialogs.
   *)

val prepare_simple_run : unit -> (#t -> 'a Lwt.t) * (#t -> unit -> unit) * (?step:React.step -> unit -> unit) * ('a -> unit)
  (** [prepare_simple_run ()] returns a tuple [(do_run, push_layer, pop_layer,
     exit)] -- functions useful for creating simple UI.

     [do_run w] where w is a widget runs the given widget in a terminal over
     stdout, loading resources from [.lambda-termrc], saving state and
     restoring it on exit from ui.
     Example: [do_run my_frame]

     [push_layer w] where w is a widget is a callback to add w as a new modal
     layer to UI.
     Example: [button#on_click (push_layer my_modal_dialog)].

     [pop_layer] is a callback to destroy the topmost modal layer.
     Example: [cancel_button#on_click pop_layer].

     [exit] is a callback to exit the UI.
     Example: [exit_button#on_click exit]
*)

