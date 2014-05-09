(*
 * lTerm_widget.mli
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Widgets for creating applications *)

open React
open LTerm_geom

(** {6 Callbacks} *)

type switch
  (** Switches are used to stop signals. *)

val register : switch option -> 'a Lwt_sequence.t -> 'a -> unit
  (** *)

val stop : switch -> unit
  (** *)

val exec_callbacks : ('a -> unit) Lwt_sequence.t -> 'a -> unit
  (** [apply_callbacks callbacks x] *)

val exec_filters : ('a -> bool) Lwt_sequence.t -> 'a -> bool

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

  method cursor_position : coord option
    (** Method invoked when the widget has the focus, it returns the
        position of the cursor inside the widget if it should be
        displayed. *)

  method allocation : rect
    (** The zone occuped by the widget. *)

  method set_allocation : rect -> unit
    (** Sets the zone occuped by the widget. *)

  method send_event : LTerm_event.t -> unit
    (** Send an event to the widget. If the widget cannot process the
        event, it is sent to the parent and so on. *)

  method on_event : ?switch : switch -> (LTerm_event.t -> bool) -> unit
    (** [on_event ?switch f] calls [f] each time an event is
        received. If [f] returns [true], the event is not passed to
        other callbacks. *)

  method size_request : size
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

class hbox : box
  (** A widget displaying a list of widgets, listed horizontally. *)

class vbox : box
  (** A widget displaying a list of widgets, listed vertically. *)

(** A widget displayiing another widget in a box. *)
class frame : object
  inherit t

  method set : #t -> unit
    (** Set the widget that is inside the frame. *)

  method empty : unit
    (** Remove the child of the frame. *)
end

(** {6 Lines} *)

class hline : t
  (** A horizontal line. *)

class vline : t
  (** A vertical line. *)

(** {6 Buttons} *)

(** Normal button. *)
class button : string -> object
  inherit t

  method label : string
    (** The text displayed on the button. *)

  method set_label : string -> unit

  method on_click : ?switch : switch -> (unit -> unit) -> unit
    (** [on_click ?switch f] calls [f] when the button is clicked. *)
end

(** Checkbutton.
 *  A button that can be in active or inactive state. *)
class checkbutton : string -> bool -> object
  inherit t

  method label : string
    (** The text displayed on the checkbutton. *)

  method state : bool
    (** The state of checkbutton; [true] means checked and [false] means unchecked. *)

  method set_label : string -> unit

  method on_click : ?switch : switch -> (unit -> unit) -> unit
  (** [on_click ?switch f] calls [f] when the button state is changed. *)
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
