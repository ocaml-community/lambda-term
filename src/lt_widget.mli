(*
 * lt_widget.mli
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open React
open Lt_geom

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

class t : object
  method children : t list
    (** The children of the widget. *)

  method parent : t option
    (** The parent of the widget, if any. *)

  method set_parent : t option -> unit
    (** Sets the parent of the widget. This also affect {!ui}. *)

  method ui : Lt_ui.t option
    (** The UI on which the widget is displayed. *)

  method set_ui : Lt_ui.t option -> unit
    (** Sets the UI on which the widget is displayed. This also sets
        the UI of all the children of the widget. *)

  method can_focus : bool
    (** Whether the widget can receive the focus or not. *)

  method queue_draw : unit
    (** Enqueue a redraw operation. This does nothing if {!ui} is not
        set. *)

  method draw : Lt_draw.context -> t -> coord option
    (** [draw ctx focused] draws the widget on the given
        context. [focused] is the focused widget. It returns the
        position of the cursor inside the widget if it is focused and
        the cursor should be displayed. *)

  method allocation : rect
    (** The zone occuped by the widget. *)

  method set_allocation : rect -> unit
    (** Sets the zone occuped by the widget. *)

  method send_event : Lt_event.t -> unit
    (** Send an event to the widget. If the widget cannot process the
        event, it is sent to the parent and so on. *)

  method on_event : ?switch : switch -> (Lt_event.t -> bool) -> unit
    (** [on_event ?switch f] calls [f] each time an event is
        received. If [f] returns [true], the event is not passed to
        other callbacks. *)

  method size_request : size
    (** The size wanted by the widget. *)
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
class frame : ?connections : Lt_draw.connection -> unit -> object
  inherit t

  method set : #t -> unit
    (** Set the widget that is inside the frame. *)

  method empty : unit
    (** Remove the child of the frame. *)

  method connections : Lt_draw.connection
    (** The connection used to draw the box around the widget. *)

  method set_connections : Lt_draw.connection -> unit
end

(** {6 Lines} *)

class type line = object
  inherit t

  method connections : Lt_draw.connection * Lt_draw.connection * Lt_draw.connection
    (** The start, middle and end connection of the line. *)

  method set_connections : Lt_draw.connection * Lt_draw.connection * Lt_draw.connection -> unit
end

class hline : ?connections : Lt_draw.connection * Lt_draw.connection * Lt_draw.connection -> unit -> line
  (** A horizontal line. *)

class vline : ?connections : Lt_draw.connection * Lt_draw.connection * Lt_draw.connection -> unit -> line
  (** A vertical line. *)

(** {6 Buttons} *)

class button : string -> object
  inherit t

  method label : string
    (** The text displayed on the button. *)

  method set_label : string -> unit

  method on_click : ?switch : switch -> (unit -> unit) -> unit
    (** [on_click ?switch f] calls [f] when the button is clicked. *)
end

(** {6 Running in a terminal} *)

val run : Lt_term.t -> ?save_state : bool -> #t -> 'a Lwt.t -> 'a Lwt.t
  (** [run term ?save_state widget w] runs on the given terminal using
      [widget] as main widget. It returns when [w] terminates. If
      [save_state] is [true] (the default) then the state of the
      terminal is saved and restored when [w] terminates. *)
