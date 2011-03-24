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

(** {6 Definitions} *)

(** Type of widgets. *)
class t : object
  method as_widget : t
    (** Sub-type the widget to the standard widget type. *)

  method children : t list signal
    (** The children of the widget. *)

  method can_focus : bool signal
    (** Whether the focus can receive the focus or not. *)

  method set_can_focus : bool signal -> unit

  method need_redraw : unit event
    (** Event which occurs when the widget need to be redrawn. *)

  method draw : Lt_draw.context -> t -> coord option
    (** [draw ctx focused] draws the widget on the given
        context. [focused] is widget that is currently focused. It
        must returns the position of the cursor inside the widget if
        it is focused and the cursor should be displayed. *)

  method handle_event : Lt_event.t -> unit
    (** Handles the given event. Note that widgets never receive
        resize events. *)

  method key_pressed : Lt_key.t event
    (** Event occuring when the widget has the focus and a key is
        pressed. *)

  method size : size signal
    (** The signal holding the current size of the widget. This size
        is updated when the widget receive a {!Lt_event.Resize}
        event. *)

  method requested_size : size signal
    (** The size wanted by the widget. *)

  method expand_horz : bool signal
    (** Whether to expand the widget horizontally if there is free
        space. *)

  method expand_vert : bool signal
    (** Whether to expand the widget vertically if there is free
        space. *)

  method set_expand_horz : bool signal -> unit
  method set_expand_vert : bool signal -> unit
end

(** {6 Simple widgets} *)

(** A widget displaying some text. *)
class label :
  ?horz_align : horz_alignment signal ->
  ?vert_align : vert_alignment signal ->
  string signal ->
object
  inherit t

  method text : string signal
    (** The signal holding the displayed text. *)

  method set_text : string signal -> unit

  method horz_align : horz_alignment signal
  method vert_align : vert_alignment signal

  method set_horz_align : horz_alignment signal -> unit
  method set_vert_align : vert_alignment signal -> unit
end

val label : ?horz_align : horz_alignment -> ?vert_align : vert_alignment -> string -> t

class title : ?horz_align : horz_alignment signal -> string signal -> object
  inherit t

  method text : string signal
    (** The signal holding the title. *)

  method set_text : string signal -> unit

  method horz_align : horz_alignment signal
  method set_horz_align : horz_alignment signal -> unit
end

val title : ?horz_align : horz_alignment -> string -> t

class hbox : t list signal -> object
  inherit t
  method children : t list signal
  method set_children : t list signal -> unit
end

val hbox : t list -> t

class vbox : t list signal -> object
  inherit t
  method children : t list signal
  method set_children : t list signal -> unit
end

val vbox : t list -> t

class frame : t signal -> object
  inherit t
  method child : t signal
  method set_child : t signal -> unit
end

val frame : t -> t

class hline : t
class vline : t

val hline : unit -> t
val vline : unit -> t

class button : string signal -> object
  inherit t

  method text : string signal
    (** The text of the button. *)

  method set_text : string signal -> unit

  method clicked : unit event
    (** Event which occurs when the button is clicked. *)
end

val button : string -> t

(** {6 Running in a terminal} *)

val run : Lt_term.t -> ?save_state : bool -> #t -> 'a Lwt.t -> 'a Lwt.t
  (** [run term ?save_state widget w] runs on the given terminal using
      [widget] as main widget. It returns when [w] terminates. If
      [save_state] is [true] (the default) then the state of the
      terminal is saved and restored when [w] terminates. *)
