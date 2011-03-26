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

  method need_redraw : unit event
    (** Event which occurs when the widget need to be redrawn. *)

  method draw : Lt_draw.context -> t -> coord option
    (** [draw ctx focused] draws the widget on the given
        context. [focused] is widget that is currently focused. It
        must returns the position of the cursor inside the widget if
        it is focused and the cursor should be displayed. *)

  method handle_event : Lt_event.t -> unit
    (** Handles the given event. A widget receive events only if it is
        focusable. *)

  method key_pressed : Lt_key.t event
    (** Event occuring when the widget has the focus and a key is
        pressed. *)

  method clicked : unit event
    (** Event which occurs when the user press {!Lt_key.Enter} on the
        widget. *)

  method requested_size : size signal
    (** The size wanted by the widget. *)

  method expand_horz : bool signal
    (** Whether the widget can expand horizontally. *)

  method expand_vert : bool signal
    (** Whether the widget can expand vertically. *)
end

(** {6 Modifiers} *)

class changeable : t signal -> t
  (** Class of widgets that can change over time. *)

val changeable : t signal -> t

class focusable : t -> t
  (** Class of widget that may receive the focus. *)

val focusable : t -> t

(** {6 Simple widgets} *)

class label :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  string -> t
  (** A widget displaying a text. *)

val label :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  string -> t

class title :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  ?left : Lt_draw.connection ->
  ?middle : Lt_draw.connection ->
  ?right : Lt_draw.connection ->
  string  -> t
  (** A widget displaying a title, of form "---[ title ]---" *)

val title :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  ?left : Lt_draw.connection ->
  ?middle : Lt_draw.connection ->
  ?right : Lt_draw.connection ->
  string  -> t

class hbox : t list -> t
  (** A widget displaying a list of widget, listed horizontally. *)

val hbox : t list -> t

class vbox : t list -> t
  (** A widget displaying a list of widget, listed vertically. *)

val vbox : t list -> t

class frame : ?connections : Lt_draw.connection -> t -> t
  (** A widget displayiing another widget in a box. *)

val frame : ?connections : Lt_draw.connection -> t -> t

class hline : ?left : Lt_draw.connection -> ?middle : Lt_draw.connection -> ?right : Lt_draw.connection -> unit -> t
  (** A horizontal line. *)

val hline : ?left : Lt_draw.connection -> ?middle : Lt_draw.connection -> ?right : Lt_draw.connection -> unit -> t

class vline : ?top : Lt_draw.connection -> ?middle : Lt_draw.connection -> ?bottom : Lt_draw.connection -> unit -> t
  (** A vertical line. *)

val vline : ?top : Lt_draw.connection -> ?middle : Lt_draw.connection -> ?bottom : Lt_draw.connection -> unit -> t

class button :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  string -> t
  (** A button. *)

val button :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  string -> t

(** {6 Running in a terminal} *)

val run : Lt_term.t -> ?save_state : bool -> #t -> 'a Lwt.t -> 'a Lwt.t
  (** [run term ?save_state widget w] runs on the given terminal using
      [widget] as main widget. It returns when [w] terminates. If
      [save_state] is [true] (the default) then the state of the
      terminal is saved and restored when [w] terminates. *)
