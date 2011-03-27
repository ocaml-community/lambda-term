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

type t
  (** Type of widgets. *)

val make :
  ?children : t list signal ->
  ?can_focus : bool ->
  ?need_redraw : unit event ->
  ?draw : (Lt_draw.context -> t -> coord option) ->
  ?on_event : (Lt_event.t -> unit) ->
  ?requested_size : size signal ->
  ?expand_horz : bool signal ->
  ?expand_vert : bool signal ->
  unit -> t

val children : t -> t list signal
  (** The children of the widget. *)

val can_focus : t -> bool
  (** Whether the widget can receive the focus or not. *)

val need_redraw : t -> unit event
  (** Event which occurs when the widget need to be redrawn. *)

val draw : t -> Lt_draw.context -> t -> coord option
  (** [draw ctx focused] draws the widget on the given
      context. [focused] is the focused widget. It returns the
      position of the cursor inside the widget if it is focused and
      the cursor should be displayed. *)

val send_event : t -> Lt_event.t -> unit
  (** Sends the given event to the given widget. *)

val requested_size : t -> size signal
  (** The size wanted by the widget. *)

val expand_horz : t -> bool signal
  (** Whether the widget can expand horizontally. *)

val expand_vert : t -> bool signal
  (** Whether the widget can expand vertically. *)

(** {6 Transformers} *)

val changeable : t signal -> t
  (** A widget that contains a widget that can change over time. *)

val event_box : ?on_event : (Lt_event.t -> unit) -> t -> t
  (** Transform a widget into a widget that can receive the focus. *)

(** {6 Simple widgets} *)

val label :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  string -> t
  (** A widget displaying a text. *)

val title :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  ?left : Lt_draw.connection ->
  ?middle : Lt_draw.connection ->
  ?right : Lt_draw.connection ->
  string -> t
  (** A widget displaying a title, of form "---[ title ]---" *)

val hbox : t list -> t
  (** A widget displaying a list of widget, listed horizontally. *)

val vbox : t list -> t
  (** A widget displaying a list of widget, listed vertically. *)

val frame : ?connections : Lt_draw.connection -> t -> t
  (** A widget displayiing another widget in a box. *)

val hline : ?left : Lt_draw.connection -> ?middle : Lt_draw.connection -> ?right : Lt_draw.connection -> unit -> t
  (** A horizontal line. *)

val vline : ?top : Lt_draw.connection -> ?middle : Lt_draw.connection -> ?bottom : Lt_draw.connection -> unit -> t
  (** A vertical line. *)

val button :
  ?expand_horz : bool ->
  ?expand_vert : bool ->
  ?horz_align : horz_alignment ->
  ?vert_align : vert_alignment ->
  ?on_click : (unit -> unit) ->
  string -> t
  (** A button. *)

(** {6 Running in a terminal} *)

val run : Lt_term.t -> ?save_state : bool -> t -> 'a Lwt.t -> 'a Lwt.t
  (** [run term ?save_state widget w] runs on the given terminal using
      [widget] as main widget. It returns when [w] terminates. If
      [save_state] is [true] (the default) then the state of the
      terminal is saved and restored when [w] terminates. *)
