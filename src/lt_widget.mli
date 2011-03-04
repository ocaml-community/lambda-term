(*
 * lt_widget.mli
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open React
open Lt_types

(** {6 Definitions} *)

(** Type of widgets. *)
class t : object
  method need_redraw : unit event
    (** Event which occurs when the widget need to be redrawn. *)

  method draw : Lt_draw.context -> unit
    (** Draw the widget on the given context. *)

  method handle_event : Lt_event.t -> unit
    (** Handles the given event. Note that widgets never receive
        resize events. *)

  method key_pressed : Lt_key.t event
    (** Event occuring when the widget has the focus and a key is
        pressed. *)
end

(** {6 Simple widgets} *)

class label : string signal -> t

(** {6 Running in a terminal} *)

val run : Lt_term.t -> ?save_state : bool -> #t -> 'a Lwt.t -> 'a Lwt.t
  (** [run term ?save_state widget w] runs on the given terminal using
      [widget] as main widget. It returns when [w] terminates. If
      [save_state] is [true] (the default) then the state of the
      terminal is saved and restored when [w] terminates. *)
