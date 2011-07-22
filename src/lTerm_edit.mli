(*
 * lTerm_edit.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Text edition *)

(** {6 Bindings} *)

val bindings : (LTerm_key.t, Zed_edit.action) Hashtbl.t
  (** Bindings. These bindings are used by {!LTerm_read_line} and by
      edition widgets. *)

(** {6 Widgets} *)

val clipboard : Zed_edit.clipboard
  (** The global clipboard. *)

(** Class of edition widgets. If no clipboard is provided, then the
    global one is used. *)
class edit : ?clipboard : Zed_edit.clipboard -> unit -> object
  inherit LTerm_widget.t

  method engine : edit Zed_edit.t
    (** The edition engine used by this widget. *)

  method cursor : Zed_cursor.t
    (** The cursor used by this widget. *)

  method context : edit Zed_edit.context
    (** The context for editing the engine. *)

  method text : string
    (** Shorthand for [Zed_rope.to_string (Zed_edit.text
        edit#engine)]. *)

  method editable : int -> int -> bool
    (** The editable function of the engine. *)

  method move : int -> int -> int
    (** The move function of the engine. *)

  method match_word : Zed_rope.t -> int -> int option
    (** The match word function of the engine. *)

  method locale : string option
    (** The locale used by the engine. *)

  method set_locale : string option -> unit
end
