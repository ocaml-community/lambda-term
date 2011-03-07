(*
 * lt_edit.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Text edition *)

(** {6 Bindings} *)

val bindings : (Lt_key.t, Zed_edit.action) Hashtbl.t
  (** Bindings. These bindings are used by {!Lt_read_line} and by
      edition widgets. *)
