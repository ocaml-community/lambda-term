(*
 * lTerm_widget_callbacks.mli
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type switch
  (** Switches are used to stop signals. *)

val register : switch option -> 'a Lwt_sequence.t -> 'a -> unit
  (** *)

val stop : switch -> unit
  (** *)

val exec_callbacks : ('a -> unit) Lwt_sequence.t -> 'a -> unit
  (** [apply_callbacks callbacks x] *)

val exec_filters : ('a -> bool) Lwt_sequence.t -> 'a -> bool

