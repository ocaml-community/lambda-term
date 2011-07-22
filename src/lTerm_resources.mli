(*
 * lTerm_resources.mli
 * -------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Widgets resources *)

type t
  (** Type of resources. *)

val empty : t
  (** The empty set of resources. *)

val get : string -> t -> string
  (** [get key resources] returns the key associated to the last
      pattern that matches [key] in [resources], or the empty string
      if no pattern matches [key]. *)

val add : string -> string -> t -> t
  (** [add pattern value] returns the new set of resources with the
      binding [pattern -> value] at the end. *)

val merge : t -> t -> t
  (** [merge res1 res2] merges the two given sets of resources. *)

exception Error of string
  (** Exception raised when the contents of a resource is invalid. *)

val get_bool : string -> t -> bool option
  (** [get_bool name resources] reads the boolean encoded in
      [resources]. *)

val get_color : string -> t -> LTerm_style.color option
  (** [get_color name resources] reads the color encoded in
      [resources]. *)

val get_style : string -> t -> LTerm_style.t
  (** [get_style prefix resources] reads the style encoded in
      [resources]. *)

val get_connection : string -> t -> LTerm_draw.connection
  (** [get_connection name resources] *)

exception Parse_error of string * int * string
  (** [Parse_error(source, line, msg)] is raised when a parsing error
      is encountered in the input. *)

val parse : string -> t
  (** [parse str] parses a string for a list of properties. [str] must
      follow the format of X resources files. i.e. comments start with a
      [!], empty lines are ignored, and configuration lines looks-like:

      {[
        key: value
      ]}
  *)

val load : string -> t Lwt.t
  (** Same as {!parse} but parses the contents of a file. *)
