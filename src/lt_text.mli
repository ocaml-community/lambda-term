(*
 * lt_text.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Styled text. *)

open CamomileLibrary

type t = (UChar.t * Lt_style.t) array
    (** Type of a string with styles for each characters. *)

val of_string : Zed_utf8.t -> t
  (** Creates a styled string from a string. All characters of the
      string have no style. *)

val to_string : t -> Zed_utf8.t
  (** Returns the string part of a styled string. *)

val stylise : string -> Lt_style.t -> t
  (** [stylise string style] creates a styled string with all styles
      set to [style]. *)
