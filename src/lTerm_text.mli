(*
 * lTerm_text.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Styled text. *)

open CamomileLibrary

type t = (UChar.t * LTerm_style.t) array
    (** Type of a string with styles for each characters. *)

(** {6 Conversions} *)

val of_string : Zed_utf8.t -> t
  (** Creates a styled string from a string. All characters of the
      string have no style. *)

val to_string : t -> Zed_utf8.t
  (** Returns the string part of a styled string. *)

val of_rope : Zed_rope.t -> t
  (** Creates a styled string from a rope. *)

val to_rope : t -> Zed_rope.t
  (** Returns the string part of a styled string as a rope. *)

val stylise : string -> LTerm_style.t -> t
  (** [stylise string style] creates a styled string with all styles
      set to [style]. *)

(** {6 Markup strings} *)

(** Markup strings are used to conveniently define styled strings. *)

(** Type of an item in a markup string. *)
type item =
  | S of Zed_utf8.t
      (** A UTF-8 encoded string. *)
  | R of Zed_rope.t
      (** A rope. *)
  | B_bold of bool
      (** Begins bold mode. *)
  | E_bold
      (** Ends bold mode. *)
  | B_underline of bool
      (** Begins underlined mode. *)
  | E_underline
      (** Ends underlined mode. *)
  | B_blink of bool
      (** Begins blinking mode. *)
  | E_blink
      (** Ends blinking mode. *)
  | B_reverse of bool
      (** Begins reverse video mode. *)
  | E_reverse
      (** Ends reverse video mode. *)
  | B_fg of LTerm_style.color
      (** Begins foreground color. *)
  | E_fg
      (** Ends foreground color. *)
  | B_bg of LTerm_style.color
      (** Begins background color. *)
  | E_bg
      (** Ends background color. *)

type markup = item list
    (** Type of a markup string. *)

val eval : markup -> t
  (** [eval makrup] evaluates a markup strings as a styled string. *)
