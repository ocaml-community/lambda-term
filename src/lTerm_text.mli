(*
 * lTerm_text.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Styled text. *)

type point =
  { mutable char  : Uchar.t
  ; mutable style : LTerm_style.t }
[@@deriving sexp]

type t = point array [@@deriving sexp]
  (** Type of a string with styles for each characters. *)

val create : ?style:LTerm_style.t -> int -> t

(** {6 Conversions} *)

val of_string : ?style:LTerm_style.t -> Zed_utf8.t -> t
val to_string : t -> Zed_utf8.t

val of_string_maybe_invalid : ?style:LTerm_style.t -> string -> t
  (** Creates a styled string from a string. All characters of the
      string have no style. The string may contain invalid UTF-8
      sequences, in which case invalid bytes are escaped with the
      syntax [\NNN]. *)

val of_rope : ?style:LTerm_style.t -> Zed_rope.t -> t
val to_rope : t -> Zed_rope.t

(** {6 Parenthesis matching} *)

(** [stylise_parenthesis text ?paren pos style] searchs for parenthesis group starting or
    ending at [pos] and apply them the style [style]. [paren] is the list of parenthesis
    recognized. *)
val stylise_parenthesis
  :  t
  -> ?paren : (Uchar.t * Uchar.t) list
  -> pos:int
  -> LTerm_style.t
  -> unit

(** {6 Convenience} *)

val mk
  :  ?bold:LTerm_style.Switch.t
  -> ?underline:LTerm_style.Switch.t
  -> ?blink:LTerm_style.Switch.t
  -> ?reverse:LTerm_style.Switch.t
  -> ?fg:LTerm_style.Color.t
  -> ?bg:LTerm_style.Color.t
  -> string
  -> t

val mkf
  :  ?bold:LTerm_style.Switch.t
  -> ?underline:LTerm_style.Switch.t
  -> ?blink:LTerm_style.Switch.t
  -> ?reverse:LTerm_style.Switch.t
  -> ?fg:LTerm_style.Color.t
  -> ?bg:LTerm_style.Color.t
  -> ('a, unit, string, t) format4
  -> 'a

val kmkf
  :  (t -> 'b)
  -> ?bold:LTerm_style.Switch.t
  -> ?underline:LTerm_style.Switch.t
  -> ?blink:LTerm_style.Switch.t
  -> ?reverse:LTerm_style.Switch.t
  -> ?fg:LTerm_style.Color.t
  -> ?bg:LTerm_style.Color.t
  -> ('a, unit, string, 'b) format4
  -> 'a

(** {6 Printf style} *)

(** The format string accept the following tags separated by commas:

    - [color]: foreground color
    - [~color]: background color
    - [attr]: enable the given attributes (bold, underline, blink or reverse)

    For instance:

    {[
      tprintf "@{<red,bold>Warning!@}\n"
    ]}
*)
val tprintf : ('a, unit, t) format -> 'a

val ktprintf : (t -> 'a) -> ('b, unit, t, 'a) format4 -> 'b;;

(** {6 Ansi control sequence parsing} *)

(** Parses a string containing ansi control sequences. It return the styled string and the
    end style. *)
val parse_ansi : start_style:LTerm_style.t -> Zed_utf8.t -> t * LTerm_style.t
