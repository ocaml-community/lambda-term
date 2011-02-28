(*
 * lt_style.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Text styles *)

(** {6 Colors} *)

type color =
    private
  | Default
      (** The default color of the terminal. *)
  | Index of int
      (** A color given by its index. Most terminal have at least 8
          colors. *)
  | RGB of int * int * int
      (** A color given by its three component between 0 and 255. The
          closest color will be used. *)

val default : color
val index : int -> color
val rgb : int -> int -> int -> color
  (** [rgb r g b] raises [Invalid_argument] if one of [r], [g] or [b]
      is not in the range [0..255]. *)

(** {8 Standard colors} *)

val black : color
val red : color
val green : color
val yellow : color
val blue : color
val magenta : color
val cyan : color
val white : color

(** {8 Light colors} *)

val lblack : color
val lred : color
val lgreen : color
val lyellow : color
val lblue : color
val lmagenta : color
val lcyan : color
val lwhite : color

(** {6 Styled text} *)

type item =
  | String of string
      (** Some UTF-8 encoded text *)
  | Reset
      (** Resets all styles to default *)
  | Bold
      (** Put the text following this item in bold. This does nothing
          on windows. *)
  | Underlined
      (** Put the text following this item underlined. This does
          nothing on windows. *)
  | Blink
      (** Make the text following this item to blink. This does
          nothing on windows. *)
  | Inverse
      (** Inverse the foreground and background for the text following
          this item. This does nothing on windows. *)
  | Hidden
      (** Hide the text following this item. This does nothing on
          windows. *)
  | Foreground of color
      (** Set the foreground color. *)
  | Background of color
      (** Set the background color. *)

type text = item list
    (** Type of a styled text. For example [Foreground lred; String
        "foo"; Reset] means to put the string [foo] in light red then
        to reset all attributes *)

val format : ('a, unit, string, item) format4 -> 'a
  (** [format fmt] produces a [String str] item from the given format
      string and parameters. *)

val strip : text -> string
  (** [strip text] removes all styles from the given styled text. *)
