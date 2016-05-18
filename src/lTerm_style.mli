(*
 * lTerm_style.mli
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Text styles *)

(** {6 Colors} *)

module Color : sig
  type t = private int

  val to_string : t -> string
  val of_string : string -> t

  (** The default color of the terminal. *)
  val default : t

  (** A color given by its index between 0 and 255. Most terminal have at least 8
      colors. *)
  val index   : int -> t

  (** A color given by its three component between 0 and 255. The closest color will be
      used. *)
  val rgb     : int -> int -> int -> t

  (** The transparent color is used for styles that shouldn't override the color. For
      instance if drawing text and the background style is [transparent], the background
      will be the same as what was there before the text was drawn. *)
  val transparent : t

  (** [merge a b] is [b] if [a] is [transparent] and [a] otherwise *)
  val merge : t -> t -> t

  module Kind : sig
    type t =
      | Transparent
      | Default
      | Index
      | RGB
    [@@deriving sexp]
  end

  val kind : t -> Kind.t

  (** {8 Standard colors} *)

  val black   : t
  val red     : t
  val green   : t
  val yellow  : t
  val blue    : t
  val magenta : t
  val cyan    : t
  val white   : t

  (** {8 Light colors} *)

  val lblack   : t
  val lred     : t
  val lgreen   : t
  val lyellow  : t
  val lblue    : t
  val lmagenta : t
  val lcyan    : t
  val lwhite   : t

  val get_index : t -> LTerm_color_mappings.map -> int
end

(** {6 Styles} *)

module Switch : sig
  type t =
    | On
    | Off
    | Unset (** [Unset] has the same role as [Color.transparent] *)
  [@@deriving sexp]
end

(** Type of text styles. *)
type t

val make
  :  ?bold       : Switch.t (* default Unset *)
  -> ?underline  : Switch.t (* default Unset *)
  -> ?blink      : Switch.t (* default Unset *)
  -> ?reverse    : Switch.t (* default Unset *)
  -> ?foreground : Color.t  (* default transparent *)
  -> ?background : Color.t  (* default transparent *)
  -> unit
  -> t

val bold       : t -> Switch.t
val underline  : t -> Switch.t
val blink      : t -> Switch.t
val reverse    : t -> Switch.t
val foreground : t -> Color.t
val background : t -> Color.t

val set
  :  ?bold       : Switch.t (* default Unset *)
  -> ?underline  : Switch.t (* default Unset *)
  -> ?blink      : Switch.t (* default Unset *)
  -> ?reverse    : Switch.t (* default Unset *)
  -> ?foreground : Color.t  (* default transparent *)
  -> ?background : Color.t  (* default transparent *)
  -> t
  -> t

val set_bold       : t -> Switch.t -> t
val set_underline  : t -> Switch.t -> t
val set_blink      : t -> Switch.t -> t
val set_reverse    : t -> Switch.t -> t
val set_foreground : t -> Color.t -> t
val set_background : t -> Color.t -> t

(** Style with all fields set to [Switch.Off] or [Color.default]. *)
val default : t

(** Style with all fields set to [Switch.Unset] or [Color.transparent]. *)
val none : t

(** [merge t1 t2] is [t2] with all [Switch.Unset] or [Color.transparent] fields set to the
    one of [t1]. *)
val merge : t -> t -> t

(** [on_default t] is [merge default t]. *)
val on_default : t -> t

val equal : t -> t -> bool

module Switches : sig
  type t = private int

  val bold       : t -> Switch.t
  val underline  : t -> Switch.t
  val blink      : t -> Switch.t
  val reverse    : t -> Switch.t

  val merge : t -> t -> t

  val default : t
  val none    : t
end

val switches     : t -> Switches.t
val set_switches : t -> Switches.t -> t

val make' : switches:Switches.t -> foreground:Color.t -> background:Color.t -> t

module Mutable : sig
  type t

  val create
    :  ?bold       : Switch.t (* default Unset *)
    -> ?underline  : Switch.t (* default Unset *)
    -> ?blink      : Switch.t (* default Unset *)
    -> ?reverse    : Switch.t (* default Unset *)
    -> ?foreground : Color.t  (* default transparent *)
    -> ?background : Color.t  (* default transparent *)
    -> unit
    -> t

  val bold       : t -> Switch.t
  val underline  : t -> Switch.t
  val blink      : t -> Switch.t
  val reverse    : t -> Switch.t
  val foreground : t -> Color.t
  val background : t -> Color.t

  val set
    :  ?bold       : Switch.t (* default Unset *)
    -> ?underline  : Switch.t (* default Unset *)
    -> ?blink      : Switch.t (* default Unset *)
    -> ?reverse    : Switch.t (* default Unset *)
    -> ?foreground : Color.t  (* default transparent *)
    -> ?background : Color.t  (* default transparent *)
    -> t
    -> unit

  val set_bold       : t -> Switch.t -> unit
  val set_underline  : t -> Switch.t -> unit
  val set_blink      : t -> Switch.t -> unit
  val set_reverse    : t -> Switch.t -> unit
  val set_foreground : t -> Color.t -> unit
  val set_background : t -> Color.t -> unit

  val switches     : t -> Switches.t
  val set_switches : t -> Switches.t -> unit
end
