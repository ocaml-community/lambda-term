(** Styled text printing *)

(** This module is for non-full screen printing. It uses classic async's writers. *)

open Async.Std

type t [@@deriving sexp]

val create : ?model:string -> Writer.t -> t

val writer : t -> Writer.t
val model  : t -> string
val colors : t -> int

val flushed : t -> unit Deferred.t

val stdout : t Lazy.t
val stderr : t Lazy.t

val  print :      LTerm_text.t -> unit
val eprint :      LTerm_text.t -> unit
val fprint : t -> LTerm_text.t -> unit

val  printf :      ('a, unit, LTerm_text.t, unit) format4 -> 'a
val eprintf :      ('a, unit, LTerm_text.t, unit) format4 -> 'a
val fprintf : t -> ('a, unit, LTerm_text.t, unit) format4 -> 'a

(** Only usefull if printing using [Writer.*] functions as the functions in this module
    reset styles before and after printing. *)
val set_style : t -> LTerm_style.t -> unit
