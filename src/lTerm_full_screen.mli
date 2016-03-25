(** Full screen applications *)

open Async.Std

type t

val create
  :  LTerm.t
  -> ?setup_signals:bool
  -> ?enable_mouse:bool
  -> (LTerm_draw.context -> LTerm_geom.coord option)
  -> t

val quit : t -> unit Deferred.t

(** Queue a refresh request *)
val refresh : t -> unit

(** The next draw will be a full draw *)
val force_redraw : t -> unit

val size : t -> LTerm_geom.size

val wait : ?no_text:bool -> t -> LTerm_event.t Deferred.t
