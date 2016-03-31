(** Full screen applications *)

type 'state t

val create
  :  ?terminal:LTerm.t
  -> ?setup_signals:bool
  -> ?mouse_events:LTerm.Mouse_events.t
  -> (LTerm_draw.context -> 'state -> LTerm_geom.coord option)
  -> 'state t

val quit : _ t -> unit

(** Queue a refresh request. *)
val refresh : _ t -> unit

val size : _ t -> LTerm_geom.size

val run_sync : 'a t -> init:'a -> f:('a -> LTerm_event.t -> 'a) -> 'a

val send_event : _ t -> LTerm_event.t -> unit
