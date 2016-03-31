open StdLabels

module Term = LTerm
module Draw = LTerm_draw
module Geom = LTerm_geom

module type Events = sig
  type LTerm_event.User.t += Quit
  type LTerm_event.User.t += Refresh
end

type 'a t =
  { term                       : Term.t
  ; draw                       : Draw.context -> 'a -> Geom.coord option
  ; saved_mode                 : Term.Mode.t
  ; saved_signals              : Term.Signals.t option
  ; mutable need_redraw        : bool
  ; mutable running            : bool
  ; mutable matrix_a           : Draw.Matrix.t
  ; mutable matrix_b           : Draw.Matrix.t
  ; mutable can_update         : bool
  ; events                     : (module Events)
  ; mutable size               : Geom.size
  }

let size t = t.size

let swap_resize_matrices t =
  let matrix_a = t.matrix_a and matrix_b = t.matrix_b in
  t.matrix_a <- Draw.Matrix.resize matrix_b t.size;
  t.matrix_b <- Draw.Matrix.resize matrix_a t.size;
;;

let do_draw t ~state =
  swap_resize_matrices t;

  let cursor = t.draw (Draw.Matrix.context t.matrix_a) state in

  (* Rendering. *)
  Term.hide_cursor t.term;
  let old = if t.can_update then Some t.matrix_b else None in
  Term.render t.term t.matrix_a ?old;
  t.can_update <- true;
  (match cursor with
   | Some coord ->
     LTerm.goto t.term coord;
     LTerm.show_cursor t.term
   | None ->
     ());

  LTerm.commit_sync t.term
;;

let create ?terminal ?(setup_signals=true) ?(mouse_events=Term.Mouse_events.Disabled) draw =
  let term =
    match terminal with
    | None -> Lazy.force Term.std
    | Some term -> term
  in
  let saved_signals =
    if setup_signals then
      Some (Term.Signals.get ())
    else
      None
  in
  if setup_signals then Term.Signals.(set handled);
  let mode = Term.mode term in
  Term.set_mode term (Term.Mode.set mode
                        ~echo:   false
                        ~raw:    true
                        ~mouse:  mouse_events
                        ~screen: Alternative);
  let size = Term.size term in
  { term
  ; size
  ; draw
  ; running = true
  ; saved_mode = mode
  ; matrix_a = Draw.Matrix.create size
  ; matrix_b = Draw.Matrix.create size
  ; can_update = false
  ; need_redraw = true
  ; saved_signals
  ; events = (module struct
               type LTerm_event.User.t += Quit
               type LTerm_event.User.t += Refresh
             end)
  }
;;

let send_event t ev = LTerm.send_event t.term ev

let quit t =
  if t.running then begin
    let (module E) = t.events in
    send_event t (User E.Quit)
  end
;;

let refresh t =
  if t.running then begin
    let (module E) = t.events in
    send_event t (User E.Refresh)
  end
;;

(*
   t.running <- false;
   Term.set_mode t.term t.saved_mode;
   Term.show_cursor t.term;
   Deferred.all_unit [t.drawer; Term.sync t.term]
   >>| fun () ->
   Option.iter t.saved_signals ~f:Term.Signals.set
*)

type action = Continue | Stop

let process_event t (ev : LTerm_event.t) =
  let (module E) = t.events in
  match ev with
  | Resume | Resize ->
    t.can_update <- false;
    t.size <- Term.size t.term;
    t.need_redraw <- true;
    Continue
  | User E.Quit ->
    Stop
  | User E.Refresh ->
    t.need_redraw <- true;
    Continue
  | _ ->
    Continue
;;

let idle t ~state =
  if t.need_redraw then begin
    t.need_redraw <- false;
    do_draw t ~state
  end
;;

let close t =
  t.running <- false;
  Term.set_mode t.term t.saved_mode;
  Term.show_cursor t.term;
  Term.commit_sync t.term;
  match t.saved_signals with
  | None -> ()
  | Some signals -> Term.Signals.set signals
;;

let rec run_sync t ~init:state ~f =
  match Term.poll_event t.term ~notifier:Term.Notifier.blocking with
  | Pending wait ->
    idle t ~state;
    Lazy.force wait;
    run_sync t ~init:state ~f
  | Ready ev ->
    match process_event t ev with
    | Stop     -> close t; state
    | Continue ->
      run_sync t ~f ~init:(f state ev)
;;
