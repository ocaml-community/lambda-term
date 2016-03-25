open! Core.Std
open Async.Std

module Term = LTerm
module Draw = LTerm_draw
module Geom = LTerm_geom

type t =
  { term                       : Term.t
  ; draw                       : Draw.context -> Geom.coord option
  ; saved_mode                 : Term.Mode.t
  ; saved_signals              : Term.Signals.t option
  ; mutable running            : bool
  ; mutable matrix_a           : Draw.matrix
  ; mutable matrix_b           : Draw.matrix
  ; mutable context_a          : Draw.context
  ; mutable context_b          : Draw.context
  ; mutable drawer             : unit Deferred.t
  ; mutable drawing_generation : int
  ; mutable drawing            : bool
  } [@@deriving fields]

let size t = Term.size t.term

module Drawing = struct
  let swap_matrices t =
    let matrix_a = t.matrix_a and matrix_b = t.matrix_b in
    t.matrix_a <- matrix_b;
    t.matrix_b <- matrix_a;
    let context_a = t.context_a and context_b = t.context_b in
    t.context_a <- context_b;
    t.context_b <- context_a;
  ;;

  let rec draw_update t =
    match t.running with
    | false ->
      t.drawing <- false;
      Deferred.unit
    | true ->
      let rendered_generation = t.drawing_generation in

      (* Swap the two matrices. *)
      swap_matrices t;

      (* Allocate the first matrix if needed. *)
      if t.matrix_a = [||] then begin
        let size = size t in
        let matrix = Draw.make_matrix size in
        t.matrix_a <- matrix;
        t.context_a <- Draw.context matrix size;
      end;

      let cursor = t.draw t.context_a in

      (* Rendering. *)
      LTerm.hide_cursor t.term;
      LTerm.render t.term ~old:t.matrix_b t.matrix_a;
      (match cursor with
       | Some coord ->
         LTerm.goto t.term coord;
         LTerm.show_cursor t.term
       | None ->
         ());

      LTerm.sync t.term
      >>= fun () ->

      if t.drawing_generation <> rendered_generation then
        draw_update t
      else begin
        t.drawing <- false;
        Deferred.unit
      end
  ;;

  let queue_draw t =
    if t.running then begin
      t.drawing_generation <- t.drawing_generation + 1;
      if not t.drawing then begin
        t.drawing <- true;
        t.drawer <- Deferred.unit >>= (fun () -> draw_update t)
      end;
    end
  ;;
end

let refresh = Drawing.queue_draw

let dummy_size = { Geom. rows = 0; cols = 0 }
let dummy_matrix = [||]
let dummy_context = Draw.context dummy_matrix dummy_size

let force_redraw t =
  t.matrix_a  <- dummy_matrix;
  t.context_a <- dummy_context;
;;

let create term ?(setup_signals=true) ?(enable_mouse=false) draw =
  let saved_signals =
    if setup_signals then
      Some (Term.Signals.get ())
    else
      None
  in
  if setup_signals then Term.Signals.(set handled);
  let mode = Term.mode term in
  Term.set_mode term { mode with
                       echo   = false
                     ; raw    = true
                     ; mouse  = if enable_mouse then true else mode.mouse
                     ; screen = Alternative
                     };
  let t =
    { term
    ; draw
    ; running = true
    ; saved_mode = mode
    ; matrix_a = dummy_matrix
    ; matrix_b = dummy_matrix
    ; context_a = dummy_context
    ; context_b = dummy_context
    ; drawer = Deferred.unit
    ; drawing = false
    ; drawing_generation = 0
    ; saved_signals
    }
  in
  refresh t;
  t
;;

let quit t =
  if t.running then begin
    t.running <- false;
    Term.set_mode t.term t.saved_mode;
    Term.show_cursor t.term;
    Deferred.all_unit [t.drawer; Term.sync t.term]
    >>| fun () ->
    Option.iter t.saved_signals ~f:Term.Signals.set
  end else
    failwith "LTerm_full_screen.quit already called"
;;

let wait ?no_text t =
  LTerm.read_event ?no_text t.term
  >>| function
  | (Resume | Resize) as ev ->
    t.matrix_a  <- dummy_matrix;
    t.matrix_b  <- dummy_matrix;
    t.context_a <- dummy_context;
    t.context_b <- dummy_context;
    refresh t;
    ev
  | ev ->
    ev
;;
