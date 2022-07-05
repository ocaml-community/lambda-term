(*
 * lTerm_widget_callbacks.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

let src = Logs.Src.create "lambda-term.widget-callbacks" ~doc:"logs LTerm_widget_callbacks module's events"
module Log = (val Logs.src_log src : Logs.LOG)

(* +-----------------------------------------------------------------+
   | Callbacks                                                       |
   +-----------------------------------------------------------------+ *)

type switch = { mutable switch_state : (unit -> unit) list option }
type 'a callbacks = 'a LTerm_dlist.t

let create () = LTerm_dlist.create ()

let register switch_opt seq f =
  match switch_opt with
    | None ->
        ignore (LTerm_dlist.add_l f seq)
    | Some switch ->
        match switch.switch_state with
          | Some l ->
              let node = LTerm_dlist.add_l f seq in
              switch.switch_state <- Some ((fun () -> LTerm_dlist.remove node) :: l)
          | None ->
              ()

let stop switch =
  match switch.switch_state with
    | Some l ->
        switch.switch_state <- None;
        List.iter (fun f -> f ()) l
    | None ->
        ()

let exec_callbacks seq x =
  LTerm_dlist.iter_l
    (fun f ->
       try
         f x
       with exn ->
         Log.err (fun m -> m "callback failed with %s" (Printexc.to_string exn));
         ())
    seq

let exec_filters seq x =
  LTerm_dlist.fold_l
    (fun f acc ->
       if acc then
         true
       else begin
         try
           f x
         with exn ->
           Log.err (fun m -> m "filter failed with %s" (Printexc.to_string exn));
           false
       end)
    seq false

