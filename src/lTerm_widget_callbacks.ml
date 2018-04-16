(*
 * lTerm_widget_callbacks.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

let section = Lwt_log.Section.make "lambda-term(widget_callbacks)"

(* +-----------------------------------------------------------------+
   | Callbacks                                                       |
   +-----------------------------------------------------------------+ *)

type switch = { mutable switch_state : (unit -> unit) list option }
type 'a callbacks = 'a Lwt_sequence.t

let create () = Lwt_sequence.create ()

let register switch_opt seq f =
  match switch_opt with
    | None ->
        ignore (Lwt_sequence.add_l f seq)
    | Some switch ->
        match switch.switch_state with
          | Some l ->
              let node = Lwt_sequence.add_l f seq in
              switch.switch_state <- Some ((fun () -> Lwt_sequence.remove node) :: l)
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
  Lwt_sequence.iter_l
    (fun f ->
       try
         f x
       with exn ->
         ignore (Lwt_log.error ~section ~exn "callback failed with"))
    seq

let exec_filters seq x =
  Lwt_sequence.fold_l
    (fun f acc ->
       if acc then
         true
       else begin
         try
           f x
         with exn ->
           ignore (Lwt_log.error ~section ~exn "filter failed with");
           false
       end)
    seq false

