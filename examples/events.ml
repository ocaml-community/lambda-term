(*
 * events.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Show events read from the terminal *)

open Lwt
open Lt_event

let rec loop term =
  lwt ev = term#read_event in
  lwt () = Lwt_io.printl (Lt_event.to_string ev) in
  match ev with
    | Lt_event.Key(_, Lt_key.Escape) ->
        return ()
    | _ ->
        loop term

lwt () =
  lwt () = Lwt_io.printl "press escape to exit" in
  lwt () = Lt_term.stdout#enter_raw_mode in
  try_lwt
    loop Lt_term.stdout
  finally
    Lt_term.stdout#leave_raw_mode
