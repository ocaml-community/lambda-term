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
  lwt ev = Lt_term.read_event term in
  lwt () = Lwt_io.printl (Lt_event.to_string ev) in
  match ev with
    | Lt_event.Key{ Lt_key.code = Lt_key.Escape } ->
        return ()
    | _ ->
        loop term

lwt () =
  lwt () = Lwt_io.printl "press escape to exit" in
  lwt term = Lazy.force Lt_term.stdout in
  lwt () = Lt_term.enable_mouse term in
  lwt mode = Lt_term.enter_raw_mode term in
  try_lwt
    loop term
  finally
    lwt () = Lt_term.leave_raw_mode term mode in
    Lt_term.disable_mouse term
