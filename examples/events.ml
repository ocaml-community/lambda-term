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
open LTerm_event

let rec loop term =
  lwt ev = LTerm.read_event term in
  lwt () = Lwt_io.printl (LTerm_event.to_string ev) in
  match ev with
    | LTerm_event.Key{ LTerm_key.code = LTerm_key.Escape } ->
        return ()
    | _ ->
        loop term

lwt () =
  lwt () = Lwt_io.printl "press escape to exit" in
  lwt term = Lazy.force LTerm.stdout in
  lwt () = LTerm.enable_mouse term in
  lwt mode = LTerm.enter_raw_mode term in
  try_lwt
    loop term
  finally
    lwt () = LTerm.leave_raw_mode term mode in
    LTerm.disable_mouse term
