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

let rec loop term =
  LTerm.read_event term
  >>= fun ev ->
  Lwt_io.printl (LTerm_event.to_string ev)
  >>= fun () ->
  match ev with
  | LTerm_event.Key{ LTerm_key.code = LTerm_key.Escape; _ } ->
    return ()
  | _ ->
    loop term

let main () =
  Lwt_io.printl "press escape to exit"
  >>= fun () ->
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  LTerm.enter_raw_mode term
  >>= fun mode ->
  Lwt.finalize (fun () -> loop term)
    (fun () ->
       LTerm.leave_raw_mode term mode
       >>= fun () ->
       LTerm.disable_mouse term)

let () = Lwt_main.run (main ())
