(*
 * read_yes_no.ml
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt

let rec read_char term =
  LTerm.read_event term >>= function
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch; LTerm_key.control = true ; _ } when ch = Uchar.of_char 'c' ->
        (* Exit on Ctrl+C *)
        Lwt.fail (Failure "interrupted")
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch ; _ } ->
        Lwt.return ch
    | _ ->
        read_char term

let rec read_yes_no term =
  LTerm.fprint term "Do you accept (y/n) ? "
  >>= fun () ->
  read_char term >|= Zed_utf8.singleton
  >>= fun ch ->
  LTerm.fprintl term ch
  >>= fun () ->
  match ch with
  | "y" ->
    return true
  | "n" ->
    return false
  | _ ->
    LTerm.fprintl term "Please enter 'y' or 'n'!"
    >>= fun () ->
    read_yes_no term

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enter_raw_mode term
  >>= fun mode ->
  Lwt.finalize (fun () ->
    read_yes_no term >>= function
    | true ->
      LTerm.fprintl term "You accepted."
    | false ->
      LTerm.fprintl term "You did not accept.")
    (fun () -> LTerm.leave_raw_mode term mode)

let () = Lwt_main.run (main ())
