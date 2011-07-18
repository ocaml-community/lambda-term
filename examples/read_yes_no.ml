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
  Lt_term.read_event term >>= function
    | Lt_event.Key { Lt_key.code = Lt_key.Char ch; Lt_key.control = true } when ch = CamomileLibraryDyn.Camomile.UChar.of_char 'c' ->
        (* Exit on Ctrl+C *)
        raise_lwt (Failure "interrupted")
    | Lt_event.Key { Lt_key.code = Lt_key.Char ch } ->
        return ch
    | _ ->
        read_char term

let rec read_yes_no term =
  lwt () = Lt_term.fprint term "Do you accept (y/n) ? " in
  lwt ch = read_char term >|= Zed_utf8.singleton in
  lwt () = Lt_term.fprintl term ch in
  match ch with
    | "y" ->
        return true
    | "n" ->
        return false
    | _ ->
        lwt () = Lt_term.fprintl term "Please enter 'y' or 'n'!" in
        read_yes_no term

lwt () =
  lwt term = Lazy.force Lt_term.stdout in
  lwt mode = Lt_term.enter_raw_mode term in
  try_lwt
    read_yes_no term >>= function
      | true ->
          Lt_term.fprintl term "You accepted."
      | false ->
          Lt_term.fprintl term "You did not accept."
  finally
    Lt_term.leave_raw_mode term mode
