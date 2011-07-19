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
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch; LTerm_key.control = true } when ch = CamomileLibraryDyn.Camomile.UChar.of_char 'c' ->
        (* Exit on Ctrl+C *)
        raise_lwt (Failure "interrupted")
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch } ->
        return ch
    | _ ->
        read_char term

let rec read_yes_no term =
  lwt () = LTerm.fprint term "Do you accept (y/n) ? " in
  lwt ch = read_char term >|= Zed_utf8.singleton in
  lwt () = LTerm.fprintl term ch in
  match ch with
    | "y" ->
        return true
    | "n" ->
        return false
    | _ ->
        lwt () = LTerm.fprintl term "Please enter 'y' or 'n'!" in
        read_yes_no term

lwt () =
  lwt term = Lazy.force LTerm.stdout in
  lwt mode = LTerm.enter_raw_mode term in
  try_lwt
    read_yes_no term >>= function
      | true ->
          LTerm.fprintl term "You accepted."
      | false ->
          LTerm.fprintl term "You did not accept."
  finally
    LTerm.leave_raw_mode term mode
