(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2016, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | After_rules ->
              if Sys.file_exists ".git" then
                tag_any ["warn(@a-4-40-41-42-44-50)"]
              else
                tag_any ["warn(-40)"]
         | _ -> ())
