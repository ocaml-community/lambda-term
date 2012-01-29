(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             flag ["c"; "compile"; "use_lwt_unix_h"] & S[A"-package"; A"lwt"];

             rule "generation of color mappings"
               ~dep:"src/gen_color_mappings.byte"
               ~prod:"src/lTerm_color_mappings.ml"
               (fun _ _ ->
                  Cmd(S[P"src/gen_color_mappings.byte"; A"src/lTerm_color_mappings.ml"]))
         | _ ->
             ())
