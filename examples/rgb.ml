(*
 * rgb.ml
 * ------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open Lt_style

lwt () =
  if Array.length Sys.argv <> 4 then begin
    lwt () = Lt_term.eprintlf "usage: %s <red> <blue> <green>" (Filename.basename Sys.executable_name) in
    exit 2
  end else begin
    let r = int_of_string Sys.argv.(1)
    and g = int_of_string Sys.argv.(2)
    and b = int_of_string Sys.argv.(3) in
    Lt_term.printls
      (Array.concat [
         Lt_text.stylise (Printf.sprintf "color with component (%d, %d, %d): " r g b) none;
         Lt_text.stylise "example" { none with foreground = Some(rgb r g b) };
       ])
  end
