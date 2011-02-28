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
    lwt () = Lt_term.eprintls [Foreground lred; format "usage: %s <red> <blue> <green>" (Filename.basename Sys.executable_name); Reset] in
    exit 2
  end else begin
    let r = int_of_string Sys.argv.(1)
    and g = int_of_string Sys.argv.(2)
    and b = int_of_string Sys.argv.(3) in
    Lt_term.printls [format "color with component (%d, %d, %d): " r g b; Foreground(rgb r g b); String "example"; Reset]
  end
