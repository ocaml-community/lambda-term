(*
 * rgb.ml
 * ------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open LTerm_style
open LTerm_text

let main () =
  if Array.length Sys.argv <> 4 then begin
    LTerm.eprintlf "usage: %s <red> <blue> <green>"
      (Filename.basename Sys.executable_name)
    >>= fun () ->
    exit 2
  end else begin
    let r = int_of_string Sys.argv.(1)
    and g = int_of_string Sys.argv.(2)
    and b = int_of_string Sys.argv.(3) in
    LTerm.printls (eval [S(Printf.sprintf "color with component (%d, %d, %d): " r g b);
                           B_fg(rgb r g b); S"example"; E_fg])
  end

let () = Lwt_main.run (main ())
