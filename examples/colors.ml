(*
 * colors.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open LTerm_style
open LTerm_text

let () =
  let rec loop i =
    if i = 16 then
      Lwt.return ()
    else
      LTerm.printls (eval [S(Printf.sprintf "color %d: " i);
                           B_fg(index i); S"foreground"; E_fg;
                           S" ";
                           B_bg(index i); S"background"; E_bg])
      >>= fun () ->
      loop (i + 1)
  in
  Lwt_main.run (loop 0)
