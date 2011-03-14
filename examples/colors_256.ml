(*
 * colors_256.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open Lt_style
open Lt_text

lwt () =
  for_lwt i = 0 to 255 do
    Lt_term.printls (eval [S(Printf.sprintf "color %d: " i);
                           B_fg(index i); S"example"; E_fg])
  done
