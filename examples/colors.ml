(*
 * colors.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open Lt_style

lwt () =
  lwt () =
    for_lwt i = 0 to 7 do
      Lt_term.printls [format "color %d: " i;
                       Foreground(Index i); String "foreground"; Reset; String " ";
                       Background(Index i); String "background"; Reset]
    done
  in
  for_lwt i = 0 to 7 do
    Lt_term.printls [format "light color %d: " i;
                     Foreground(Light i); String "foreground"; Reset; String " ";
                     Background(Light i); String "background"; Reset]
  done
