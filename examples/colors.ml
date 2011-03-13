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
open Lt_text

lwt () =
  for_lwt i = 0 to 15 do
    Lt_term.printls
      (Array.concat [
         stylise (Printf.sprintf "color %d: " i) none;
         stylise "foreground" { none with foreground = Some(index i) };
         stylise " " none;
         stylise "background" { none with background = Some(index i) };
       ])
  done
