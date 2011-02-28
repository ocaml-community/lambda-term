(*
 * lt_draw.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type point = {
  mutable char : int;
  mutable bold : bool;
  mutable underlined : bool;
  mutable blink : bool;
  mutable foreground : Lt_style.color;
  mutable background : Lt_style.color;
}

type matrix = point array array

let make_matrix size =
  Array.init
    size.Lt_types.lines
    (fun _ ->
       Array.init
         size.Lt_types.columns
         (fun _ -> {
            char = 32;
            bold = false;
            underlined = false;
            blink = false;
            foreground = Lt_style.default;
            background = Lt_style.default;
          }))
