(*
 * events.ml
 * ---------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Show events read from the terminal *)

open Lambda_term

let rec loop term =
  let ev = Terminal.read_event_sync term in
  Printf.printf "%s\n%!" (Event.to_string ev);
  match ev with
  | Key (N, Escape) -> ()
  | _ -> loop term

let () =
  print_endline "press escape to exit";
  let term = Lazy.force Terminal.std in
  Terminal.modify_mode term ~mouse:true ~raw:true ~echo:false;
  Terminal.commit_sync term;
  loop term;
  Terminal.close term
