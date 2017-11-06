(*
 * lambda_term_actions.ml
 * ----------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* List all available actions. *)

let print_action length (action, doc) =
  print_string action;
  for _ = String.length action to length do
    print_char ' '
  done;
  print_string ": ";
  print_string doc;
  print_char '\n'

let () =
  (* Collect actions. *)
  let edit_actions =
    ("insert(...)", "insert a character.")
    :: (List.map (fun (action, name) -> (name, Zed_edit.doc_of_action action)) Zed_edit.actions)
    @ (List.map (fun (action, name) -> (name, LTerm_edit.doc_of_action action)) LTerm_edit.actions)
  and read_line_actions =
    List.map (fun (action, name) -> (name, LTerm_read_line.doc_of_action action)) LTerm_read_line.actions
  in

  (* Search the longest line. *)
  let length = List.fold_left (fun acc (action, _doc) -> max (String.length action) acc) 0 edit_actions in
  let length = List.fold_left (fun acc (action, _doc) -> max (String.length action) acc) length read_line_actions in

  (* Print actions. *)
  print_string "General actions\n\
                ===============\n\n";
  List.iter (print_action length) edit_actions;
  print_string "\nRead-line actions\n\
                  =================\n\n";
  List.iter (print_action length) read_line_actions;

  flush stdout
