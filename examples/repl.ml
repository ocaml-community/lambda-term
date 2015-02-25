(*
 * repl.ml
 * --------
 * Copyright : (c) 2015, Martin DeMello <mdemello@google.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Add a REPL to an existing interpreter *)

open React
open Lwt
open LTerm_text

(* +-----------------------------------------------------------------+
   | Interpreter                                                     |
   +-----------------------------------------------------------------+ *)

(* A simple model of an interpreter. It maintains some state, and exposes a function
 *   eval : state -> input -> (new_state, output) *)
module Interpreter = struct
  type state = { n : int }

  let eval state s =
    let out = "evaluated " ^ s in
    let new_state = { n = state.n + 1 } in
    (new_state, out)
end

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

(* Create a prompt based on the current interpreter state *)
let make_prompt state =
  let prompt = Printf.sprintf "In  [%d]: " state.Interpreter.n in
  eval [ S prompt ]

(* Format the interpreter output for REPL display *)
let make_output state out =
  let output = Printf.sprintf "Out [%d]: %s" state.Interpreter.n out in
  eval [ S output ]

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (S.const (make_prompt state))
end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec loop term history state =
  match_lwt
    try_lwt
      let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
      lwt command = rl#run in
      return (Some command)
    with Sys.Break ->
      return None
  with
  | Some command ->
      let state, out = Interpreter.eval state command in
      lwt () = LTerm.fprintls term (make_output state out) in
      LTerm_history.add history command;
      loop term history state
  | None ->
      loop term history state

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  lwt () = LTerm_inputrc.load () in
  try_lwt
    let state = { Interpreter.n = 1 } in
    lwt term = Lazy.force LTerm.stdout in
    loop term (LTerm_history.create []) state
  with LTerm_read_line.Interrupt ->
    return ()
