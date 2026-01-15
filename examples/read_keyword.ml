(*
 * read_keyword.ml
 * ----------------
 * Copyright : (c) 2022, Shiwei Weng <weng@cs.jhu.edu>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Read a keyword and display it. *)

open Lwt_react
let ( >>= ) = Lwt.( >>= )

type move = Play | Pause

class read_keyword term = object(self)
  inherit [move] LTerm_read_line.read_keyword () as super
  inherit [move LTerm_read_line.read_keyword_result] LTerm_read_line.term term

  method! send_action = function
    | LTerm_read_line.Break ->
        (* Ignore Ctrl+C *)
        ()
    | action ->
        super#send_action action

  method! keywords = [
    Zed_string.of_utf8 "play", Play;
    Zed_string.of_utf8 "pause", Pause
  ]

  initializer
    self#set_prompt (S.const (LTerm_text.of_utf8 "Type a read_keyword: "))
end

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lazy.force LTerm.stdout
  >>= fun term ->
  (new read_keyword term)#run
  >>= function
    | Rk_value Play -> Lwt_io.printlf "You played it"
    | Rk_value Pause -> Lwt_io.printlf "You paused it"
    | Rk_error _ -> Lwt_io.printlf "You didn't type a keyword"

let () = Lwt_main.run (main ())
