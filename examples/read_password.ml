(*
 * read_password.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Read a password and display it. *)

open Lwt_react

let ( >>= ) = Lwt.( >>= )

class read_password term = object(self)
  inherit LTerm_read_line.read_password () as super
  inherit [Zed_string.t] LTerm_read_line.term term

  method! send_action = function
    | LTerm_read_line.Break ->
        (* Ignore Ctrl+C *)
        ()
    | action ->
        super#send_action action

  initializer
    self#set_prompt (S.const (LTerm_text.of_utf8 "Type a password: "))
end

let main () =
  LTerm_inputrc.load ()
  >>= fun () ->
  Lazy.force LTerm.stdout
  >>= fun term ->
  (new read_password term)#run
  >>= fun password ->
  Lwt_io.printlf "You typed %S" (Zed_string.to_utf8 password)

let () = Lwt_main.run (main ())
