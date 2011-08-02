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
open LTerm_style

class read_password term = object(self)
  inherit LTerm_read_line.read_password ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  initializer
    self#set_prompt (S.const (LTerm_text.of_string "Type a password: "))
end

lwt () =
  lwt () = LTerm_inputrc.load () in
  lwt term = Lazy.force LTerm.stdout in
  lwt password = (new read_password term)#run in
  Lwt_io.printlf "You typed %S" password
