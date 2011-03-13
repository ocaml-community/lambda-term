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
open Lt_style

class read_password term = object(self)
  inherit Lt_read_line.read_password ()
  inherit [Zed_utf8.t] Lt_read_line.term term

  initializer
    self#set_prompt (S.const (Lt_text.of_string "Type a password: "))
end

lwt () =
  lwt term = Lazy.force Lt_term.stdout in
  lwt password = (new read_password term)#run in
  Lwt_io.printlf "You typed %S" password
