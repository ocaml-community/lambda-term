(*
 * lTerm_vi.ml
 * ------------
 * Copyright : (c) 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

module Concurrent = struct
  module Thread= struct
      include Lwt
      let run= Lwt_unix.run [@@ocaml.warning "-3"]
      let sleep= Lwt_unix.sleep
    end
  module MsgBox= struct
      include Lwt_mvar
      let get= Lwt_mvar.take
      let create= Lwt_mvar.create_empty
    end
end

module Vi = Mew_vi.Core.Make (Concurrent)
include Vi

let of_lterm_code : LTerm_key.code -> Mew_vi.Key.code= function
  | Char chr-> Char (Zed_utf8.escaped_char chr)
  | Enter     -> Enter
  | Escape    -> Escape
  | Tab       -> Tab
  | Up        -> Up
  | Down      -> Down
  | Left      -> Left
  | Right     -> Right
  | F1        -> F1
  | F2        -> F2
  | F3        -> F3
  | F4        -> F4
  | F5        -> F5
  | F6        -> F6
  | F7        -> F7
  | F8        -> F8
  | F9        -> F9
  | F10       -> F10
  | F11       -> F11
  | F12       -> F12
  | Next_page -> Next_page
  | Prev_page -> Prev_page
  | Home      -> Home
  | End       -> End
  | Insert    -> Insert
  | Delete    -> Delete
  | Backspace -> Backspace

let of_vi_code : Mew_vi.Key.code -> LTerm_key.code= function
  | Char bin -> Char (Zed_utf8.extract bin 0)
  | Enter     -> Enter
  | Escape    -> Escape
  | Tab       -> Tab
  | Up        -> Up
  | Down      -> Down
  | Left      -> Left
  | Right     -> Right
  | F1        -> F1
  | F2        -> F2
  | F3        -> F3
  | F4        -> F4
  | F5        -> F5
  | F6        -> F6
  | F7        -> F7
  | F8        -> F8
  | F9        -> F9
  | F10       -> F10
  | F11       -> F11
  | F12       -> F12
  | Next_page -> Next_page
  | Prev_page -> Prev_page
  | Home      -> Home
  | End       -> End
  | Insert    -> Insert
  | Delete    -> Delete
  | Backspace -> Backspace

let of_lterm_key lterm_key=
  {
    Mew_vi.Key.control= lterm_key.LTerm_key.control;
    meta= lterm_key.meta;
    shift= lterm_key.shift;
    code= of_lterm_code lterm_key.code;
  }

let of_vi_key vi_key=
  {
    LTerm_key.control= vi_key.Mew_vi.Key.control;
    meta= vi_key.meta;
    shift= vi_key.shift;
    code= of_vi_code vi_key.code;
  }

