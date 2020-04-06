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

module Query = struct
  (*
  (* left right *)
  | Left of int (* h *)
  | Right of int (* l *)
  | Line_FirstChar of int (* 0 *)
  | Line_FirstNonBlank of int (* ^ *)
  | Line_LastChar of int (* $ *)
  | Line_LastNonBlank of int (* g_ *)

  (* up down *)
  | Upward of int (* k *)
  | Downward of int (* j *)
  | GotoLine of int (* gg or G *)
  | GotoLine_first (* gg *)
  | GotoLine_last (* G *)

  (* word *)
  | Word of int (* w *)
  | WORD of int (* W *)
  | Word_end of int (* e *)
  | WORD_end of int (* E *)
  | Word_back of int (* b *)
  | WORD_back of int (* B *)
  | Word_back_end of int (* ge *)
  | WORD_back_end of int (* gE *)

  (* text object *)
  | Sentence_backword of int (* ( *)
  | Sentence_forward of int (* ) *)
  | Paragraph_backward of int (* { *)
  | Paragraph_forward of int (* } *)

  (* text object selection *)
  | Word_include of int (* aw *)
  | Word_inner of int (* iw *)
  | WORD_include of int (* aW *)
  | WORD_inner of int (* iW *)
  | Sentence_include of int (* as *)
  | Sentence_inner of int (* is *)
  | Paragraph_include of int (* ap *)
  | Paragraph_inner of int (* ip *)
  *)
  let left n ctx= (* h *)
    let n= max 0 n in
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit
    and line_idx= Zed_edit.line ctx in
    let line_len= Zed_lines.line_length lines line_idx in
    let column= Zed_edit.column ctx in
    let dest= (column - n) |> max 0 |> min line_len in
    let positon= Zed_edit.position ctx in
    let delta= column - dest in
    (positon - delta, delta)

  let right ?(newline=false) n ctx= (* l *)
    let n= max 0 n in
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit
    and line_idx= Zed_edit.line ctx in
    let line_len=
      max 0 @@
      let len= Zed_lines.line_length lines line_idx in
      if newline then len
      else len - 1
    in
    let column= Zed_edit.column ctx in
    let dest= (column + n) |> max 0 |> min line_len in
    let positon= Zed_edit.position ctx in
    let delta= dest - column in
    (positon + delta, delta)

  let line_FirstChar _n ctx= (* 0 *)
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit
    and line_idx= Zed_edit.line ctx in
    let start= Zed_lines.line_start lines line_idx in
    let column= Zed_edit.column ctx in
    (start, column - start)

  let line_FirstNonBlank _n ctx= (* ^ *)
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit
    and line_idx= Zed_edit.line ctx in
    let start= Zed_lines.line_start lines line_idx in
    let line_len= Zed_lines.line_length lines line_idx in
    start, line_len

  let line_LastChar ?(newline=false) n ctx= (* ^ *)
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit
    and line_idx= Zed_edit.line ctx in
    let count= Zed_lines.count lines in
    let line_idx=
      if n > 1 then
        min count @@
        line_idx + (n - 1)
      else
        line_idx
    in
    let stop= Zed_lines.line_stop lines line_idx in
    if newline then
      stop
    else
      max 0 (stop - 1)

  open CamomileLibraryDefault.Camomile

  let get_category ?(nl_as_sp=false) uchar=
    if uchar = Zed_utf8.extract "\n" 0 && nl_as_sp then
      `Zs
    else
      UCharInfo.general_category uchar

  let get_boundary multi_line ctx=
    let edit= Zed_edit.edit ctx in
    if multi_line then
      (0, Zed_rope.length (Zed_edit.text edit))
    else
      let lines= Zed_edit.lines edit
      and line_idx= Zed_edit.line ctx in
      (Zed_lines.line_start lines line_idx
      , Zed_lines.line_stop lines line_idx)

  let category_equal c1 c2=
    match c1, c2 with
    | `Ll, `Lu | `Lu, `Ll-> true
    | _-> c1 = c2

  let next_category ?(nl_as_sp=true) ~pos ~stop text=
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let zip= Zed_rope.Zip.make_f text pos in
    let rec skip_curr zip pos=
      if pos < stop then
        let zchar, zip= Zed_rope.Zip.next zip in
        let category= get_category ~nl_as_sp (Zed_char.core zchar) in
        if category_equal category start_category then
          skip_curr zip (pos + 1)
        else
          pos
      else
        pos
    in
    skip_curr zip pos

  let prev_category ?(nl_as_sp=true) ~pos ~start text=
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let zip= Zed_rope.Zip.make_f text pos in
    let rec skip_curr zip pos=
      if pos > start then
        let zchar, zip= Zed_rope.Zip.prev zip in
        let category= get_category ~nl_as_sp (Zed_char.core zchar) in
        if category_equal category start_category then
          skip_curr zip (pos - 1)
        else
          pos
      else
        pos
    in
    (skip_curr zip pos) - 1

  let next_word ?(multi_line=true) ~pos ~stop text=
    let nl_as_sp= multi_line in
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let next= next_category ~nl_as_sp ~pos ~stop text in
    if start_category = `Zs then
      next (* currently at a space, just skip spaces *)
    else
    if next < stop then
      (* skip potential subsequent spaces after skip current word*)
      let zchar= Zed_rope.get text next in
      let core= Zed_char.core zchar in
      if get_category ~nl_as_sp core = `Zs then
        (* skip subsequent spaces *)
        next_category ~nl_as_sp ~pos:next ~stop text
      else
        next
    else
      stop

  let prev_word ?(multi_line=true) ~pos ~start text=
    if pos <= start then start else
    let nl_as_sp= multi_line in
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    and before_start=
      let zchar= Zed_rope.get text (pos - 1) in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let prev= prev_category ~nl_as_sp ~pos ~start text in
    1 +
      if prev <= start then start else
      if category_equal start_category before_start then
        if start_category <> `Zs then
          prev
        else
          prev_category ~nl_as_sp ~pos:prev ~start text
      else if before_start = `Zs then
        let prev= prev_category ~nl_as_sp ~pos:prev ~start text in
        if prev <= start then prev else
        prev_category ~nl_as_sp ~pos:prev ~start text
      else
        prev_category ~nl_as_sp ~pos:prev ~start text

  let next_word_end ?(multi_line=true) ~pos ~stop text=
    let pos=
      if pos >= (stop-1) then stop else
      let nl_as_sp= multi_line in
      let start_category=
        let zchar= Zed_rope.get text pos in
        let core= Zed_char.core zchar in
        get_category ~nl_as_sp core
      and after_start=
        let zchar= Zed_rope.get text (pos + 1) in
        let core= Zed_char.core zchar in
        get_category ~nl_as_sp core
      in
      let next= next_category ~nl_as_sp ~pos ~stop text in
      if next >= stop then stop else
      if category_equal start_category after_start
        && start_category <> `Zs
      then
        next
      else
        let next= next_category ~nl_as_sp ~pos:next ~stop text in
        if next >= stop then stop else
        if start_category = `Zs then
          next
        else
          next_category ~nl_as_sp ~pos:next ~stop text
    in
    max 0 @@ pos - 1

  let prev_word_end ?(multi_line=true) ~pos ~start text=
    if pos <= start then start else
    let nl_as_sp= multi_line in
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let prev= prev_category ~nl_as_sp ~pos ~start text in
    if prev <= start then start else
    if start_category = `Zs then
      prev
    else
      prev_category ~nl_as_sp ~pos:prev ~start text
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

