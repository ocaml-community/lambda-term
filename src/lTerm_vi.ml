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

  let is_space= function
    | `Cc | `Zs | `Zl | `Zp | `Mn -> true
    | _-> false

  let is_not_space c= not (is_space c)

  let category_equal c1 c2=
    match c1, c2 with
    | `Ll, `Lu | `Lu, `Ll-> true
    | _-> c1 = c2

  let category_equal_blank c1 c2=
    let b1= is_space c1
    and b2= is_space c2 in
    b1 = b2

  let next_category
      ?(nl_as_sp=true)
      ?(is_equal=category_equal)
      ~pos
      ~stop
      text
    =
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
        if is_equal category start_category then
          skip_curr zip (pos + 1)
        else
          pos
      else
        pos
    in
    skip_curr zip pos

  let prev_category
      ?(nl_as_sp=true)
      ?(is_equal=category_equal)
      ~pos
      ~start
      text
    =
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
        if is_equal category start_category then
          skip_curr zip (pos - 1)
        else
          pos
      else
        pos
    in
    (skip_curr zip pos) - 1

  let next_word' ?(multi_line=true) ~next_category ~pos ~stop text=
    let nl_as_sp= multi_line in
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let next= next_category ~nl_as_sp ~pos ~stop text in
    if is_space start_category then
      next (* currently at a space, just skip spaces *)
    else
    if next < stop then
      (* skip potential subsequent spaces after skip current word*)
      let zchar= Zed_rope.get text next in
      let core= Zed_char.core zchar in
      if is_space (get_category ~nl_as_sp core) then
        (* skip subsequent spaces *)
        next_category ~nl_as_sp ~pos:next ~stop text
      else
        next
    else
      stop

  let next_word ?multi_line ~pos ~stop text=
    let next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal in
    next_word' ?multi_line ~next_category ~pos ~stop text

  let next_WORD ?multi_line ~pos ~stop text=
    let next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal_blank in
    next_word' ?multi_line ~next_category ~pos ~stop text

  let prev_word' ?(multi_line=true) ~prev_category ~pos ~start text=
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
      if category_equal start_category before_start then
        if is_space start_category then
          prev
        else
          prev_category ~nl_as_sp ~pos:prev ~start text
      else if is_space before_start then
        let prev= prev_category ~nl_as_sp ~pos:prev ~start text in
        if prev <= start then prev else
        prev_category ~nl_as_sp ~pos:prev ~start text
      else
        prev_category ~nl_as_sp ~pos:prev ~start text

  let prev_word ?multi_line ~pos ~start text=
    let prev_category ~nl_as_sp=
      prev_category ~nl_as_sp ~is_equal:category_equal in
    prev_word' ?multi_line ~prev_category ~pos ~start text

  let prev_WORD ?multi_line ~pos ~start text=
    let prev_category ~nl_as_sp=
      prev_category ~nl_as_sp ~is_equal:category_equal_blank in
    prev_word' ?multi_line ~prev_category ~pos ~start text

  let next_word_end' ?(multi_line=true) ~next_category ~pos ~stop text=
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
        && is_not_space start_category
      then
        next
      else
        let next= next_category ~nl_as_sp ~pos:next ~stop text in
        if next >= stop then stop else
        if is_space start_category then
          next
        else
          next_category ~nl_as_sp ~pos:next ~stop text
    in
    max 0 @@ pos - 1

  let next_word_end ?multi_line ~pos ~stop text=
    let next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal in
    next_word_end' ?multi_line ~next_category ~pos ~stop text

  let next_WORD_end ?multi_line ~pos ~stop text=
    let next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal_blank in
    next_word_end' ?multi_line ~next_category ~pos ~stop text

  let prev_word_end' ?(multi_line=true) ~prev_category ~pos ~start text=
    if pos <= start then start else
    let nl_as_sp= multi_line in
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let prev= prev_category ~nl_as_sp ~pos ~start text in
    if prev <= start then start else
    if is_space start_category then
      prev
    else
      let before_category=
        let zchar= Zed_rope.get text prev in
        let core= Zed_char.core zchar in
        get_category ~nl_as_sp core
      in
      if is_space before_category then
        prev_category ~nl_as_sp ~pos:prev ~start text
      else
        prev

  let prev_word_end ?multi_line ~pos ~start text=
    let prev_category ~nl_as_sp=
      prev_category ~nl_as_sp ~is_equal:category_equal in
    prev_word_end' ?multi_line ~prev_category ~pos ~start text

  let prev_WORD_end ?multi_line ~pos ~start text=
    let prev_category ~nl_as_sp=
      prev_category ~nl_as_sp ~is_equal:category_equal_blank in
    prev_word_end' ?multi_line ~prev_category ~pos ~start text

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

open LTerm_read_line_base
open Lwt

let perform ctx exec result action=
  let list_make elm n=
    let rec create acc n=
      if n > 0 then
        create (elm::acc) (n-1)
      else
        acc
    in
    create [] n
  in
  let list_dup elm n=
    let rec create acc n=
      if n > 0 then
        create (elm::acc) (n-1)
      else
        acc
    in
    create [] n |> List.concat
  in
  let delete ?boundary start len=
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let eot= Zed_rope.length text in
    let boundary_start, boundary_end=
      match boundary with
      | Some (b, e)-> b, e
      | None-> 0, eot
    in
    let _ori_start, _ori_len, _ori_stop= start, len, start+len in
    let start, len, stop=
      let start= max boundary_start _ori_start in
      let stop= min boundary_end _ori_stop in
      let len= stop - start in
      start, len, stop
    in
    if len > 0 then
      let end_pos=
        if stop >= eot then
          let end_pos= max 0 @@ start - 1 in
          if eot > 0 then
            if (=)
              (Zed_char.core (Zed_rope.get text end_pos))
              (Zed_utf8.extract "\n" 0)
            then
              max 0 @@ end_pos - 1
            else
              end_pos
          else end_pos
        else
          if (=)
            (Zed_char.core (Zed_rope.get text stop))
            (Zed_utf8.extract "\n" 0)
          then
            max 0 @@ start - 1
          else
            start
      in
      exec [
        Edit (Zed (Zed_edit.Goto start));
        Edit (Zed (Zed_edit.Kill_next_chars len));
        Edit (Zed (Zed_edit.Goto end_pos))
        ]
    else
      return (ContinueLoop [])
  in
  let change ?boundary start len=
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let eot= Zed_rope.length text in
    let boundary_start, boundary_end=
      match boundary with
      | Some (b, e)-> b, e
      | None-> 0, eot
    in
    let _ori_start, _ori_len, _ori_stop= start, len, start+len in
    let start, len, _stop=
      let start= max boundary_start _ori_start in
      let stop= min boundary_end _ori_stop in
      let len= stop - start in
      start, len, stop
    in
    if len > 0 then
      exec [
        Edit (Zed (Zed_edit.Goto start));
        Edit (Zed (Zed_edit.Kill_next_chars len));
        Edit (Zed (Zed_edit.Goto start))
        ]
    else
      return (ContinueLoop [])
  in
  let setup_pos ()=
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let pos= Zed_edit.position ctx in
    let text_len= Zed_rope.length text in
    (if text_len > 0 then
      let step= if pos >= text_len then pos - 1 else pos in
      let step=
        if (=)
          (Zed_char.core (Zed_rope.get text step))
          (Zed_utf8.extract "\n" 0)
        then max 0 @@ step - 1
        else step
      in
      exec [Edit (Zed (Zed_edit.Goto step))]
    else
      exec [Edit (Zed (Zed_edit.Goto_bol))]) >>=
    (function
      | Result r-> Lwt_mvar.put result r
      | ContinueLoop _-> return ())
  in
  match action with
  | Vi_action.Insert (insert, count)->
    (match insert with
    | Newline_below _s->
      exec @@
        (Edit (Zed (Zed_edit.Goto_eol)))::
        (list_make (Edit (Zed (Zed_edit.Newline))) count)
      >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Newline_above _s->
      exec @@
        list_dup [
          Edit (Zed (Zed_edit.Goto_bol));
          Edit (Zed (Zed_edit.Newline));
          Edit (Zed (Zed_edit.Prev_line));
        ]
        count
      >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | _-> return ())
  | Motion (motion, count)->
    (match motion with
    | Left n->
      let rec left n=
        if n > 0 then
          let pos, _delta= Query.left n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> left (n-1))
        else
          return ()
      in
      left (count*n)
    | Right n->
      let rec right n=
        if n > 0 then
          let pos, _delta= Query.right n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> right (n-1))
        else
          return ()
      in
      right (count*n)
    | Right_nl n->
      let newline= true in
      let rec right n=
        if n > 0 then
          let pos, _delta= Query.right ~newline n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> right (n-1))
        else
          return ()
      in
      right (count*n)
    | Upward n->
      exec
        (list_make
          (Edit (Zed Zed_edit.Prev_line))
          (count*n)) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Downward n->
      exec
        (list_make
          (Edit (Zed Zed_edit.Next_line))
          (count*n)) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word n=
        let pos= Zed_edit.position ctx in
        if n > 0 && pos < stop then
          let next=
            min (stop - 1) (Query.next_word ~pos ~stop text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto next))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> next_word (n-1))
        else
          return ()
      in
      next_word (count*n)
    | WORD n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word n=
        let pos= Zed_edit.position ctx in
        if n > 0 && pos < stop then
          let next=
            min (stop - 1) (Query.next_WORD ~pos ~stop text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto next))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> next_word (n-1))
        else
          return ()
      in
      next_word (count*n)
    | Word_back n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let rec prev_word n=
        let pos= min (stop - 1) (Zed_edit.position ctx) in
        if n > 0 && pos > start then
          let prev=
            max start (Query.prev_word ~pos ~start text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto prev))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> prev_word (n-1))
        else
          return ()
      in
      prev_word (count*n)
    | WORD_back n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let rec prev_word n=
        let pos= min (stop - 1) (Zed_edit.position ctx) in
        if n > 0 && pos > start then
          let prev=
            max start (Query.prev_WORD ~pos ~start text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto prev))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> prev_word (n-1))
        else
          return ()
      in
      prev_word (count*n)
    | Word_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word n=
        let pos= Zed_edit.position ctx in
        if n > 0 && pos < stop then
          let next= min
            (stop - 1)
            (Query.next_word_end ~pos ~stop text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto next))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> next_word (n-1))
        else
          return ()
      in
      next_word (count*n)
    | WORD_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word n=
        let pos= Zed_edit.position ctx in
        if n > 0 && pos < stop then
          let next= min
            (stop - 1)
            (Query.next_WORD_end ~pos ~stop text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto next))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> next_word (n-1))
        else
          return ()
      in
      next_word (count*n)
    | Word_back_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let rec prev_word n=
        let pos= min (stop - 1) (Zed_edit.position ctx) in
        if n > 0 && pos > start then
          let prev=
            max start (Query.prev_word_end ~pos ~start text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto prev))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> prev_word (n-1))
        else
          return ()
      in
      prev_word (count*n)
    | WORD_back_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let rec prev_word n=
        let pos= min (stop - 1) (Zed_edit.position ctx) in
        if n > 0 && pos > start then
          let prev=
            max start (Query.prev_WORD_end ~pos ~start text)
          in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto prev))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> prev_word (n-1))
        else
          return ()
      in
      prev_word (count*n)
    | Line_FirstChar n->
      exec
        (list_make
          (Edit (Zed Zed_edit.Goto_bol))
          (count*n)) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_FirstNonBlank _n->
      let edit= Zed_edit.edit ctx in
      let start, stop= Query.get_boundary false ctx in
      let text= Zed_edit.text edit in
      let chr_fst= (Zed_char.core (Zed_rope.get text start)) in
      let nonblank=
        if Query.(is_space (get_category chr_fst)) then
          Query.next_word ~pos:start ~stop text
        else
          start
      in
      exec
        (list_make
          (Edit (Zed (Zed_edit.Goto nonblank)))
          1) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_LastChar n->
      let rec lastChar n=
        if n > 0 then
          let pos= Query.line_LastChar n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> lastChar (n-1))
        else
          return ()
      in
      lastChar (count*n)
    | Line_LastChar_nl n->
      let newline= true in
      let rec lastChar n=
        if n > 0 then
          let pos= Query.line_LastChar ~newline n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result r-> Lwt_mvar.put result r
            | ContinueLoop _-> lastChar (n-1))
        else
          return ()
      in
      lastChar (count*n)
    | GotoLine_first->
      exec [Edit (Zed (Zed_edit.Goto_bot))] >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | GotoLine_last->
      exec [
        Edit (Zed (Zed_edit.Goto_eot));
        Edit (Zed (Zed_edit.Prev_char))
        ] >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | _-> return ())
  | Delete (motion, count)->
    (match motion with
    | Left n->
      let pos, delta= Query.left (count*n) ctx in
      delete pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Right n->
      let newline=true in
      let pos, delta= Query.right ~newline (count*n) ctx in
      let pos= pos - delta in
      delete pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Right_nl n->
      let newline= true in
      let pos, delta= Query.right ~newline (count*n) ctx in
      let pos= pos - delta in
      exec [
        Edit (Zed (Zed_edit.Goto pos));
        Edit (Zed (Zed_edit.Kill_next_chars delta));
        ] >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Upward n->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let dest= max 0 (line - count * n) in
      let line_delta = line - dest in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines dest
        and pos_end= Zed_lines.line_stop lines line in
        let pos_delta= pos_end - pos_start in
        delete pos_start pos_delta >>=
        (function
          | Result r-> Lwt_mvar.put result r
          | ContinueLoop _-> return ())
      else
        return ()
    | Downward n->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count * n) in
      let line_delta = dest - line in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines line
        and pos_end= Zed_lines.line_stop lines dest in
        let pos_end=
          if dest < line_count
          then pos_end + 1
          else pos_end in
        let pos_delta= pos_end - pos_start in
        delete pos_start pos_delta >>=
        (function
          | Result r-> Lwt_mvar.put result r
          | ContinueLoop _-> return ())
      else
        return ()
    | Line->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count - 1) in
      let pos_start= Zed_lines.line_start lines line
      and pos_end= Zed_lines.line_stop lines dest in
      let pos_end=
        if dest < line_count
        then pos_end + 1
        else pos_end in
      let pos_delta= pos_end - pos_start in
      delete pos_start pos_delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word n->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos (count*n) in
      let delta= next_pos - pos in
      delete pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | WORD n->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_WORD ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos (count*n) in
      let delta= next_pos - pos in
      delete pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word_back n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let pos= min (stop - 1) (Zed_edit.position ctx) in
      let rec prev_word pos n=
        if n > 0 && pos > start then
          let prev= max
            start
            (Query.prev_word ~pos ~start text)
          in
          prev_word prev (n-1)
        else
          pos
      in
      let prev_pos= prev_word pos (count*n) in
      let delta= pos - prev_pos in
      delete prev_pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | WORD_back n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let pos= min (stop - 1) (Zed_edit.position ctx) in
      let rec prev_word pos n=
        if n > 0 && pos > start then
          let prev= max
            start
            (Query.prev_WORD ~pos ~start text)
          in
          prev_word prev (n-1)
        else
          pos
      in
      let prev_pos= prev_word pos (count*n) in
      let delta= pos - prev_pos in
      delete prev_pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let pos= Zed_edit.position ctx in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word_end ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos= next_word pos (count*n) in
      let delta= next_pos + 1 - pos in
      delete pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | WORD_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let pos= Zed_edit.position ctx in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_WORD_end ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos= next_word pos (count*n) in
      let delta= next_pos + 1 - pos in
      delete pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word_back_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return () else
      let start, stop= Query.get_boundary true ctx in
      let pos= min (stop - 1) (Zed_edit.position ctx) in
      let rec prev_word pos n=
        if n > 0 && pos > start then
          let prev=
            (Query.prev_word_end ~pos ~start text)
          in
          prev_word prev (n-1)
        else
          pos
      in
      let dest= prev_word pos (count*n) in
      let delta= pos - dest + 1 in
      delete dest delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | WORD_back_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return () else
      let start, stop= Query.get_boundary true ctx in
      let pos= min (stop - 1) (Zed_edit.position ctx) in
      let rec prev_word pos n=
        if n > 0 && pos > start then
          let prev=
            (Query.prev_WORD_end ~pos ~start text)
          in
          prev_word prev (n-1)
        else
          pos
      in
      let dest= prev_word pos (count*n) in
      let delta= pos - dest + 1 in
      delete dest delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_FirstChar _n->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let pos= Zed_edit.position ctx in
      let start= Zed_lines.line_start lines line in
      delete start (pos - start) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_FirstNonBlank _n->
      let edit= Zed_edit.edit ctx in
      let pos= Zed_edit.position ctx in
      let start, stop= Query.get_boundary false ctx in
      let text= Zed_edit.text edit in
      let chr_fst= (Zed_char.core (Zed_rope.get text start)) in
      let nonblank=
        if Query.(is_space (get_category chr_fst)) then
          Query.next_word ~pos:start ~stop text
        else
          start
      in
      delete nonblank (pos - nonblank) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_LastChar n->
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar (count*n) ctx in
      delete pos (next+1 - pos) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_LastChar_nl n->
      let newline= true in
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar ~newline (count*n) ctx in
      delete pos (next+1 - pos) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | _-> return ())
  | Change (motion, count)->
    (match motion with
    | Left n->
      let pos, delta= Query.left (count*n) ctx in
      change pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Right n->
      let newline= true in
      let pos, delta= Query.right ~newline (count*n) ctx in
      let pos= pos - delta in
      change pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Right_nl n->
      let newline= true in
      let pos, delta= Query.right ~newline (count*n) ctx in
      let pos= pos - delta in
      exec [
        Edit (Zed (Zed_edit.Goto pos));
        Edit (Zed (Zed_edit.Kill_next_chars delta));
        ] >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Upward n->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let dest= max 0 (line - count * n) in
      let line_delta = line - dest in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines dest
        and pos_end= Zed_lines.line_stop lines line in
        let pos_delta= pos_end - pos_start in
        change pos_start pos_delta >>=
        (function
          | Result r-> Lwt_mvar.put result r
          | ContinueLoop _-> return ())
      else
        return ()
    | Downward n->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count * n) in
      let line_delta = dest - line in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines line
        and pos_end= Zed_lines.line_stop lines dest in
        let pos_end=
          if dest < line_count
          then pos_end + 1
          else pos_end in
        let pos_delta= pos_end - pos_start in
        change pos_start pos_delta >>=
        (function
          | Result r-> Lwt_mvar.put result r
          | ContinueLoop _-> return ())
      else
        return ()
    | Word n->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos (count*n) in
      let delta= next_pos - pos in
      change pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word_back n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let start, stop= Query.get_boundary true ctx in
      let pos= min (stop - 1) (Zed_edit.position ctx) in
      let rec prev_word pos n=
        if n > 0 && pos > start then
          let prev= max
            start
            (Query.prev_word ~pos ~start text)
          in
          prev_word prev (n-1)
        else
          pos
      in
      let prev_pos= prev_word pos (count*n) in
      let delta= pos - prev_pos in
      change prev_pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop= Query.get_boundary true ctx in
      let pos= Zed_edit.position ctx in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word_end ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos= next_word pos (count*n) in
      let delta= next_pos + 1 - pos in
      change pos delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Word_back_end n->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return () else
      let start, stop= Query.get_boundary true ctx in
      let pos= min (stop - 1) (Zed_edit.position ctx) in
      let rec prev_word pos n=
        if n > 0 && pos > start then
          let prev=
            (Query.prev_word_end ~pos ~start text)
          in
          prev_word prev (n-1)
        else
          pos
      in
      let dest= prev_word pos (count*n) in
      let delta= pos - dest + 1 in
      change dest delta >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_FirstChar _n->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let pos= Zed_edit.position ctx in
      let start= Zed_lines.line_start lines line in
      change start (pos - start) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_FirstNonBlank _n->
      let edit= Zed_edit.edit ctx in
      let pos= Zed_edit.position ctx in
      let start, stop= Query.get_boundary false ctx in
      let text= Zed_edit.text edit in
      let chr_fst= (Zed_char.core (Zed_rope.get text start)) in
      let nonblank=
        if Query.(is_space (get_category chr_fst)) then
          Query.next_word ~pos:start ~stop text
        else
          start
      in
      change nonblank (pos - nonblank) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_LastChar n->
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar (count*n) ctx in
      change pos (next+1 - pos) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | Line_LastChar_nl n->
      let newline= true in
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar ~newline (count*n) ctx in
      change pos (next+1 - pos) >>=
      (function
        | Result r-> Lwt_mvar.put result r
        | ContinueLoop _-> return ())
    | _-> return ())
  | Undo count->
    exec @@ list_dup [
      Edit (Zed (Zed_edit.Undo));
      ] count >>=
    (function
      | Result r-> Lwt_mvar.put result r
      | ContinueLoop _-> return ())
    >>= setup_pos
  | Paste_before count->
    exec @@ list_dup [
      Edit (Zed (Zed_edit.Yank));
      Edit (Zed (Zed_edit.Prev_char));
      ] count >>=
    (function
      | Result r-> Lwt_mvar.put result r
      | ContinueLoop _-> return ())
  | Paste_after count->
    exec @@ list_dup [
      Edit (Zed (Zed_edit.Next_char));
      Edit (Zed (Zed_edit.Yank));
      Edit (Zed (Zed_edit.Prev_char));
      ] count >>=
    (function
      | Result r-> Lwt_mvar.put result r
      | ContinueLoop _-> return ())
  | ChangeMode _mode-> return ()

