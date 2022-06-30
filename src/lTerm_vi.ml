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
    | Right_nl of int (* l, including newline *)
    | Line_FirstChar of int (* 0 *)
    | Line_FirstNonBlank of int (* ^ *)
    | Line_LastChar of int (* $ *)
    | Line_LastChar_nl of int (* $ *)
    | Line_LastNonBlank of int (* g_ *)
    | Line_LastNonBlank_nl of int (* g_ *)

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

    (* line *)
    | Line

    (* occurrence *)
    | Occurrence_inline of string
    | Occurrence_inline_back of string
    | Occurrence_inline_till of string
    | Occurrence_inline_till_back of string

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
    | Parenthesis_include of int (* a( a) *)
    | Parenthesis_inner of int (* i( i) *)
    | Bracket_include of int (* a[ a] *)
    | Bracket_inner of int (* i[ i] *)
    | AngleBracket_include of int (* a< a> *)
    | AngleBracket_inner of int (* i< i> *)
    | Brace_include of int (* a{ a} *)
    | Brace_inner of int (* i{ i} *)
    | Quote_include of (string * int)
    | Quote_inner of (string * int)

    (* match *)
    | Match
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

  let get_category ?(nl_as_sp=false) uchar=
    if uchar = Zed_utf8.extract "\n" 0 && nl_as_sp then
      `Zs
    else
      Uucp.Gc.general_category uchar

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

  let goto_line ctx index=
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit in
    let index= min index (Zed_lines.count lines) in
    Zed_lines.line_start lines index

  let next_line ctx delta=
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let lines= Zed_edit.lines edit in
    let index = Zed_edit.line ctx in
    let cursor= Zed_edit.cursor ctx in
    let count= Zed_lines.count lines in
    if index = Zed_lines.count lines then
      Zed_rope.length text - 1
    else begin
      let stop =
        if index + delta >= count then
          Zed_rope.length text
        else
          Zed_lines.line_start lines (index + delta + 1) - 1
      in
      let wanted_idx= Zed_lines.get_idx_by_width
          lines
          (min count (index + delta))
          (Zed_cursor.get_wanted_column cursor)
      in
      max
        (Zed_lines.line_start lines (index + delta))
        (min wanted_idx (stop-1))
    end

  let prev_line ctx delta=
    let edit= Zed_edit.edit ctx in
    let lines= Zed_edit.lines edit in
    let cursor= Zed_edit.cursor ctx in
    let index = Zed_cursor.get_line cursor in
    if index - delta < 0 then
      0
    else
      let stop = Zed_lines.line_start lines (index-delta+1) - 1 in
      let wanted_idx= Zed_lines.get_idx_by_width
          lines
          (index - delta)
          (Zed_cursor.get_wanted_column cursor)
      in
      max
        (Zed_lines.line_start lines (index - delta))
        (min wanted_idx (stop-1))

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

  let line_FirstNonBlank _n ctx= (* ^ *)
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let lines= Zed_edit.lines edit
    and line_idx= Zed_edit.line ctx in
    let line_len= Zed_lines.line_length lines line_idx in
    let start, stop= get_boundary false ctx in
    if line_len > 0 then
      if is_space (get_category (Zed_char.core (Zed_rope.get text start))) then
        min (stop-1) (next_word ~multi_line:false ~pos:start ~stop text)
      else
        start
    else
      0

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
          prev_category ~nl_as_sp ~pos:prev ~start text
        else
          prev
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
        if is_space start_category || is_not_space after_start then
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

  let occurrence_char ~pos ~stop chr text=
    try
      let zip= Zed_rope.Zip.make_f text pos in
      let next= Zed_rope.Zip.find_f
        (fun c-> Zed_char.compare chr c = 0)
        zip
      in
      let next_pos= Zed_rope.Zip.offset next in
      if next_pos < stop then
        Some next_pos
      else
        None
    with _-> None

  let occurrence_char_back ~pos ~start chr text=
    try
      let zip= Zed_rope.Zip.make_f text pos in
      let prev= Zed_rope.Zip.find_b
        (fun c-> Zed_char.compare chr c = 0)
        zip
      in
      let prev_pos= Zed_rope.Zip.offset prev in
      if prev_pos > start then
        Some (prev_pos - 1)
      else
        None
    with _-> None

  let occurrence ~pos ~stop ~cmp text=
    try
      let zip= Zed_rope.Zip.make_f text pos in
      let next= Zed_rope.Zip.find_f cmp zip in
      let next_pos= Zed_rope.Zip.offset next in
      if next_pos < stop then
        Some (next_pos, Zed_rope.get text next_pos)
      else
        None
    with _-> None

  let occurrence_back ~pos ~start ~cmp text=
    try
      let zip= Zed_rope.Zip.make_f text pos in
      let prev= Zed_rope.Zip.find_b cmp zip in
      let prev_pos= Zed_rope.Zip.offset prev in
      if prev_pos > start then
        Some (prev_pos - 1, Zed_rope.get text (prev_pos - 1))
      else
        None
    with _-> None

  let occurrence_pare_raw ~pos ~level ~start ~stop pair text=
    let left, right= pair in
    let rec find_left level pos=
      if pos >= start then
        if level > 0 then
          match occurrence_char_back ~pos ~start left text with
          | Some pos-> find_left (level-1) (pos - 1)
          | None-> None
        else
          Some (pos+1)
      else
        None
    in
    let rec find_right level pos=
      if pos < stop then
        if level > 0 then
          match occurrence_char ~pos ~stop right text with
          | Some pos-> find_right (level-1) (pos - 1)
          | None-> None
        else
          Some (pos-1)
      else
        None
    in
    if level > 0 then
      match find_left level (pos+1) with
      | Some left->
        (match find_right level pos with
        | Some right-> Some (left, right)
        | None-> None)
      | None-> None
    else
      None

  let occurrence_pare ~pos ~level ~start ~stop pair text=
    let left, right= pair in
    let equal a b= Zed_char.compare a b = 0 in
    let cmp c= equal c left || equal c right in
    let rec find_left level pos=
      if level > 0 then
        if pos >= start then
          match occurrence_back ~pos ~start ~cmp text with
          | Some (pos, c)->
            if equal c left then
              find_left (level-1) (pos - 1)
            else
              find_left (level+1) (pos - 1)
          | None-> None
        else
          None
      else
        Some (pos+1)
    in
    let rec find_right level pos=
      if level > 0 then
        if pos < stop then
          match occurrence ~pos ~stop ~cmp text with
          | Some (pos, c)->
            if equal c right then
              find_right (level-1) (pos + 1)
            else
              find_right (level+1) (pos + 1)
          | None-> None
        else
          None
      else
        Some (pos-1)
    in
    if level > 0 && pos >= start && pos < stop then
      let init_pos=
        if equal (Zed_rope.get text pos) left
        then pos+1
        else if equal (Zed_rope.get text pos) right
        then pos
        else pos+1
      in
      match find_left level init_pos with
      | Some left->
        (match find_right 1 (left+1) with
        | Some right-> Some (left, right)
        | None-> None)
      | None-> None
    else
      None

  let item_match ~start ~stop pos text=
    match Zed_rope.get text pos |> Zed_char.to_utf8 with
    | "("->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "(", Zed_char.of_utf8 ")") text
      with
      | Some (_, right)-> Some right
      | None-> None)
    | ")"->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "(", Zed_char.of_utf8 ")") text
      with
      | Some (left, _)-> Some left
      | None-> None)
    | "["->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "[", Zed_char.of_utf8 "]") text
      with
      | Some (_, right)-> Some right
      | None-> None)
    | "]"->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "[", Zed_char.of_utf8 "]") text
      with
      | Some (left, _)-> Some left
      | None-> None)
    | "<"->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "<", Zed_char.of_utf8 ">") text
      with
      | Some (_, right)-> Some right
      | None-> None)
    | ">"->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "<", Zed_char.of_utf8 ">") text
      with
      | Some (left, _)-> Some left
      | None-> None)
    | "{"->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "{", Zed_char.of_utf8 "}") text
      with
      | Some (_, right)-> Some right
      | None-> None)
    | "}"->
      (match occurrence_pare
        ~pos ~level:1 ~start ~stop
        (Zed_char.of_utf8 "{", Zed_char.of_utf8 "}") text
      with
      | Some (left, _)-> Some left
      | None-> None)
    | _-> None

  let include_word' ?(multi_line=true) ~next_category ~pos ~stop text=
    if Zed_rope.length text = 0 then None else
    if pos >= stop then None else
    let nl_as_sp= multi_line in
    let start_category=
      let zchar= Zed_rope.get text pos in
      let core= Zed_char.core zchar in
      get_category ~nl_as_sp core
    in
    let pos_begin=
      if is_space start_category then
        let next= next_category ~nl_as_sp ~pos ~stop text in
        if next < stop then
          Some next
        else
          None
      else
        let prev= prev_category ~nl_as_sp ~pos ~start:0 text in
        if prev >= pos - 1 then
          Some pos
        else
          Some (prev + 1)
    in
    match pos_begin with
    | Some pos_begin->
      let pos_end= next_word'
        ~multi_line ~next_category ~pos:pos_begin ~stop text - 1
      in
      Some (pos_begin, pos_end)
    | None-> None

  let include_word ?multi_line ~pos ~stop text=
    let next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal in
    include_word' ?multi_line ~next_category ~pos ~stop text

  let include_WORD ?multi_line ~pos ~stop text=
    let next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal_blank in
    include_word' ?multi_line ~next_category ~pos ~stop text

  let inner_word'
      ?(multi_line=true) ~prev_category ~next_category ~pos ~stop text
    =
    if Zed_rope.length text = 0 then None else
    let nl_as_sp= multi_line in
    let pos_begin=
      if pos = 0 then
        0
      else
        prev_category ~nl_as_sp ~pos ~start:0 text + 1
    and pos_end= next_category ~nl_as_sp ~pos ~stop text - 1 in
    Some (pos_begin, pos_end)

  let inner_word ?multi_line ~pos ~stop text=
    let prev_category ~nl_as_sp=
      prev_category ~nl_as_sp ~is_equal:category_equal
    and next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal in
    inner_word' ?multi_line ~prev_category ~next_category ~pos ~stop text

  let inner_WORD ?multi_line ~pos ~stop text=
    let prev_category ~nl_as_sp=
      prev_category ~nl_as_sp ~is_equal:category_equal_blank
    and next_category ~nl_as_sp=
      next_category ~nl_as_sp ~is_equal:category_equal_blank in
    inner_word' ?multi_line ~prev_category ~next_category ~pos ~stop text

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

let perform vi_edit ctx exec action=
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
  let delete ~register ?(line=false) ?boundary start len=
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let eot= Zed_rope.length text in
    let lines= Zed_edit.lines edit in
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
            max
              (start - 1)
              Zed_lines.(line_start lines (line_index lines start))
          else
            start
      in
      let content=
        let str= Zed_rope.sub text start len
          |> Zed_rope.to_string
          |> Zed_string.to_utf8
        in
        if line then
          Vi.Interpret.Register.Line str
        else
          Vi.Interpret.Register.Seq str
      in
      vi_edit#set_register register content;
      vi_edit#set_register "\"" content;
      let del_len= if line && stop < eot then len + 1 else len in
      exec [
        Edit (Zed (Zed_edit.Goto start));
        Edit (Zed (Zed_edit.Kill_next_chars del_len));
        Edit (Zed (Zed_edit.Goto end_pos))
        ]
    else
      return (ContinueLoop [])
  in
  let change ~register ?(line=false) ?boundary start len=
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
      let content=
        let str= Zed_rope.sub text start len
          |> Zed_rope.to_string
          |> Zed_string.to_utf8
        in
        if line then
          Vi.Interpret.Register.Line str
        else
          Vi.Interpret.Register.Seq str
      in
      vi_edit#set_register register content;
      vi_edit#set_register "\"" content;
      exec [
        Edit (Zed (Zed_edit.Goto start));
        Edit (Zed (Zed_edit.Kill_next_chars len));
        Edit (Zed (Zed_edit.Goto start))
        ]
    else
      return (ContinueLoop [])
  in
  let yank ~register ?(line=false) start len=
    let edit= Zed_edit.edit ctx in
    let text= Zed_edit.text edit in
    let content=
      let str= Zed_rope.sub text start len
        |> Zed_rope.to_string
        |> Zed_string.to_utf8
      in
      if line then
        Vi.Interpret.Register.Line str
      else
        Vi.Interpret.Register.Seq str
    in
    vi_edit#set_register register content;
    vi_edit#set_register "\"" content;
    Zed_edit.copy_sequence ctx start len;
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
      exec [Edit (Zed (Zed_edit.Goto_bol))])
  in
  let pare_include pair level action=
    let text= Zed_edit.text (Zed_edit.edit ctx) in
    let pos= Zed_edit.position ctx in
    let start= 0
    and stop= Zed_rope.length text in
    (match Query.occurrence_pare
      ~pos ~level ~start ~stop
      pair
      text
    with
    | Some (left, right)-> action left (right+1 - left)
    | None-> return (ContinueLoop []))
  in
  let pare_inner pair level action=
    let text= Zed_edit.text (Zed_edit.edit ctx) in
    let pos= Zed_edit.position ctx in
    let start= 0
    and stop= Zed_rope.length text in
    (match Query.occurrence_pare
      ~pos ~level ~start ~stop
      pair
      text
    with
    | Some (left, right)-> action (left+1) (right - (left+1))
    | None-> return (ContinueLoop []))
  in
  match action with
  | Vi_action.Insert (insert, count)->
    (match insert with
    | Newline_below _s->
      exec @@
        (Edit (Zed (Zed_edit.Goto_eol)))::
        (list_make (Edit (Zed (Zed_edit.Newline))) count)
    | Newline_above _s->
      exec @@
        list_dup [
          Edit (Zed (Zed_edit.Goto_bol));
          Edit (Zed (Zed_edit.Newline));
          Edit (Zed (Zed_edit.Prev_line));
        ]
        count
    | _-> return (ContinueLoop []))
  | Motion (motion, count)->
    (match motion with
    | Left->
      let rec left n=
        if n > 0 then
          let pos, _delta= Query.left n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result _ as r-> return r
            | ContinueLoop _-> left (n-1))
        else
          return (ContinueLoop [])
      in
      left count
    | Right->
      let rec right n=
        if n > 0 then
          let pos, _delta= Query.right n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result _ as r-> return r
            | ContinueLoop _-> right (n-1))
        else
          return (ContinueLoop [])
      in
      right count
    | Right_nl->
      let newline= true in
      let rec right n=
        if n > 0 then
          let pos, _delta= Query.right ~newline n ctx in
          exec
            (list_make
              (Edit (Zed (Zed_edit.Goto pos))) 1) >>=
          (function
            | Result _ as r-> return r
            | ContinueLoop _-> right (n-1))
        else
          return (ContinueLoop [])
      in
      right count
    | Upward->
      let pos= Query.prev_line ctx count in
      exec [Edit (Zed (Zed_edit.Set_pos pos))]
    | Downward->
      let pos= Query.next_line ctx count in
      exec [Edit (Zed (Zed_edit.Set_pos pos))]
    | Word->
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
            [Edit (Zed (Zed_edit.Goto next))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> next_word (n-1))
        else
          return (ContinueLoop [])
      in
      next_word count
    | WORD->
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
            [Edit (Zed (Zed_edit.Goto next))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> next_word (n-1))
        else
          return (ContinueLoop [])
      in
      next_word count
    | Word_back->
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
            | Result _ as r-> return r
            | ContinueLoop _-> prev_word (n-1))
        else
          return (ContinueLoop [])
      in
      prev_word count
    | WORD_back->
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
            [Edit (Zed (Zed_edit.Goto prev))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> prev_word (n-1))
        else
          return (ContinueLoop [])
      in
      prev_word count
    | Word_end->
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
            [Edit (Zed (Zed_edit.Goto next))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> next_word (n-1))
        else
          return (ContinueLoop [])
      in
      next_word count
    | WORD_end->
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
            [Edit (Zed (Zed_edit.Goto next))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> next_word (n-1))
        else
          return (ContinueLoop [])
      in
      next_word count
    | Word_back_end->
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
            [Edit (Zed (Zed_edit.Goto prev))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> prev_word (n-1))
        else
          return (ContinueLoop [])
      in
      prev_word count
    | WORD_back_end->
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
            [Edit (Zed (Zed_edit.Goto prev))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> prev_word (n-1))
        else
          return (ContinueLoop [])
      in
      prev_word count
    | Line_FirstChar->
      exec
        (list_make
          (Edit (Zed Zed_edit.Goto_bol))
          count)
    | Line_FirstNonBlank->
      let nonblank= Query.line_FirstNonBlank 1 ctx in
      exec
        [Edit (Zed (Zed_edit.Goto nonblank))]
    | Line_LastChar->
      let rec lastChar n=
        if n > 0 then
          let pos= Query.line_LastChar n ctx in
          exec
            [Edit (Zed (Zed_edit.Goto pos))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> lastChar (n-1))
        else
          return (ContinueLoop [])
      in
      lastChar count
    | Line_LastChar_nl->
      let newline= true in
      let rec lastChar n=
        if n > 0 then
          let pos= Query.line_LastChar ~newline n ctx in
          exec
            [Edit (Zed (Zed_edit.Goto pos))]
          >>= (function
            | Result _ as r-> return r
            | ContinueLoop _-> lastChar (n-1))
        else
          return (ContinueLoop [])
      in
      lastChar count
    | GotoLine->
      let pos= Query.goto_line ctx count in
      exec [Edit (Zed (Zed_edit.Set_pos pos))]
    | GotoLine_first->
      exec [Edit (Zed (Zed_edit.Goto_bot))]
    | GotoLine_last->
      exec [
        Edit (Zed (Zed_edit.Goto_eot));
        Edit (Zed (Zed_edit.Prev_char))
        ]
    | Occurrence_inline chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some pos->
        exec [ Edit (Zed (Zed_edit.Goto pos)) ]
      | None-> return (ContinueLoop []))
    | Occurrence_inline_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos-1) count with
      | Some pos->
        exec [ Edit (Zed (Zed_edit.Goto pos)) ]
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some pos->
        exec [ Edit (Zed (Zed_edit.Goto (pos-1))) ]
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos-1) count with
      | Some pos->
        exec [ Edit (Zed (Zed_edit.Goto (pos+1))) ]
      | None-> return (ContinueLoop []))
    | Match->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.item_match ~start:0 ~stop pos text with
      | Some pos->
        exec [ Edit (Zed (Zed_edit.Goto pos)) ]
      | None-> return (ContinueLoop []))
    | _-> return (ContinueLoop []))
  | Delete (register, motion, count)->
    let delete= delete ~register in
    (match motion with
    | Left->
      let pos, delta= Query.left count ctx in
      delete pos delta
    | Right->
      let newline=true in
      let pos, delta= Query.right ~newline count ctx in
      let pos= pos - delta in
      delete pos delta
    | Right_nl->
      let newline= true in
      let pos, delta= Query.right ~newline count ctx in
      let pos= pos - delta in
      exec [
        Edit (Zed (Zed_edit.Goto pos));
        Edit (Zed (Zed_edit.Kill_next_chars delta));
        ]
    | Upward->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let dest= max 0 (line - count) in
      let line_delta = line - dest in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines dest
        and pos_end= Zed_lines.line_stop lines line in
        let pos_delta= pos_end - pos_start in
        delete ~line:true pos_start pos_delta
      else
        return (ContinueLoop [])
    | Downward->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count) in
      let line_delta = dest - line in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines line
        and pos_end= Zed_lines.line_stop lines dest in
        let pos_end=
          if dest < line_count
          then pos_end + 1
          else pos_end in
        let pos_delta= pos_end - pos_start in
        delete ~line:true pos_start pos_delta
      else
        return (ContinueLoop [])
    | Line->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count - 1) in
      let pos_start= Zed_lines.line_start lines line
      and pos_end= Zed_lines.line_stop lines dest in
      let pos_delta= pos_end - pos_start in
      delete ~line:true pos_start pos_delta
    | Word->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos count in
      let delta= next_pos - pos in
      delete pos delta
    | WORD->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_WORD ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos count in
      let delta= next_pos - pos in
      delete pos delta
    | Word_back->
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
      let prev_pos= prev_word pos count in
      let delta= pos - prev_pos in
      delete prev_pos delta
    | WORD_back->
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
      let prev_pos= prev_word pos count in
      let delta= pos - prev_pos in
      delete prev_pos delta
    | Word_end->
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
      let next_pos= next_word pos count in
      let delta= next_pos + 1 - pos in
      delete pos delta
    | WORD_end->
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
      let next_pos= next_word pos count in
      let delta= next_pos + 1 - pos in
      delete pos delta
    | Word_back_end->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return (ContinueLoop []) else
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
      let dest= prev_word pos count in
      let delta= pos - dest + 1 in
      delete dest delta
    | WORD_back_end->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return (ContinueLoop []) else
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
      let dest= prev_word pos count in
      let delta= pos - dest + 1 in
      delete dest delta
    | Line_FirstChar->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let pos= Zed_edit.position ctx in
      let start= Zed_lines.line_start lines line in
      delete start (pos - start)
    | Line_FirstNonBlank->
      let pos= Zed_edit.position ctx in
      let nonblank= Query.line_FirstNonBlank 1 ctx in
      (if nonblank < pos then
        delete nonblank (pos - nonblank)
      else
        delete pos (nonblank - pos))
    | Line_LastChar->
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar count ctx in
      delete pos (next+1 - pos)
    | Line_LastChar_nl->
      let newline= true in
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar ~newline count ctx in
      delete pos (next+1 - pos)
    | Parenthesis_include->
      pare_include Zed_char.(of_utf8 "(", of_utf8 ")") count delete
    | Parenthesis_inner->
      pare_inner Zed_char.(of_utf8 "(", of_utf8 ")") count delete
    | Bracket_include->
      pare_include Zed_char.(of_utf8 "[", of_utf8 "]") count delete
    | Bracket_inner->
      pare_inner Zed_char.(of_utf8 "[", of_utf8 "]") count delete
    | AngleBracket_include->
      pare_include Zed_char.(of_utf8 "<", of_utf8 ">") count delete
    | AngleBracket_inner->
      pare_inner Zed_char.(of_utf8 "<", of_utf8 ">") count delete
    | Brace_include->
      pare_include Zed_char.(of_utf8 "{", of_utf8 "}") count delete
    | Brace_inner->
      pare_inner Zed_char.(of_utf8 "{", of_utf8 "}") count delete
    | Occurrence_inline chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some pos->
        let start= Zed_edit.position ctx in
        let delta= pos+1 - start in
        delete start delta
      | None-> return (ContinueLoop []))
    | Occurrence_inline_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos-1) count with
      | Some pos->
        let stop= Zed_edit.position ctx in
        let delta= stop - pos in
        delete pos delta
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos+1) count with
      | Some dest->
        delete pos (dest - pos)
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some dest->
        delete (dest+1) (pos-1 - dest)
      | None-> return (ContinueLoop []))
    | Match->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.item_match ~start:0 ~stop pos text with
      | Some dest->
        (if dest > pos then
          delete pos (dest+1 - pos)
        else
          delete dest (pos+1 - dest))
      | None-> return (ContinueLoop []))
    | Word_include->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let move_n pos n=
        let rec move_n pos n=
          if n >= 1 then
            match Query.include_word ~stop ~pos text with
            | Some (_word_begin, word_end)-> move_n (word_end+1) (n-1)
            | None-> pos-1
          else
            pos-1
        in
        if n >= 1 then
          match Query.include_word ~stop ~pos text with
          | Some (word_begin, word_end)->
            let word_end= move_n (word_end+1) (n - 1) in
            Some (word_begin, word_end)
          | None-> None
        else
          None
      in
      (match move_n pos count with
      | Some (word_begin, word_end)->
        delete word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | WORD_include->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let move_n pos n=
        let rec move_n pos n=
          if n >= 1 then
            match Query.include_WORD ~stop ~pos text with
            | Some (_word_begin, word_end)-> move_n (word_end+1) (n-1)
            | None-> pos
          else
            pos-1
        in
        if n >= 1 then
          match Query.include_WORD ~stop ~pos text with
          | Some (word_begin, word_end)->
            let word_end= move_n (word_end+1) (n - 1) in
            Some (word_begin, word_end)
          | None-> None
        else
          None
      in
      (match move_n pos count with
      | Some (word_begin, word_end)->
        delete word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | Word_inner->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.inner_word ~pos ~stop text with
      | Some (word_begin, word_end)->
        delete word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | WORD_inner->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.inner_WORD ~pos ~stop text with
      | Some (word_begin, word_end)->
        delete word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | Quote_inner chr->
      let quote= Zed_char.of_utf8 chr in
      pare_inner (quote, quote) 1 delete
    | Quote_include chr->
      let quote= Zed_char.of_utf8 chr in
      pare_include (quote, quote) count delete
    | _-> return (ContinueLoop []))
  | Change (register, motion, count)->
    let change= change ~register in
    (match motion with
    | Left->
      let pos, delta= Query.left count ctx in
      change pos delta
    | Right->
      let newline= true in
      let pos, delta= Query.right ~newline count ctx in
      let pos= pos - delta in
      change pos delta
    | Right_nl->
      let newline= true in
      let pos, delta= Query.right ~newline count ctx in
      let pos= pos - delta in
      exec [
        Edit (Zed (Zed_edit.Goto pos));
        Edit (Zed (Zed_edit.Kill_next_chars delta));
        ]
    | Upward->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let dest= max 0 (line - count) in
      let line_delta = line - dest in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines dest
        and pos_end= Zed_lines.line_stop lines line in
        let pos_delta= pos_end - pos_start in
        change ~line:true pos_start pos_delta
      else
        return (ContinueLoop [])
    | Downward->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count) in
      let line_delta = dest - line in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines line
        and pos_end= Zed_lines.line_stop lines dest in
        let pos_end=
          if dest < line_count
          then pos_end + 1
          else pos_end in
        let pos_delta= pos_end - pos_start in
        change ~line:true pos_start pos_delta
      else
        return (ContinueLoop [])
    | Word->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos count in
      let delta= next_pos - pos in
      change pos delta
    | WORD->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_WORD ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos count in
      let delta= next_pos - pos in
      change pos delta
    | Word_back->
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
      let prev_pos= prev_word pos count in
      let delta= pos - prev_pos in
      change prev_pos delta
    | WORD_back->
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
      let prev_pos= prev_word pos count in
      let delta= pos - prev_pos in
      change prev_pos delta
    | Word_end->
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
      let next_pos= next_word pos count in
      let delta= next_pos + 1 - pos in
      change pos delta
    | WORD_end->
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
      let next_pos= next_word pos count in
      let delta= next_pos + 1 - pos in
      change pos delta
    | Word_back_end->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return (ContinueLoop []) else
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
      let dest= prev_word pos count in
      let delta= pos - dest + 1 in
      change dest delta
    | WORD_back_end->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return (ContinueLoop []) else
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
      let dest= prev_word pos count in
      let delta= pos - dest + 1 in
      change dest delta
    | Line_FirstChar->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let pos= Zed_edit.position ctx in
      let start= Zed_lines.line_start lines line in
      change start (pos - start)
    | Line_FirstNonBlank->
      let pos= Zed_edit.position ctx in
      let nonblank= Query.line_FirstNonBlank 1 ctx in
      (if nonblank < pos then
        change nonblank (pos - nonblank)
      else
        change pos (nonblank - pos))
    | Line_LastChar->
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar count ctx in
      change pos (next+1 - pos)
    | Line_LastChar_nl->
      let newline= true in
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar ~newline count ctx in
      change pos (next+1 - pos)
    | Parenthesis_include->
      pare_include Zed_char.(of_utf8 "(", of_utf8 ")") count change
    | Parenthesis_inner->
      pare_inner Zed_char.(of_utf8 "(", of_utf8 ")") count change
    | Bracket_include->
      pare_include Zed_char.(of_utf8 "[", of_utf8 "]") count change
    | Bracket_inner->
      pare_inner Zed_char.(of_utf8 "[", of_utf8 "]") count change
    | AngleBracket_include->
      pare_include Zed_char.(of_utf8 "<", of_utf8 ">") count change
    | AngleBracket_inner->
      pare_inner Zed_char.(of_utf8 "<", of_utf8 ">") count change
    | Brace_include->
      pare_include Zed_char.(of_utf8 "{", of_utf8 "}") count change
    | Brace_inner->
      pare_inner Zed_char.(of_utf8 "{", of_utf8 "}") count change
    | Occurrence_inline chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some pos->
        let start= Zed_edit.position ctx in
        let delta= pos+1 - start in
        change start delta
      | None-> return (ContinueLoop []))
    | Occurrence_inline_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos-1) count with
      | Some pos->
        let stop= Zed_edit.position ctx in
        let delta= stop - pos in
        change pos delta
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos+1) count with
      | Some dest->
        change pos (dest - pos)
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some dest->
        change (dest+1) (pos-1 - dest)
      | None-> return (ContinueLoop []))
    | Match->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.item_match ~start:0 ~stop pos text with
      | Some dest->
        (if dest > pos then
          change pos (dest+1 - pos)
        else
          change dest (pos+1 - dest))
      | None-> return (ContinueLoop []))
    | Word_include->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let move_n pos n=
        let rec move_n pos n=
          if n >= 1 then
            match Query.include_word ~stop ~pos text with
            | Some (_word_begin, word_end)-> move_n (word_end+1) (n-1)
            | None-> pos-1
          else
            pos-1
        in
        if n >= 1 then
          match Query.include_word ~stop ~pos text with
          | Some (word_begin, word_end)->
            let word_end= move_n (word_end+1) (n - 1) in
            Some (word_begin, word_end)
          | None-> None
        else
          None
      in
      (match move_n pos count with
      | Some (word_begin, word_end)->
        change word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | WORD_include->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let move_n pos n=
        let rec move_n pos n=
          if n >= 1 then
            match Query.include_WORD ~stop ~pos text with
            | Some (_word_begin, word_end)-> move_n (word_end+1) (n-1)
            | None-> pos
          else
            pos-1
        in
        if n >= 1 then
          match Query.include_WORD ~stop ~pos text with
          | Some (word_begin, word_end)->
            let word_end= move_n (word_end+1) (n - 1) in
            Some (word_begin, word_end)
          | None-> None
        else
          None
      in
      (match move_n pos count with
      | Some (word_begin, word_end)->
        change word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | Word_inner->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.inner_word ~pos ~stop text with
      | Some (word_begin, word_end)->
        change word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | WORD_inner->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.inner_WORD ~pos ~stop text with
      | Some (word_begin, word_end)->
        change word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | Quote_inner chr->
      let quote= Zed_char.of_utf8 chr in
      pare_inner (quote, quote) 1 change
    | Quote_include chr->
      let quote= Zed_char.of_utf8 chr in
      pare_include (quote, quote) count change
    | _-> return (ContinueLoop []))
  | Yank (register, motion, count)->
    let yank= yank ~register in
    (match motion with
    | Left->
      let pos, delta= Query.left count ctx in
      yank pos delta
    | Right->
      let newline=true in
      let pos, delta= Query.right ~newline count ctx in
      let pos= pos - delta in
      yank pos delta
    | Right_nl->
      let newline= true in
      let pos, delta= Query.right ~newline count ctx in
      let pos= pos - delta in
      yank pos delta
    | Upward->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let dest= max 0 (line - count) in
      let line_delta = line - dest in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines dest
        and pos_end= Zed_lines.line_stop lines line in
        let pos_delta= pos_end - pos_start in
        yank ~line:true pos_start pos_delta
      else
        return (ContinueLoop [])
    | Downward->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count) in
      let line_delta = dest - line in
      if line_delta > 0 then
        let pos_start= Zed_lines.line_start lines line
        and pos_end= Zed_lines.line_stop lines dest in
        let pos_end=
          if dest < line_count
          then pos_end + 1
          else pos_end in
        let pos_delta= pos_end - pos_start in
        yank ~line:true pos_start pos_delta
      else
        return (ContinueLoop [])
    | Line->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line_count= Zed_lines.count lines in
      let line= Zed_edit.line ctx in
      let dest= min line_count (line + count - 1) in
      let pos_start= Zed_lines.line_start lines line
      and pos_end= Zed_lines.line_stop lines dest in
      let pos_delta= pos_end - pos_start in
      yank ~line:true pos_start pos_delta
    | Word->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_word ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos count in
      let delta= next_pos - pos in
      yank pos delta
    | WORD->
      let pos= Zed_edit.position ctx in
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let _start, stop=
        if count > 1 then
          Query.get_boundary true ctx
        else
          Query.get_boundary false ctx
      in
      let rec next_word pos n=
        if n > 0 && pos < stop then
          let next=
            (Query.next_WORD ~pos ~stop text)
          in
          next_word next (n-1)
        else
          pos
      in
      let next_pos = next_word pos count in
      let delta= next_pos - pos in
      yank pos delta
    | Word_back->
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
      let prev_pos= prev_word pos count in
      let delta= pos - prev_pos in
      yank prev_pos delta
    | WORD_back->
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
      let prev_pos= prev_word pos count in
      let delta= pos - prev_pos in
      yank prev_pos delta
    | Word_end->
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
      let next_pos= next_word pos count in
      let delta= next_pos + 1 - pos in
      yank pos delta
    | WORD_end->
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
      let next_pos= next_word pos count in
      let delta= next_pos + 1 - pos in
      yank pos delta
    | Word_back_end->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return (ContinueLoop []) else
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
      let dest= prev_word pos count in
      let delta= pos - dest + 1 in
      yank dest delta
    | WORD_back_end->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      if Zed_rope.length text <= 0 then return (ContinueLoop []) else
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
      let dest= prev_word pos count in
      let delta= pos - dest + 1 in
      yank dest delta
    | Line_FirstChar->
      let edit= Zed_edit.edit ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let pos= Zed_edit.position ctx in
      let start= Zed_lines.line_start lines line in
      yank start (pos - start)
    | Line_FirstNonBlank->
      let pos= Zed_edit.position ctx in
      let nonblank= Query.line_FirstNonBlank 1 ctx in
      if nonblank < pos then
        yank nonblank (pos - nonblank)
      else
        yank nonblank (pos - nonblank)
    | Line_LastChar->
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar count ctx in
      yank pos (next+1 - pos)
    | Line_LastChar_nl->
      let newline= true in
      let pos= Zed_edit.position ctx in
      let next= Query.line_LastChar ~newline count ctx in
      yank pos (next+1 - pos)
    | Parenthesis_include->
      pare_include Zed_char.(of_utf8 "(", of_utf8 ")") count yank
    | Parenthesis_inner->
      pare_inner Zed_char.(of_utf8 "(", of_utf8 ")") count yank
    | Bracket_include->
      pare_include Zed_char.(of_utf8 "[", of_utf8 "]") count yank
    | Bracket_inner->
      pare_inner Zed_char.(of_utf8 "[", of_utf8 "]") count yank
    | AngleBracket_include->
      pare_include Zed_char.(of_utf8 "<", of_utf8 ">") count yank
    | AngleBracket_inner->
      pare_inner Zed_char.(of_utf8 "<", of_utf8 ">") count yank
    | Brace_include->
      pare_include Zed_char.(of_utf8 "{", of_utf8 "}") count yank
    | Brace_inner->
      pare_inner Zed_char.(of_utf8 "{", of_utf8 "}") count yank
    | Occurrence_inline chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some pos->
        let start= Zed_edit.position ctx in
        let delta= pos+1 - start in
        yank start delta
      | None-> return (ContinueLoop []))
    | Occurrence_inline_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx + 1 in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos-1) count with
      | Some pos->
        let stop= Zed_edit.position ctx in
        let delta= stop - pos in
        yank pos delta
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let stop= Zed_lines.line_stop lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let next= Query.occurrence_char ~pos ~stop chr text in
        if n = 1 then next else
          match next with
          | Some next-> query_n chr (next+1) (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) (pos+1) count with
      | Some dest->
        yank  pos (dest - pos)
      | None-> return (ContinueLoop []))
    | Occurrence_inline_till_back chr->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let lines= Zed_edit.lines edit in
      let line= Zed_edit.line ctx in
      let start= Zed_lines.line_start lines line in
      let rec query_n chr pos n=
        if n < 1 then None else
        let prev= Query.occurrence_char_back ~pos ~start chr text in
        if n = 1 then prev else
          match prev with
          | Some prev-> query_n chr prev (n-1)
          | None-> None
      in
      (match query_n (Zed_char.of_utf8 chr) pos count with
      | Some dest->
        yank (dest+1) (pos-1 - dest)
      | None-> return (ContinueLoop []))
    | Match->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.item_match ~start:0 ~stop pos text with
      | Some dest->
        if dest > pos then
          yank pos (dest+1 - pos)
        else
          yank dest (pos+1 - dest)
      | None-> return (ContinueLoop []))
    | Word_include->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      let move_n pos n=
        let rec move_n pos n=
          if n >= 1 then
            match Query.include_word ~stop ~pos text with
            | Some (_word_begin, word_end)-> move_n (word_end+1) (n-1)
            | None-> pos-1
          else
            pos-1
        in
        if n >= 1 then
          match Query.include_word ~stop ~pos text with
          | Some (word_begin, word_end)->
            let word_end= move_n (word_end+1) (n - 1) in
            Some (word_begin, word_end)
          | None-> None
        else
          None
      in
      (match move_n pos count with
      | Some (word_begin, word_end)->
        Zed_edit.copy_sequence ctx word_begin (word_end+1 - word_begin);
        return (ContinueLoop [])
      | None-> return (ContinueLoop []))
    | WORD_include->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      let move_n pos n=
        let rec move_n pos n=
          if n >= 1 then
            match Query.include_WORD ~stop ~pos text with
            | Some (_word_begin, word_end)-> move_n (word_end+1) (n-1)
            | None-> pos
          else
            pos-1
        in
        if n >= 1 then
          match Query.include_WORD ~stop ~pos text with
          | Some (word_begin, word_end)->
            let word_end= move_n (word_end+1) (n - 1) in
            Some (word_begin, word_end)
          | None-> None
        else
          None
      in
      (match move_n pos count with
      | Some (word_begin, word_end)->
        yank word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | Word_inner->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.inner_word ~pos ~stop text with
      | Some (word_begin, word_end)->
        yank word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | WORD_inner->
      let edit= Zed_edit.edit ctx in
      let text= Zed_edit.text edit in
      let pos= Zed_edit.position ctx in
      let stop= Zed_rope.length text in
      (match Query.inner_WORD ~pos ~stop text with
      | Some (word_begin, word_end)->
        yank word_begin (word_end+1 - word_begin)
      | None-> return (ContinueLoop []))
    | Quote_inner chr->
      let quote= Zed_char.of_utf8 chr in
      pare_inner (quote, quote) 1 yank
    | Quote_include chr->
      let quote= Zed_char.of_utf8 chr in
      pare_include (quote, quote) count yank
    | _-> return (ContinueLoop []))
  | Undo count->
    exec @@ list_dup [
      Edit (Zed (Zed_edit.Undo));
      ] count
    >>= (fun r-> setup_pos () >>= fun _-> return r)
  | Paste_before (register, count)->
    let action_paste=
      let open Vi.Interpret.Register in
      match vi_edit#get_register register with
      | Some (Seq str)-> [
        Edit (Zed (Zed_edit.Insert_str (Zed_string.of_utf8 str)));
        Edit (Zed (Zed_edit.Prev_char));
        ]
      | Some (Line str)-> [
        Edit (Zed (Zed_edit.Goto_bol));
        Edit (Zed (Zed_edit.Insert_str (Zed_string.of_utf8 (str ^ "\n"))));
        Edit (Zed (Zed_edit.Prev_line));
        Edit (Zed (Zed_edit.Goto_eol));
        Edit (Zed (Zed_edit.Prev_char));
        ]
      | None-> []
    in
    exec @@ list_dup action_paste count
  | Paste_after (register, count)->
    let action_paste=
      let open Vi.Interpret.Register in
      match vi_edit#get_register register with
      | Some (Seq str)->
        let actions= [
          Edit (Zed (Zed_edit.Insert_str (Zed_string.of_utf8 str)));
          Edit (Zed (Zed_edit.Prev_char));
          ]
        in
        if Zed_edit.at_eol ctx then
          actions
        else
          Edit (Zed (Zed_edit.Next_char)) :: actions
      | Some (Line str)->
        [
        Edit (Zed (Zed_edit.Goto_eol));
        Edit (Zed (Zed_edit.Insert_str
          (Zed_string.of_utf8
            ("\n" ^ (String.sub str 0 (String.length str))))));
        Edit (Zed (Zed_edit.Goto_eol));
        Edit (Zed (Zed_edit.Prev_char));
        ]
      | None-> []
    in
    exec @@ list_dup action_paste count
  | Join count->
    exec @@
      (list_make (Edit (Zed (Zed_edit.Join_line))) count)
  | DeleteSelected register->
    let delete= delete ~register in
    let edit= Zed_edit.edit ctx in
    if Zed_edit.get_selection edit then
      let a = Zed_edit.position ctx and b = Zed_cursor.get_position (Zed_edit.mark edit) in
      let a = min a b and b = max a b in
      delete a (b+1 - a)
    else
      return (ContinueLoop [])
  | YankSelected register->
    let yank= yank ~register in
    let edit= Zed_edit.edit ctx in
    if Zed_edit.get_selection edit then
      let a = Zed_edit.position ctx and b = Zed_cursor.get_position (Zed_edit.mark edit) in
      let a = min a b and b = max a b in
      yank a (b+1 - a)
    else
      return (ContinueLoop [])
  | ChangeMode mode->
    let edit= Zed_edit.edit ctx in
    (match mode with
    | Insert-> Zed_edit.set_selection edit false
    | Normal-> Zed_edit.set_selection edit false
    | Visual-> Zed_edit.set_mark ctx
    | Commandline-> Zed_edit.set_selection edit false);
    return (ContinueLoop [])

