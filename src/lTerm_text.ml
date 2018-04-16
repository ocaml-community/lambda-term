(*
 * lTerm_text.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDefault.Camomile
open LTerm_style

type t = (UChar.t * LTerm_style.t) array

(* +-----------------------------------------------------------------+
   | Conversions                                                     |
   +-----------------------------------------------------------------+ *)

let dummy = (UChar.of_char ' ', LTerm_style.none)

let of_string str =
  let len = Zed_utf8.length str in
  let arr = Array.make len dummy in
  let rec loop ofs idx =
    if idx = len then
      arr
    else begin
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      Array.unsafe_set arr idx (chr, LTerm_style.none);
      loop ofs (idx + 1)
    end
  in
  loop 0 0

let rec invalid_length str ofs acc =
  let ofs, len, _ = Zed_utf8.next_error str ofs in
  if ofs = String.length str then
    acc + len
  else
    invalid_length str (ofs + 1) (acc + len + 4)

let uchar_of_hex x =
  if x < 10 then
    UChar.of_int (Char.code '0' + x)
  else
    UChar.of_int (Char.code 'a' + x - 10)

let of_string_maybe_invalid str =
  let len = invalid_length str 0 0 in
  let arr = Array.make len dummy in
  let rec loop ofs idx =
    if idx = len then
      arr
    else begin
      let ofs, idx =
        try
          let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
          Array.unsafe_set arr idx (chr, LTerm_style.none);
          (ofs, idx + 1)
        with Zed_utf8.Invalid _ ->
          let code = Char.code (String.unsafe_get str ofs) in
          Array.unsafe_set arr (idx + 0) (UChar.of_char '\\', LTerm_style.none);
          Array.unsafe_set arr (idx + 1) (UChar.of_char 'y', LTerm_style.none);
          Array.unsafe_set arr (idx + 2) (uchar_of_hex (code lsr 4), LTerm_style.none);
          Array.unsafe_set arr (idx + 3) (uchar_of_hex (code land 15), LTerm_style.none);
          (ofs + 1, idx + 4)
      in
      loop ofs idx
    end
  in
  loop 0 0

let to_string txt =
  let buf = Buffer.create (Array.length txt) in
  Array.iter (fun (ch, _style) -> Buffer.add_string buf (Zed_utf8.singleton ch)) txt;
  Buffer.contents buf

let of_rope rope =
  let arr = Array.make (Zed_rope.length rope) dummy in
  let rec loop zip idx =
    if Zed_rope.Zip.at_eos zip then
      arr
    else begin
      let chr, zip = Zed_rope.Zip.next zip in
      Array.unsafe_set arr idx (chr, LTerm_style.none);
      loop zip (idx + 1)
    end
  in
  loop (Zed_rope.Zip.make_f rope 0) 0

let to_rope txt =
  let buf = Zed_rope.Buffer.create () in
  Array.iter (fun (ch, _style) -> Zed_rope.Buffer.add buf ch) txt;
  Zed_rope.Buffer.contents buf

let stylise str style =
  let len = Zed_utf8.length str in
  let arr = Array.make len dummy in
  let rec loop ofs idx =
    if idx = len then
      arr
    else begin
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      Array.unsafe_set arr idx (chr, style);
      loop ofs (idx + 1)
    end
  in
  loop 0 0

(* +-----------------------------------------------------------------+
   | Parenthesis matching                                            |
   +-----------------------------------------------------------------+ *)

let lparen = UChar.of_char '('
let rparen = UChar.of_char ')'
let lbrace = UChar.of_char '{'
let rbrace = UChar.of_char '}'
let lbracket = UChar.of_char '['
let rbracket = UChar.of_char ']'

type search_result =
  | No_match_found
  | No_paren_found
  | Match_found of int

let stylise_parenthesis text ?(paren = [(lparen, rparen); (lbrace, rbrace); (lbracket, rbracket)]) pos style_paren =
  if Array.length text > 0 then begin
    let rec rsearch idx left right depth =
      if idx >= Array.length text then
        No_match_found
      else
        let ch, _ = text.(idx) in
        if ch = right then
          if depth = 0 then
            Match_found idx
          else
            rsearch (idx + 1) left right (depth - 1)
        else if ch = left then
          rsearch (idx + 1) left right (depth + 1)
        else
          rsearch (idx + 1) left right depth
    in
    let rec lsearch idx left right depth =
      if idx < 0 then
        No_match_found
      else
        let ch, _ = text.(idx) in
        if ch = left then
          if depth = 0 then
            Match_found idx
          else
            lsearch (idx - 1) left right (depth - 1)
        else if ch = right then
          lsearch (idx - 1) left right (depth + 1)
        else
          lsearch (idx - 1) left right depth
    in
    let found =
      if pos = Array.length text then
        false
      else
        let ch, _ = text.(pos) in
        let rec loop = function
          | [] ->
              No_paren_found
          | (lparen, rparen) :: rest ->
              if ch = lparen then
                rsearch (pos + 1) lparen rparen 0
              else if ch = rparen then
                lsearch (pos - 1) lparen rparen 0
              else
                loop rest
        in
        match loop paren with
          | Match_found idx ->
              let ch, style = text.(idx) in
              text.(idx) <- (ch, LTerm_style.merge style_paren style);
              true
          | No_match_found ->
              true
          | No_paren_found ->
              false
    in
    if not found && pos > 0 then
      let ch, style = text.(pos - 1) in
      let rec loop = function
        | [] ->
            No_paren_found
        | (lparen, rparen) :: rest ->
            if ch = lparen then
              rsearch (pos + 1) lparen rparen 0
            else if ch = rparen then
              lsearch (pos - 2) lparen rparen 0
            else
              loop rest
      in
      match loop paren with
        | Match_found idx ->
            text.(pos - 1) <- (ch, LTerm_style.merge style_paren style);
            let ch, style = text.(idx) in
            text.(idx) <- (ch, LTerm_style.merge style_paren style)
        | No_match_found | No_paren_found ->
            ()
  end

(* +-----------------------------------------------------------------+
   | Markup strings                                                  |
   +-----------------------------------------------------------------+ *)

type item =
  | S of Zed_utf8.t
  | R of Zed_rope.t
  | B_bold of bool
  | E_bold
  | B_underline of bool
  | E_underline
  | B_blink of bool
  | E_blink
  | B_reverse of bool
  | E_reverse
  | B_fg of LTerm_style.color
  | E_fg
  | B_bg of LTerm_style.color
  | E_bg

type markup = item list

type eval_stack = {
  mutable q_bold : bool option list;
  mutable q_underline : bool option list;
  mutable q_blink : bool option list;
  mutable q_reverse : bool option list;
  mutable q_fg : LTerm_style.color option list;
  mutable q_bg : LTerm_style.color option list;
}

let markup_length markup =
  let rec loop len = function
    | [] -> len
    | S str :: rest -> loop (len + Zed_utf8.length str) rest
    | R str :: rest -> loop (len + Zed_rope.length str) rest
    | _ :: rest -> loop len rest
  in
  loop 0 markup

let eval markup =
  let state = {
    q_bold = [];
    q_underline = [];
    q_blink = [];
    q_reverse = [];
    q_fg = [];
    q_bg = [];
  } in
  let arr = Array.make (markup_length markup) dummy in
  let rec copy_utf8 str ofs idx style =
    if ofs = String.length str then
      idx
    else begin
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      Array.unsafe_set arr idx (chr, style);
      copy_utf8 str ofs (idx + 1) style
    end
  in
  let rec copy_rope zip idx style =
    if Zed_rope.Zip.at_eos zip then
      idx
    else begin
      let chr, zip = Zed_rope.Zip.next zip in
      Array.unsafe_set arr idx (chr, style);
      copy_rope zip (idx + 1) style
    end
  in
  let rec loop idx style = function
    | [] ->
        arr
    | S str :: rest ->
        loop (copy_utf8 str 0 idx style) style rest
    | R str :: rest ->
        loop (copy_rope (Zed_rope.Zip.make_f str 0) idx style) style rest
    | B_bold status :: rest ->
        state.q_bold <- style.bold :: state.q_bold;
        loop idx { style with bold = Some status } rest
    | E_bold :: rest -> begin
        match state.q_bold with
          | [] ->
              loop idx style rest
          | save :: l ->
              state.q_bold <- l;
              loop idx { style with bold = save } rest
      end
    | B_underline status :: rest ->
        state.q_underline <- style.underline :: state.q_underline;
        loop idx { style with underline = Some status } rest
    | E_underline :: rest -> begin
        match state.q_underline with
          | [] ->
              loop idx style rest
          | save :: l ->
              state.q_underline <- l;
              loop idx { style with underline = save } rest
      end
    | B_blink status :: rest ->
        state.q_blink <- style.blink :: state.q_blink;
        loop idx { style with blink = Some status } rest
    | E_blink :: rest -> begin
        match state.q_blink with
          | [] ->
              loop idx style rest
          | save :: l ->
              state.q_blink <- l;
              loop idx { style with blink = save } rest
      end
    | B_reverse color :: rest ->
        state.q_reverse <- style.reverse :: state.q_reverse;
        loop idx { style with reverse = Some color } rest
    | E_reverse :: rest -> begin
        match state.q_reverse with
          | [] ->
              loop idx style rest
          | save :: l ->
              state.q_reverse <- l;
              loop idx { style with reverse = save } rest
      end
    | B_fg color :: rest ->
        state.q_fg <- style.foreground :: state.q_fg;
        loop idx { style with foreground = Some color } rest
    | E_fg :: rest -> begin
        match state.q_fg with
          | [] ->
              loop idx style rest
          | save :: l ->
              state.q_fg <- l;
              loop idx { style with foreground = save } rest
      end
    | B_bg color :: rest ->
        state.q_bg <- style.background :: state.q_bg;
        loop idx { style with background = Some color } rest
    | E_bg :: rest -> begin
        match state.q_bg with
          | [] ->
              loop idx style rest
          | save :: l ->
              state.q_bg <- l;
              loop idx { style with background = save } rest
      end
  in
  loop 0 none markup



(** {6 Styled formatters} *)

let make_formatter ?read_color () =
  let style = Stack.create () in
  let content = ref [||] in

  let get_style () =
    if Stack.is_empty style then LTerm_style.none
    else Stack.top style
  and pop_style () =
    if Stack.is_empty style then ()
    else ignore (Stack.pop style)
  and push_style sty =
    if Stack.is_empty style then Stack.push sty style
    else Stack.push (LTerm_style.merge (Stack.top style) sty) style
  in

  let put s pos len =
    let s = String.sub s pos len in
    content := Array.append !content (stylise s (get_style ()))
  in
  let flush () = () in
  let fmt = Format.make_formatter put flush in

  let get_content () =
    Format.pp_print_flush fmt () ; !content
  in

  begin match read_color with
    | None -> ()
    | Some f ->
        Format.pp_set_tags fmt true;
        Format.pp_set_formatter_tag_functions fmt {
          Format.
          mark_open_tag =
            (fun a -> push_style (f a) ; "");
          mark_close_tag =
            (fun _ -> pop_style (); "");
          print_open_tag = (fun _ -> ());
          print_close_tag = (fun _ -> ());
        } ;
  end ;

  get_content, fmt

let pp_with_style to_style =
  fun style fstr fmt ->
    let tag = to_style style in
    Format.pp_open_tag fmt tag;
    Format.kfprintf
      (fun fmt ->
         Format.pp_close_tag fmt ())
      fmt fstr

let kstyprintf ?read_color f fstr =
  let get_content, fmt = make_formatter ?read_color () in
  Format.kfprintf (fun _ -> f (get_content ())) fmt fstr

let styprintf ?read_color fstr = kstyprintf ?read_color (fun x -> x) fstr
