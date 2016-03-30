(*
 * lTerm_text.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open StdLabels
open Zed

type point =
  { mutable char  : Uchar.t
  ; mutable style : LTerm_style.t }
[@@deriving sexp]

type t = point array [@@deriving sexp]

(* +-----------------------------------------------------------------+
   | Conversions                                                     |
   +-----------------------------------------------------------------+ *)

let uspace = Uchar.of_char ' '

let create ?(style=LTerm_style.none) len =
  Array.init len ~f:(fun _ -> { char = uspace; style })
;;

let of_string ?(style=LTerm_style.none) str =
  let len = Zed_utf8.length str in
  let arr = create ~style len in
  let rec loop ofs idx =
    if idx = len then
      arr
    else begin
      let chr, ofs = Zed_utf8.extract_next str ofs in
      arr.(idx).char <- chr;
      loop ofs (idx + 1)
    end
  in
  loop 0 0
;;

let rec invalid_length str ofs acc =
  let ofs, len, _ = Zed_utf8.next_error str ofs in
  if ofs = String.length str then
    acc + len
  else
    invalid_length str (ofs + 1) (acc + len + 4)
;;

let of_string_maybe_invalid ?(style=LTerm_style.none) str =
  let len = invalid_length str 0 0 in
  let arr = create ~style len in
  let rec loop ~ofs ~idx ~until =
    if ofs = until then
      if ofs = String.length str then
        arr
      else begin
        let code = Char.code str.[ofs] in
        arr.(idx + 0).char <- Uchar.of_char '\\';
        let a = code / 100 in
        let b = (code - a * 100) / 10 in
        let c = code mod 10 in
        arr.(idx + 1).char <- Uchar.of_int (Char.code '0' + a);
        arr.(idx + 2).char <- Uchar.of_int (Char.code '0' + b);
        arr.(idx + 3).char <- Uchar.of_int (Char.code '0' + c);
        let ofs = ofs + 1 and idx = idx + 4 in
        let until, _, _ = Zed_utf8.next_error str ofs in
        loop ~ofs ~idx ~until
      end
    else begin
      let chr, ofs = Zed_utf8.extract_next str ofs in
      arr.(idx).char <- chr;
      loop ~ofs ~idx:(idx + 1) ~until
    end
  in
  let until, _, _ = Zed_utf8.next_error str 0 in
  loop ~ofs:0 ~idx:0 ~until
;;

let to_string txt =
  let buf = Buffer.create (Array.length txt) in
  Array.iter txt ~f:(fun { char; _ } -> Zed_utf8.add buf char);
  Buffer.contents buf
;;

let of_rope ?(style=LTerm_style.none) rope =
  let arr = create ~style (Zed_rope.length rope) in
  let rec loop zip idx =
    match Zed_rope.Zip.next zip with
    | No_more -> arr
    | Yield (chr, zip) ->
      arr.(idx).char <- chr;
      loop zip (idx + 1)
  in
  loop (Zed_rope.Zip.make_f rope 0) 0
;;

let to_rope txt =
  let buf = Zed_rope.Buffer.create () in
  Array.iter txt ~f:(fun { char; _ } -> Zed_rope.Buffer.add buf char);
  Zed_rope.Buffer.contents buf
;;

(* +-----------------------------------------------------------------+
   | Parenthesis matching                                            |
   +-----------------------------------------------------------------+ *)

let lparen = Uchar.of_char '('
let rparen = Uchar.of_char ')'
let lbrace = Uchar.of_char '{'
let rbrace = Uchar.of_char '}'
let lbracket = Uchar.of_char '['
let rbracket = Uchar.of_char ']'

type search_result =
  | No_match_found
  | No_paren_found
  | Match_found of int

let default_paren = [ (lparen   , rparen  )
                    ; (lbrace   , rbrace  )
                    ; (lbracket , rbracket)
                    ]

let stylise_parenthesis text ?(paren = default_paren) ~pos style_paren =
  if Array.length text > 0 then begin
    let rec rsearch idx left right depth =
      if idx >= Array.length text then
        No_match_found
      else
        let { char; _ } = text.(idx) in
        if char = right then
          if depth = 0 then
            Match_found idx
          else
            rsearch (idx + 1) left right (depth - 1)
        else if char = left then
          rsearch (idx + 1) left right (depth + 1)
        else
          rsearch (idx + 1) left right depth
    in
    let rec lsearch idx left right depth =
      if idx < 0 then
        No_match_found
      else
        let { char; _ } = text.(idx) in
        if char = left then
          if depth = 0 then
            Match_found idx
          else
            lsearch (idx - 1) left right (depth - 1)
        else if char = right then
          lsearch (idx - 1) left right (depth + 1)
        else
          lsearch (idx - 1) left right depth
    in
    let found =
      if pos = Array.length text then
        false
      else
        let { char; _ } = text.(pos) in
        let rec loop = function
          | [] ->
            No_paren_found
          | (lparen, rparen) :: rest ->
            if char = lparen then
              rsearch (pos + 1) lparen rparen 0
            else if char = rparen then
              lsearch (pos - 1) lparen rparen 0
            else
              loop rest
        in
        match loop paren with
        | Match_found idx ->
          let pt = text.(idx) in
          pt.style <- LTerm_style.merge style_paren pt.style;
          true
        | No_match_found ->
          true
        | No_paren_found ->
          false
    in
    if not found && pos > 0 then
      let { char; _ } = text.(pos - 1) in
      let rec loop = function
        | [] ->
          No_paren_found
        | (lparen, rparen) :: rest ->
          if char = lparen then
            rsearch (pos + 1) lparen rparen 0
          else if char = rparen then
            lsearch (pos - 2) lparen rparen 0
          else
            loop rest
      in
      match loop paren with
      | Match_found idx ->
        let pt = text.(pos - 1) in
        pt.style <- LTerm_style.merge style_paren pt.style;
        let pt = text.(idx) in
        pt.style <- LTerm_style.merge style_paren pt.style;
      | No_match_found | No_paren_found ->
        ()
  end

(* +-----------------------------------------------------------------+
   | Convenience                                                     |
   +-----------------------------------------------------------------+ *)

let mk
      ?bold
      ?underline
      ?blink
      ?reverse
      ?fg:foreground
      ?bg:background
      str
  =
  of_string ~style:(LTerm_style.make
                      ?bold
                      ?underline
                      ?blink
                      ?reverse
                      ?foreground
                      ?background
                      ())
    str
;;

let mkf
      ?bold
      ?underline
      ?blink
      ?reverse
      ?fg:foreground
      ?bg:background
      fmt
  =
  let style =
    LTerm_style.make
      ?bold
      ?underline
      ?blink
      ?reverse
      ?foreground
      ?background
      ()
  in
  Printf.ksprintf (fun str -> of_string ~style str) fmt
;;

let kmkf
      k
      ?bold
      ?underline
      ?blink
      ?reverse
      ?fg:foreground
      ?bg:background
      fmt
  =
  let style =
    LTerm_style.make
      ?bold
      ?underline
      ?blink
      ?reverse
      ?foreground
      ?background
      ()
  in
  Printf.ksprintf (fun str -> k (of_string ~style str)) fmt
;;

(* +-----------------------------------------------------------------+
   | Styled formatters                                               |
   +-----------------------------------------------------------------+ *)

let fold_coma_separated_words =
  let rec loop s i j f acc =
    if j = String.length s then
      let word = String.sub s ~pos:i ~len:(j - i) in
      f word acc
    else
      match s.[j] with
      | ',' ->
        let len = j - i in
        let word = String.sub s ~pos:i ~len:(j - i) in
        loop s (j + 1) (j + 1) f (f word acc)
      | _ ->
        loop s i (j + 1) f acc
  in
  fun s ~init ~f -> loop s 0 0 f init
;;

let style_of_tag s =
  fold_coma_separated_words s ~init:LTerm_style.none
    ~f:(fun s ac ->
      match s with
      | "bold"      -> LTerm_style.set_bold      ac On
      | "underline" -> LTerm_style.set_underline ac On
      | "blink"     -> LTerm_style.set_blink     ac On
      | "reverse"   -> LTerm_style.set_reverse   ac On
      | ""          -> failwith "empty style tag"
      | s           ->
        if s.[0] = '~' then
          LTerm_style.set_background ac
            (LTerm_style.Color.of_string
               (String.sub s ~pos:1 ~len:(String.length s - 1)))
        else
          LTerm_style.set_foreground ac (LTerm_style.Color.of_string s))
;;

let rec tagput_acc b (acc : (_, _) CamlinternalFormat.acc) =
  match acc with
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> tagput_acc b p; Buffer.add_string b s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> tagput_acc b p; Buffer.add_char b c
  | Acc_flush p              -> tagput_acc b p
  | Acc_invalid_arg (p, msg) -> tagput_acc b p; invalid_arg msg;
  | End_of_acc               -> ()
  | _ -> failwith "LTerm_text.ktprintf: format too compilcated"
;;

let style_of_format_acc acc =
  let buf = Buffer.create 16 in
  tagput_acc buf acc;
  let len = Buffer.length buf - 2 in
  if len > 0 then
    style_of_tag (Buffer.sub buf 1 len)
  else
    LTerm_style.none
;;

module Buffer = struct
  type t =
    { mutable txt           : point array
    ; mutable pos           : int
    ; mutable current_style : LTerm_style.t
    ; mutable style_stack   : LTerm_style.t list
    }

  let create len =
    { txt           = create len
    ; pos           = 0
    ; current_style = LTerm_style.none
    ; style_stack   = []
    }
  ;;

  let push_style t style =
    let current_style = t.current_style in
    let new_style = LTerm_style.merge current_style style in
    t.style_stack <- current_style :: t.style_stack;
    t.current_style <- new_style;
  ;;

  let pop_style t =
    match t.style_stack with
    | [] -> ()
    | style :: rest ->
      t.current_style <- style;
      t.style_stack <- rest;
  ;;

  let set_style t style = t.current_style <- style

  let check_size t needed =
    let len = Array.length t.txt in
    let old = t.txt in
    if t.pos + needed > len then begin
      let new_len = ref (2 * len) in
      while t.pos + needed > !new_len do
        new_len := !new_len * 2
      done;
      t.txt <- Array.init !new_len ~f:(fun i ->
        if i < len then
          old.(i)
        else
          { char = uspace
          ; style = LTerm_style.none
          })
    end
  ;;

  let add_uchar t ch =
    check_size t 1;
    let pos = t.pos in
    let pt = t.txt.(t.pos) in
    t.pos <- pos + 1;
    pt.char <- ch;
    pt.style <- t.current_style;
  ;;

  let add_char t ch = add_uchar t (Uchar.of_char ch)

  let add_string =
    let rec loop t str ofs limit =
      if ofs < limit then begin
        let ch, ofs = Zed_utf8.extract_next str ofs in
        add_uchar t ch;
        loop t str ofs limit
      end else if ofs = String.length str then
        ()
      else begin
        add_char t str.[ofs];
        let ofs = ofs + 1 in
        let limit, _, _ = Zed_utf8.next_error str ofs in
        loop t str ofs limit
      end
    in
    fun t str ->
      let limit, _, _ = Zed_utf8.next_error str 0 in
      loop t str 0 limit
  ;;

  let add_txt t txt =
    let len = Array.length txt in
    check_size t len;
    let start = t.pos in
    for i = 0 to len - 1 do
      let src_pt = txt.(i) in
      let dst_pt = t.txt.(start + i) in
      dst_pt.char <- src_pt.char;
      dst_pt.style <- LTerm_style.merge t.current_style src_pt.style;
    done;
    t.pos <- t.pos + len
  ;;

  let contents t = Array.sub t.txt ~pos:0 ~len:t.pos
end

let rec txtput_acc b (acc : (_, _) CamlinternalFormat.acc) =
  match acc with
  | Acc_formatting_lit (p, Close_tag) ->
    txtput_acc b p;
    Buffer.pop_style b
  | Acc_formatting_lit (p, fmting_lit) ->
    let s = CamlinternalFormat.string_of_formatting_lit fmting_lit in
    txtput_acc b p;
    Buffer.add_string b s
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    let style = style_of_format_acc acc' in
    txtput_acc b p;
    Buffer.push_style b style;
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    txtput_acc b p;
    Buffer.add_string b "@[";
    txtput_acc b acc'
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> txtput_acc b p; Buffer.add_string b s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> txtput_acc b p; Buffer.add_char b c
  | Acc_delay (p, f)         -> txtput_acc b p; Buffer.add_txt b (f ())
  | Acc_flush p              -> txtput_acc b p
  | Acc_invalid_arg (p, msg) -> txtput_acc b p; invalid_arg msg;
  | End_of_acc               -> ()
;;

let ktprintf k (Format (fmt, s) : (_, _, _, _) format4) =
  let k' () acc =
    let buf = Buffer.create (max 1 (String.length s)) in
    txtput_acc buf acc;
    k (Buffer.contents buf)
  in
  CamlinternalFormat.make_printf k' () End_of_acc fmt
;;

let tprintf fmt = ktprintf (fun x -> x) fmt

(* +-----------------------------------------------------------------+
   | Ansi control sequence parsing                                   |
   +-----------------------------------------------------------------+ *)

let parse_ansi ~start_style str =
  let buf = Buffer.create (String.length str) in
  Buffer.set_style buf start_style;
  let lexbuf = Lexing.from_string str in
  let rec loop start style =
    if start = String.length str then
      (Buffer.contents buf, style)
    else begin
      let stop, restart, next_style = LTerm_ansi_parser.token style lexbuf in
      if stop > start then
        Buffer.add_string buf (String.sub str ~pos:start ~len:(stop - start));
      Buffer.set_style buf next_style;
      loop restart next_style
    end
  in
  loop 0 start_style
;;
