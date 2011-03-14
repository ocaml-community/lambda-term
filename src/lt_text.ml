(*
 * lt_text.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lt_style

type t = (UChar.t * Lt_style.t) array

(* +-----------------------------------------------------------------+
   | Conversions                                                     |
   +-----------------------------------------------------------------+ *)

let dummy = (UChar.of_char ' ', Lt_style.none)

let of_string str =
  let len = Zed_utf8.length str in
  let arr = Array.create len dummy in
  let rec loop ofs idx =
    if idx = len then
      arr
    else begin
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      Array.unsafe_set arr idx (chr, Lt_style.none);
      loop ofs (idx + 1)
    end
  in
  loop 0 0

let to_string txt =
  let buf = Buffer.create (Array.length txt) in
  Array.iter (fun (ch, style) -> Buffer.add_string buf (Zed_utf8.singleton ch)) txt;
  Buffer.contents buf

let of_rope rope =
  let arr = Array.create (Zed_rope.length rope) dummy in
  let rec loop zip idx =
    if Zed_rope.Zip.at_eos zip then
      arr
    else begin
      let chr, zip = Zed_rope.Zip.next zip in
      Array.unsafe_set arr idx (chr, Lt_style.none);
      loop zip (idx + 1)
    end
  in
  loop (Zed_rope.Zip.make_f rope 0) 0

let to_rope txt =
  let buf = Zed_rope.Buffer.create () in
  Array.iter (fun (ch, style) -> Zed_rope.Buffer.add buf ch) txt;
  Zed_rope.Buffer.contents buf

let stylise str style =
  let len = Zed_utf8.length str in
  let arr = Array.create len dummy in
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
  | B_fg of Lt_style.color
  | E_fg
  | B_bg of Lt_style.color
  | E_bg

type markup = item list

type eval_stack = {
  mutable q_bold : bool option list;
  mutable q_underline : bool option list;
  mutable q_blink : bool option list;
  mutable q_reverse : bool option list;
  mutable q_fg : Lt_style.color option list;
  mutable q_bg : Lt_style.color option list;
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
  let arr = Array.create (markup_length markup) dummy in
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
