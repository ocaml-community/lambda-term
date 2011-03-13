(*
 * lt_text.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile

type t = (UChar.t * Lt_style.t) array

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
