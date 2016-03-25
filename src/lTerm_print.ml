
open Zed
open Core.Std
open Async.Std

type t =
  { writer         : Writer.t sexp_opaque
  ; model          : string
  ; colors         : int
  ; color_map      : LTerm_color_mappings.map sexp_opaque
  ; bold_is_bright : bool
  }
[@@deriving fields, sexp]

let create ?(model=LTerm.default_model) writer =
  let colors = LTerm.colors_of_term model in
  let color_map =
    match colors with
    | 16  -> LTerm_color_mappings.colors_16
    | 88  -> LTerm_color_mappings.colors_88
    | 256 -> LTerm_color_mappings.colors_256
    | _   -> assert false
  in
  { writer
  ; model
  ; colors
  ; color_map
  ; bold_is_bright = LTerm.bold_is_bright model }
;;

let flushed t = Writer.flushed t.writer

let stdout = lazy(create (Lazy.force Writer.stdout))
let stderr = lazy(create (Lazy.force Writer.stderr))

module Codes = struct
  let _reset     = '0'
  let _bold      = '1'
  let _underline = '4'
  let _blink     = '5'
  let _reverse   = '7'
  let foreground = 30
  let background = 40
end

let add_index t base n =
  if n < 8 then
    Writer.writef t.writer ";%u" (base + n)
  else if n < 16 &&  t.bold_is_bright then
    if base = Codes.foreground then
      Writer.writef t.writer ";1;%u" (base + n - 8)
    else
      Writer.writef t.writer ";%u" (base + n - 8)
  else
    Writer.writef t.writer ";%u;5;%u" (base + 8) n
;;

let add_color t base col =
  match LTerm_style.Color.kind col with
  | Default | Transparent  -> ()
  | Index | RGB -> add_index t base (LTerm_style.Color.get_index col t.color_map)
;;

let is_on : LTerm_style.Switch.t -> bool = function
  | On -> true
  | Off | Unset -> false
;;

let set_style t style =
  Writer.write t.writer "\027[0";
  if is_on (LTerm_style.bold      style) then Writer.write t.writer ";1";
  if is_on (LTerm_style.underline style) then Writer.write t.writer ";4";
  if is_on (LTerm_style.blink     style) then Writer.write t.writer ";5";
  if is_on (LTerm_style.reverse   style) then Writer.write t.writer ";7";
  add_color t Codes.foreground (LTerm_style.foreground style);
  add_color t Codes.background (LTerm_style.background style);
  Writer.write_char t.writer 'm'
;;

let fprint t (txt:LTerm_text.t) =
  let len = Array.length txt in
  if len > 0 then begin
    let curr_style = ref (LTerm_style.on_default txt.(0).style) in
    set_style t !curr_style;
    for i = 0 to len - 1 do
      let { LTerm_text. char; style } = txt.(i) in
      let style = LTerm_style.on_default style in
      if not (LTerm_style.equal !curr_style style) then begin
        set_style t style;
        curr_style := style
      end;
      if Uchar.code char < 128 then
        Writer.write_char t.writer (Uchar.to_char char)
      else
        Writer.write t.writer (Zed_utf8.singleton char)
    done;
    Writer.write t.writer "\027[0m"
  end
;;

let  print txt = fprint (Lazy.force stdout) txt
let eprint txt = fprint (Lazy.force stderr) txt

let fprintf t fmt = LTerm_text.ktprintf (fprint t) fmt

let  printf fmt = fprintf (Lazy.force stdout) fmt
let eprintf fmt = fprintf (Lazy.force stderr) fmt
