(*
 * lt_draw.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lt_types
open Lt_style

type point = {
  mutable char : UChar.t;
  mutable bold : bool;
  mutable underline : bool;
  mutable blink : bool;
  mutable reverse : bool;
  mutable foreground : Lt_style.color;
  mutable background : Lt_style.color;
}

type matrix = point array array

let make_matrix size =
  Array.init
    size.Lt_types.lines
    (fun _ ->
       Array.init
         size.Lt_types.columns
         (fun _ -> {
            char = UChar.of_char ' ';
            bold = false;
            underline = false;
            blink = false;
            reverse = false;
            foreground = Lt_style.default;
            background = Lt_style.default;
          }))

type context = {
  matrix : matrix;
  line1 : int;
  column1 : int;
  line2 : int;
  column2 : int;
}

let context m s =
  if Array.length m <> s.lines then invalid_arg "Lt_draw.context";
  Array.iter (fun l -> if Array.length l <> s.columns then invalid_arg "Lt_draw.context") m;
  { matrix = m; line1 = 0; column1 = 0; line2 = s.lines; column2 = s.columns }

let size ctx = {
  lines = ctx.line2 - ctx.line1;
  columns = ctx.column2 - ctx.column1;
}

exception Out_of_bounds

let sub ctx rect =
  if rect.r_line < 0 || rect.r_column < 0 || rect.r_lines < 0 || rect.r_columns < 0 then raise Out_of_bounds;
  let line1 = ctx.line1 + rect.r_line and column1 = ctx.column1 + rect.r_column in
  let line2 = line1 + rect.r_lines and column2 = column1 + rect.r_columns in
  if line2 > ctx.line2 || column2 > ctx.column2 then raise Out_of_bounds;
  { ctx with line1; column1; line2; column2 }

let space = UChar.of_char ' '
let newline = UChar.of_char '\n'

let clear ctx =
  for line = ctx.line1 to ctx.line2 - 1 do
    for column = ctx.column1 to ctx.column2 - 1 do
      let point = Array.unsafe_get (Array.unsafe_get ctx.matrix line) column in
      point.char <- space;
      point.bold <- false;
      point.underline <- false;
      point.blink <- false;
      point.reverse <- false;
      point.foreground <- Lt_style.default;
      point.background <- Lt_style.default
    done
  done

let fill ctx ch =
  for line = ctx.line1 to ctx.line2 - 1 do
    for column = ctx.column1 to ctx.column2 - 1 do
      (Array.unsafe_get (Array.unsafe_get ctx.matrix line) column).char <- ch
    done
  done

let point ctx line column =
  if line < 0 || column < 0 then raise Out_of_bounds;
  let line = ctx.line1 + line and column = ctx.line1 + column in
  if line >= ctx.line2 || column >= ctx.column2 then raise Out_of_bounds;
  Array.unsafe_get (Array.unsafe_get ctx.matrix line) column

let draw_char ctx line column ch =
  if line >= 0 || column >= 0 then begin
    let line = ctx.line1 + line and column = ctx.column1 + column in
    if line < ctx.line2 || column < ctx.column2 then
      (Array.unsafe_get (Array.unsafe_get ctx.matrix line) column).char <- ch
  end

let draw_string ctx line column str =
  let rec loop line column ofs =
    if ofs < String.length str then begin
      let ch, ofs = Zed_utf8.unsafe_extract_next str ofs in
      if ch = newline then
        loop (line + 1) ctx.column1 ofs
      else begin
        if line >= ctx.line1 && line < ctx.line2 && column >= ctx.column1 && column < ctx.column2 then
          (Array.unsafe_get (Array.unsafe_get ctx.matrix line) column).char <- ch;
        loop line (column + 1) ofs
      end
    end
  in
  loop (ctx.line1 + line) (ctx.column1 + column) 0

let draw_styled ctx line column str =
  let rec loop line column idx =
    if idx < Array.length str then begin
      let ch, style = Array.unsafe_get str idx in
      if ch = newline then
        loop (line + 1) ctx.column1 (idx + 1)
      else begin
        if line >= ctx.line1 && line < ctx.line2 && column >= ctx.column1 && column < ctx.column2 then begin
          let point = Array.unsafe_get (Array.unsafe_get ctx.matrix line) column in
          point.char <- ch;
          begin
            match Lt_style.bold style with
              | Some x -> point.bold <- x
              | None -> ()
          end;
          begin
            match Lt_style.underline style with
              | Some x -> point.underline <- x
              | None -> ()
          end;
          begin
            match Lt_style.blink style with
              | Some x -> point.blink <- x
              | None -> ()
          end;
          begin
            match Lt_style.reverse style with
              | Some x -> point.reverse <- x
              | None -> ()
          end;
          begin
            match Lt_style.foreground style with
              | Some x -> point.foreground <- x
              | None -> ()
          end;
          begin
            match Lt_style.background style with
              | Some x -> point.background <- x
              | None -> ()
          end
        end;
        loop line (column + 1) (idx + 1)
      end
    end
  in
  loop (ctx.line1 + line) (ctx.column1 + column) 0
