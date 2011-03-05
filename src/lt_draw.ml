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
  mutable underlined : bool;
  mutable blink : bool;
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
            underlined = false;
            blink = false;
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
      point.underlined <- false;
      point.blink <- false;
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

type style_mask = {
  mutable sm_bold : bool;
  mutable sm_underlined : bool;
  mutable sm_blink : bool;
  mutable sm_hidden : bool;
  mutable sm_foreground : Lt_style.color option;
  mutable sm_background : Lt_style.color option;
}

let draw_styled ctx line column txt =
  let sm = {
    sm_bold = false;
    sm_underlined = false;
    sm_blink = false;
    sm_hidden = false;
    sm_foreground = None;
    sm_background = None;
  } in
  let rec loop_string line column str ofs txt =
    if ofs < String.length str then begin
      let ch, ofs = Zed_utf8.unsafe_extract_next str ofs in
      if ch = newline then
        loop_string (line + 1) ctx.column1 str ofs txt
      else begin
        if line >= ctx.line1 && line < ctx.line2 && column >= ctx.column1 && column < ctx.column2 then begin
          let point = Array.unsafe_get (Array.unsafe_get ctx.matrix line) column in
          if not sm.sm_hidden then point.char <- ch;
          if sm.sm_bold then point.bold <- true;
          if sm.sm_underlined then point.underlined <- true;
          if sm.sm_blink then point.blink <- true;
          (match sm.sm_foreground with
             | Some c -> point.foreground <- c
             | none -> ());
          (match sm.sm_background with
             | Some c -> point.background <- c
             | none -> ())
        end;
        loop_string line (column + 1) str ofs txt
      end
    end else
      loop line column txt
  and loop line column = function
    | [] ->
        ()
    | String str :: rest ->
        loop_string line column str 0 rest
    | Reset :: rest ->
        sm.sm_bold <- false;
        sm.sm_underlined <- false;
        sm.sm_blink <- false;
        sm.sm_hidden <- false;
        sm.sm_foreground <- None;
        sm.sm_background <- None;
        loop line column rest
    | Bold :: rest ->
        sm.sm_bold <- true;
        loop line column rest
    | Underlined :: rest ->
        sm.sm_underlined <- true;
        loop line column rest
    | Blink :: rest ->
        sm.sm_blink <- true;
        loop line column rest
    | Inverse :: rest ->
        let foreground = sm.sm_foreground and background = sm.sm_background in
        sm.sm_foreground <- background;
        sm.sm_background <- foreground;
        loop line column rest
    | Hidden :: rest ->
        sm.sm_hidden <- true;
        loop line column rest
    | Foreground color :: rest ->
        sm.sm_foreground <- Some color;
        loop line column rest
    | Background color :: rest ->
        sm.sm_background <- Some color;
        loop line column rest
  in
  loop (ctx.line1 + line) (ctx.column1 + column) txt
