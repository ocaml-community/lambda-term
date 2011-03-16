(*
 * lt_draw.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lt_geom
open Lt_style

let unsafe_get matrix line column =
  Array.unsafe_get (Array.unsafe_get matrix line) column

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
    size.Lt_geom.lines
    (fun _ ->
       Array.init
         size.Lt_geom.columns
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
      let point = unsafe_get ctx.matrix line column in
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
      (unsafe_get ctx.matrix line column).char <- ch
    done
  done

let point ctx line column =
  if line < 0 || column < 0 then raise Out_of_bounds;
  let line = ctx.line1 + line and column = ctx.line1 + column in
  if line >= ctx.line2 || column >= ctx.column2 then raise Out_of_bounds;
  unsafe_get ctx.matrix line column

let draw_char ctx line column ch =
  if line >= 0 && column >= 0 then begin
    let line = ctx.line1 + line and column = ctx.column1 + column in
    if line < ctx.line2 && column < ctx.column2 then
      (unsafe_get ctx.matrix line column).char <- ch
  end

let draw_string ctx line column str =
  let rec loop line column ofs =
    if ofs < String.length str then begin
      let ch, ofs = Zed_utf8.unsafe_extract_next str ofs in
      if ch = newline then
        loop (line + 1) ctx.column1 ofs
      else begin
        if line >= ctx.line1 && line < ctx.line2 && column >= ctx.column1 && column < ctx.column2 then
          (unsafe_get ctx.matrix line column).char <- ch;
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
          let point = unsafe_get ctx.matrix line column in
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

type connection =
  | Blank
  | Light
  | Heavy

type piece = { top : connection; bottom : connection; left : connection; right : connection }

let piece_of_char char =
  match UChar.code char with
    | 0x2500 -> Some { top = Blank; bottom = Blank; left = Light; right = Light }
    | 0x2501 -> Some { top = Blank; bottom = Blank; left = Heavy; right = Heavy }
    | 0x2502 -> Some { top = Light; bottom = Light; left = Blank; right = Blank }
    | 0x2503 -> Some { top = Heavy; bottom = Heavy; left = Blank; right = Blank }
    | 0x250c -> Some { top = Blank; bottom = Light; left = Blank; right = Light }
    | 0x250d -> Some { top = Blank; bottom = Light; left = Blank; right = Heavy }
    | 0x250e -> Some { top = Blank; bottom = Heavy; left = Blank; right = Light }
    | 0x250f -> Some { top = Blank; bottom = Heavy; left = Blank; right = Heavy }
    | 0x2510 -> Some { top = Blank; bottom = Light; left = Light; right = Blank }
    | 0x2511 -> Some { top = Blank; bottom = Light; left = Heavy; right = Blank }
    | 0x2512 -> Some { top = Blank; bottom = Heavy; left = Light; right = Blank }
    | 0x2513 -> Some { top = Blank; bottom = Heavy; left = Heavy; right = Blank }
    | 0x2514 -> Some { top = Light; bottom = Blank; left = Blank; right = Light }
    | 0x2515 -> Some { top = Light; bottom = Blank; left = Blank; right = Heavy }
    | 0x2516 -> Some { top = Heavy; bottom = Blank; left = Blank; right = Light }
    | 0x2517 -> Some { top = Heavy; bottom = Blank; left = Blank; right = Heavy }
    | 0x2518 -> Some { top = Light; bottom = Blank; left = Light; right = Blank }
    | 0x2519 -> Some { top = Light; bottom = Blank; left = Heavy; right = Blank }
    | 0x251a -> Some { top = Heavy; bottom = Blank; left = Light; right = Blank }
    | 0x251b -> Some { top = Heavy; bottom = Blank; left = Heavy; right = Blank }
    | 0x251c -> Some { top = Light; bottom = Light; left = Blank; right = Light }
    | 0x251d -> Some { top = Light; bottom = Light; left = Blank; right = Heavy }
    | 0x251e -> Some { top = Heavy; bottom = Light; left = Blank; right = Light }
    | 0x251f -> Some { top = Light; bottom = Heavy; left = Blank; right = Light }
    | 0x2520 -> Some { top = Heavy; bottom = Heavy; left = Blank; right = Light }
    | 0x2521 -> Some { top = Heavy; bottom = Light; left = Blank; right = Heavy }
    | 0x2522 -> Some { top = Light; bottom = Heavy; left = Blank; right = Heavy }
    | 0x2523 -> Some { top = Heavy; bottom = Heavy; left = Blank; right = Heavy }
    | 0x2524 -> Some { top = Light; bottom = Light; left = Light; right = Blank }
    | 0x2525 -> Some { top = Light; bottom = Light; left = Heavy; right = Blank }
    | 0x2526 -> Some { top = Heavy; bottom = Light; left = Light; right = Blank }
    | 0x2527 -> Some { top = Light; bottom = Heavy; left = Light; right = Blank }
    | 0x2528 -> Some { top = Heavy; bottom = Heavy; left = Light; right = Blank }
    | 0x2529 -> Some { top = Heavy; bottom = Light; left = Heavy; right = Blank }
    | 0x252a -> Some { top = Light; bottom = Heavy; left = Heavy; right = Blank }
    | 0x252b -> Some { top = Heavy; bottom = Heavy; left = Heavy; right = Blank }
    | 0x252c -> Some { top = Blank; bottom = Light; left = Light; right = Light }
    | 0x252d -> Some { top = Blank; bottom = Light; left = Heavy; right = Light }
    | 0x252e -> Some { top = Blank; bottom = Light; left = Light; right = Heavy }
    | 0x252f -> Some { top = Blank; bottom = Light; left = Heavy; right = Heavy }
    | 0x2530 -> Some { top = Blank; bottom = Heavy; left = Light; right = Light }
    | 0x2531 -> Some { top = Blank; bottom = Heavy; left = Heavy; right = Light }
    | 0x2532 -> Some { top = Blank; bottom = Heavy; left = Light; right = Heavy }
    | 0x2533 -> Some { top = Blank; bottom = Heavy; left = Heavy; right = Heavy }
    | 0x2534 -> Some { top = Light; bottom = Blank; left = Light; right = Light }
    | 0x2535 -> Some { top = Light; bottom = Blank; left = Heavy; right = Light }
    | 0x2536 -> Some { top = Light; bottom = Blank; left = Light; right = Heavy }
    | 0x2537 -> Some { top = Light; bottom = Blank; left = Heavy; right = Heavy }
    | 0x2538 -> Some { top = Heavy; bottom = Blank; left = Light; right = Light }
    | 0x2539 -> Some { top = Heavy; bottom = Blank; left = Heavy; right = Light }
    | 0x253a -> Some { top = Heavy; bottom = Blank; left = Light; right = Heavy }
    | 0x253b -> Some { top = Heavy; bottom = Blank; left = Heavy; right = Heavy }
    | 0x253c -> Some { top = Light; bottom = Light; left = Light; right = Light }
    | 0x253d -> Some { top = Light; bottom = Light; left = Heavy; right = Light }
    | 0x253e -> Some { top = Light; bottom = Light; left = Light; right = Heavy }
    | 0x253f -> Some { top = Light; bottom = Light; left = Heavy; right = Heavy }
    | 0x2540 -> Some { top = Heavy; bottom = Light; left = Light; right = Light }
    | 0x2541 -> Some { top = Light; bottom = Heavy; left = Light; right = Light }
    | 0x2542 -> Some { top = Heavy; bottom = Heavy; left = Light; right = Light }
    | 0x2543 -> Some { top = Heavy; bottom = Light; left = Heavy; right = Light }
    | 0x2544 -> Some { top = Heavy; bottom = Light; left = Light; right = Heavy }
    | 0x2545 -> Some { top = Light; bottom = Heavy; left = Heavy; right = Light }
    | 0x2546 -> Some { top = Light; bottom = Heavy; left = Light; right = Heavy }
    | 0x2547 -> Some { top = Heavy; bottom = Light; left = Heavy; right = Heavy }
    | 0x2548 -> Some { top = Light; bottom = Heavy; left = Heavy; right = Heavy }
    | 0x2549 -> Some { top = Heavy; bottom = Heavy; left = Heavy; right = Light }
    | 0x254a -> Some { top = Heavy; bottom = Heavy; left = Light; right = Heavy }
    | 0x254b -> Some { top = Heavy; bottom = Heavy; left = Heavy; right = Heavy }
    | 0x2574 -> Some { top = Blank; bottom = Blank; left = Light; right = Blank }
    | 0x2575 -> Some { top = Light; bottom = Blank; left = Blank; right = Blank }
    | 0x2576 -> Some { top = Blank; bottom = Blank; left = Blank; right = Light }
    | 0x2577 -> Some { top = Blank; bottom = Light; left = Blank; right = Blank }
    | 0x2578 -> Some { top = Blank; bottom = Blank; left = Heavy; right = Blank }
    | 0x2579 -> Some { top = Heavy; bottom = Blank; left = Blank; right = Blank }
    | 0x257a -> Some { top = Blank; bottom = Blank; left = Blank; right = Heavy }
    | 0x257b -> Some { top = Blank; bottom = Heavy; left = Blank; right = Blank }
    | 0x257c -> Some { top = Blank; bottom = Blank; left = Light; right = Heavy }
    | 0x257d -> Some { top = Light; bottom = Heavy; left = Blank; right = Blank }
    | 0x257e -> Some { top = Blank; bottom = Blank; left = Heavy; right = Light }
    | 0x257f -> Some { top = Heavy; bottom = Light; left = Blank; right = Blank }
    | _ -> None

let char_of_piece = function
  | { top = Blank; bottom = Blank; left = Blank; right = Blank } -> UChar.of_int 0x0020
  | { top = Blank; bottom = Blank; left = Light; right = Light } -> UChar.of_int 0x2500
  | { top = Blank; bottom = Blank; left = Heavy; right = Heavy } -> UChar.of_int 0x2501
  | { top = Light; bottom = Light; left = Blank; right = Blank } -> UChar.of_int 0x2502
  | { top = Heavy; bottom = Heavy; left = Blank; right = Blank } -> UChar.of_int 0x2503
  | { top = Blank; bottom = Light; left = Blank; right = Light } -> UChar.of_int 0x250c
  | { top = Blank; bottom = Light; left = Blank; right = Heavy } -> UChar.of_int 0x250d
  | { top = Blank; bottom = Heavy; left = Blank; right = Light } -> UChar.of_int 0x250e
  | { top = Blank; bottom = Heavy; left = Blank; right = Heavy } -> UChar.of_int 0x250f
  | { top = Blank; bottom = Light; left = Light; right = Blank } -> UChar.of_int 0x2510
  | { top = Blank; bottom = Light; left = Heavy; right = Blank } -> UChar.of_int 0x2511
  | { top = Blank; bottom = Heavy; left = Light; right = Blank } -> UChar.of_int 0x2512
  | { top = Blank; bottom = Heavy; left = Heavy; right = Blank } -> UChar.of_int 0x2513
  | { top = Light; bottom = Blank; left = Blank; right = Light } -> UChar.of_int 0x2514
  | { top = Light; bottom = Blank; left = Blank; right = Heavy } -> UChar.of_int 0x2515
  | { top = Heavy; bottom = Blank; left = Blank; right = Light } -> UChar.of_int 0x2516
  | { top = Heavy; bottom = Blank; left = Blank; right = Heavy } -> UChar.of_int 0x2517
  | { top = Light; bottom = Blank; left = Light; right = Blank } -> UChar.of_int 0x2518
  | { top = Light; bottom = Blank; left = Heavy; right = Blank } -> UChar.of_int 0x2519
  | { top = Heavy; bottom = Blank; left = Light; right = Blank } -> UChar.of_int 0x251a
  | { top = Heavy; bottom = Blank; left = Heavy; right = Blank } -> UChar.of_int 0x251b
  | { top = Light; bottom = Light; left = Blank; right = Light } -> UChar.of_int 0x251c
  | { top = Light; bottom = Light; left = Blank; right = Heavy } -> UChar.of_int 0x251d
  | { top = Heavy; bottom = Light; left = Blank; right = Light } -> UChar.of_int 0x251e
  | { top = Light; bottom = Heavy; left = Blank; right = Light } -> UChar.of_int 0x251f
  | { top = Heavy; bottom = Heavy; left = Blank; right = Light } -> UChar.of_int 0x2520
  | { top = Heavy; bottom = Light; left = Blank; right = Heavy } -> UChar.of_int 0x2521
  | { top = Light; bottom = Heavy; left = Blank; right = Heavy } -> UChar.of_int 0x2522
  | { top = Heavy; bottom = Heavy; left = Blank; right = Heavy } -> UChar.of_int 0x2523
  | { top = Light; bottom = Light; left = Light; right = Blank } -> UChar.of_int 0x2524
  | { top = Light; bottom = Light; left = Heavy; right = Blank } -> UChar.of_int 0x2525
  | { top = Heavy; bottom = Light; left = Light; right = Blank } -> UChar.of_int 0x2526
  | { top = Light; bottom = Heavy; left = Light; right = Blank } -> UChar.of_int 0x2527
  | { top = Heavy; bottom = Heavy; left = Light; right = Blank } -> UChar.of_int 0x2528
  | { top = Heavy; bottom = Light; left = Heavy; right = Blank } -> UChar.of_int 0x2529
  | { top = Light; bottom = Heavy; left = Heavy; right = Blank } -> UChar.of_int 0x252a
  | { top = Heavy; bottom = Heavy; left = Heavy; right = Blank } -> UChar.of_int 0x252b
  | { top = Blank; bottom = Light; left = Light; right = Light } -> UChar.of_int 0x252c
  | { top = Blank; bottom = Light; left = Heavy; right = Light } -> UChar.of_int 0x252d
  | { top = Blank; bottom = Light; left = Light; right = Heavy } -> UChar.of_int 0x252e
  | { top = Blank; bottom = Light; left = Heavy; right = Heavy } -> UChar.of_int 0x252f
  | { top = Blank; bottom = Heavy; left = Light; right = Light } -> UChar.of_int 0x2530
  | { top = Blank; bottom = Heavy; left = Heavy; right = Light } -> UChar.of_int 0x2531
  | { top = Blank; bottom = Heavy; left = Light; right = Heavy } -> UChar.of_int 0x2532
  | { top = Blank; bottom = Heavy; left = Heavy; right = Heavy } -> UChar.of_int 0x2533
  | { top = Light; bottom = Blank; left = Light; right = Light } -> UChar.of_int 0x2534
  | { top = Light; bottom = Blank; left = Heavy; right = Light } -> UChar.of_int 0x2535
  | { top = Light; bottom = Blank; left = Light; right = Heavy } -> UChar.of_int 0x2536
  | { top = Light; bottom = Blank; left = Heavy; right = Heavy } -> UChar.of_int 0x2537
  | { top = Heavy; bottom = Blank; left = Light; right = Light } -> UChar.of_int 0x2538
  | { top = Heavy; bottom = Blank; left = Heavy; right = Light } -> UChar.of_int 0x2539
  | { top = Heavy; bottom = Blank; left = Light; right = Heavy } -> UChar.of_int 0x253a
  | { top = Heavy; bottom = Blank; left = Heavy; right = Heavy } -> UChar.of_int 0x253b
  | { top = Light; bottom = Light; left = Light; right = Light } -> UChar.of_int 0x253c
  | { top = Light; bottom = Light; left = Heavy; right = Light } -> UChar.of_int 0x253d
  | { top = Light; bottom = Light; left = Light; right = Heavy } -> UChar.of_int 0x253e
  | { top = Light; bottom = Light; left = Heavy; right = Heavy } -> UChar.of_int 0x253f
  | { top = Heavy; bottom = Light; left = Light; right = Light } -> UChar.of_int 0x2540
  | { top = Light; bottom = Heavy; left = Light; right = Light } -> UChar.of_int 0x2541
  | { top = Heavy; bottom = Heavy; left = Light; right = Light } -> UChar.of_int 0x2542
  | { top = Heavy; bottom = Light; left = Heavy; right = Light } -> UChar.of_int 0x2543
  | { top = Heavy; bottom = Light; left = Light; right = Heavy } -> UChar.of_int 0x2544
  | { top = Light; bottom = Heavy; left = Heavy; right = Light } -> UChar.of_int 0x2545
  | { top = Light; bottom = Heavy; left = Light; right = Heavy } -> UChar.of_int 0x2546
  | { top = Heavy; bottom = Light; left = Heavy; right = Heavy } -> UChar.of_int 0x2547
  | { top = Light; bottom = Heavy; left = Heavy; right = Heavy } -> UChar.of_int 0x2548
  | { top = Heavy; bottom = Heavy; left = Heavy; right = Light } -> UChar.of_int 0x2549
  | { top = Heavy; bottom = Heavy; left = Light; right = Heavy } -> UChar.of_int 0x254a
  | { top = Heavy; bottom = Heavy; left = Heavy; right = Heavy } -> UChar.of_int 0x254b
  | { top = Blank; bottom = Blank; left = Light; right = Blank } -> UChar.of_int 0x2574
  | { top = Light; bottom = Blank; left = Blank; right = Blank } -> UChar.of_int 0x2575
  | { top = Blank; bottom = Blank; left = Blank; right = Light } -> UChar.of_int 0x2576
  | { top = Blank; bottom = Light; left = Blank; right = Blank } -> UChar.of_int 0x2577
  | { top = Blank; bottom = Blank; left = Heavy; right = Blank } -> UChar.of_int 0x2578
  | { top = Heavy; bottom = Blank; left = Blank; right = Blank } -> UChar.of_int 0x2579
  | { top = Blank; bottom = Blank; left = Blank; right = Heavy } -> UChar.of_int 0x257a
  | { top = Blank; bottom = Heavy; left = Blank; right = Blank } -> UChar.of_int 0x257b
  | { top = Blank; bottom = Blank; left = Light; right = Heavy } -> UChar.of_int 0x257c
  | { top = Light; bottom = Heavy; left = Blank; right = Blank } -> UChar.of_int 0x257d
  | { top = Blank; bottom = Blank; left = Heavy; right = Light } -> UChar.of_int 0x257e
  | { top = Heavy; bottom = Light; left = Blank; right = Blank } -> UChar.of_int 0x257f

let draw_piece ctx line column piece =
  let line = ctx.line1 + line and column = ctx.column1 + column in
  if line >= ctx.line1 && column >= ctx.column1 && line < ctx.line2 && column < ctx.column2 then begin
    let piece =
      if line > ctx.line1 then begin
        let point = unsafe_get ctx.matrix (line - 1) column in
        match piece_of_char point.char with
          | None ->
              piece
          | Some piece' ->
              if piece.top = piece'.bottom then
                piece
              else if piece.top = Blank then
                { piece with top = piece'.bottom }
              else if piece'.bottom = Blank then begin
                point.char <- char_of_piece { piece' with bottom = piece.top };
                piece
              end else
                piece
      end else
        piece
    in
    let piece =
      if line < ctx.line2 - 1 then begin
        let point = unsafe_get ctx.matrix (line + 1) column in
        match piece_of_char point.char with
          | None ->
              piece
          | Some piece' ->
              if piece.bottom = piece'.top then
                piece
              else if piece.bottom = Blank then
                { piece with bottom = piece'.top }
              else if piece'.top = Blank then begin
                point.char <- char_of_piece { piece' with top = piece.bottom };
                piece
              end else
                piece
      end else
        piece
    in
    let piece =
      if column > ctx.column1 then begin
        let point = unsafe_get ctx.matrix line (column - 1) in
        match piece_of_char point.char with
          | None ->
              piece
          | Some piece' ->
              if piece.left = piece'.right then
                piece
              else if piece.left = Blank then
                { piece with left = piece'.right }
              else if piece'.right = Blank then begin
                point.char <- char_of_piece { piece' with right = piece.left };
                piece
              end else
                piece
      end else
        piece
    in
    let piece =
      if column < ctx.column2 - 1 then begin
        let point = unsafe_get ctx.matrix line (column + 1) in
        match piece_of_char point.char with
          | None ->
              piece
          | Some piece' ->
              if piece.right = piece'.left then
                piece
              else if piece.right = Blank then
                { piece with right = piece'.left }
              else if piece'.left = Blank then begin
                point.char <- char_of_piece { piece' with left = piece.right };
                piece
              end else
                piece
      end else
        piece
    in
    (unsafe_get ctx.matrix line column).char <- char_of_piece piece
  end

let hline = { top = Blank; bottom = Blank; left = Light; right = Light }
let vline = { top = Light; bottom = Light; left = Blank; right = Blank }
let tl_corner = { top = Blank; bottom = Light; left = Blank; right = Light }
let tr_corner = { top = Blank; bottom = Light; left = Light; right = Blank }
let bl_corner = { top = Light; bottom = Blank; left = Blank; right = Light }
let br_corner = { top = Light; bottom = Blank; left = Light; right = Blank }

let draw_hline ctx line column len =
  for i = 0 to len - 1 do
    draw_piece ctx line (column + i) hline
  done

let draw_vline ctx line column len =
  for i = 0 to len - 1 do
    draw_piece ctx (line + i) column vline
  done

let draw_frame ctx rect =
  for i = 1 to rect.r_columns - 2 do
    draw_piece ctx rect.r_line (rect.r_column + i) hline;
    draw_piece ctx (rect.r_line + rect.r_lines - 1) (rect.r_column + i) hline
  done;
  for i = 1 to rect.r_lines - 2 do
    draw_piece ctx (rect.r_line + i) rect.r_column vline;
    draw_piece ctx (rect.r_line + i) (rect.r_column + rect.r_columns - 1) vline
  done;
  draw_piece ctx rect.r_line rect.r_column tl_corner;
  draw_piece ctx rect.r_line (rect.r_column + rect.r_columns - 1) tr_corner;
  draw_piece ctx (rect.r_line + rect.r_lines - 1) (rect.r_column + rect.r_columns - 1) br_corner;
  draw_piece ctx (rect.r_line + rect.r_lines - 1) rect.r_column bl_corner
