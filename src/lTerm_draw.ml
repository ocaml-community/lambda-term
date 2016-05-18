(*
 * lTerm_draw.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open StdLabels

module Geom  = LTerm_geom
module Style = LTerm_style

let space   = Uchar.of_char ' '
let newline = Uchar.of_char '\n'

type point = LTerm_matrix_private.point =
  { mutable char       : Uchar.t
  ; mutable switches   : Style.Switches.t
  ; mutable foreground : Style.Color.t
  ; mutable background : Style.Color.t
  }

type context = LTerm_matrix_private.context =
  { matrix          : point array array
  ; row1            : int
  ; col1            : int
  ; row2            : int
  ; col2            : int
  ; hidden_newlines : bool
  }

module Matrix = LTerm_matrix_private

let size ctx : Geom.size =
  { rows = ctx.row2 - ctx.row1
  ; cols = ctx.col2 - ctx.col1
  }

exception Out_of_bounds

let sub ctx (rect:Geom.rect) =
  if rect.row1 < 0 || rect.col1 < 0 || rect.row1 > rect.row2 || rect.col1 > rect.col2 then
    raise Out_of_bounds;
  let row1 = ctx.row1 + rect.row1
  and col1 = ctx.col1 + rect.col1
  and row2 = ctx.row1 + rect.row2
  and col2 = ctx.col1 + rect.col2 in
  if row2 > ctx.row2 || col2 > ctx.col2 then raise Out_of_bounds;
  { ctx with row1; col1; row2; col2; hidden_newlines = false }

let clear ctx =
  for row = ctx.row1 to ctx.row2 - 1 do
    for col = ctx.col1 to ctx.col2 - (if ctx.hidden_newlines then 0 else 1) do
      let point = ctx.matrix.(row).(col) in
      point.char       <- space;
      point.switches   <- Style.Switches.default;
      point.foreground <- Style.Color.default;
      point.background <- Style.Color.default;
    done
  done

let point ctx ~row ~col =
  if row < 0 || col < 0 then raise Out_of_bounds;
  let row = ctx.row1 + row and col = ctx.col1 + col in
  if row >= ctx.row2 || col >= ctx.col2 then raise Out_of_bounds;
  ctx.matrix.(row).(col)

let get ctx ~row ~col = (point ctx ~row ~col).char

let get_style ctx ~row ~col =
  let p = point ctx ~row ~col in
  Style.make' ~switches:p.switches ~foreground:p.foreground ~background:p.background

let default_style = Style.none

let set_point_style pt ~style =
  pt.switches   <- Style.Switches.merge pt.switches (Style.switches   style);
  pt.foreground <- Style.Color.merge pt.foreground  (Style.foreground style);
  pt.background <- Style.Color.merge pt.background  (Style.background style);
;;

let set ctx ~row ~col ?(style=default_style) char =
  let abs_row = ctx.row1 + row and abs_col = ctx.col1 + col in
  if row >= 0 && col >= 0 && abs_row < ctx.row2 && abs_col < ctx.col2 then begin
    let pt = ctx.matrix.(row).(col) in
    pt.char <- char;
    set_point_style pt ~style
  end
;;

let set_style ctx ~row ~col style =
  let abs_row = ctx.row1 + row and abs_col = ctx.col1 + col in
  if row >= 0 && col >= 0 && abs_row < ctx.row2 && abs_col < ctx.col2 then begin
    let pt = ctx.matrix.(row).(col) in
    set_point_style pt ~style
  end
;;

let set_hidden_newline ctx ~row state =
  if not ctx.hidden_newlines then
    invalid_arg "LTerm_draw.set_hidden_newline";
  if row < 0 then raise Out_of_bounds;
  let row = ctx.row1 + row in
  if row >= ctx.row2 then raise Out_of_bounds;
  let pt = ctx.matrix.(row).(ctx.row2) in
  pt.char <- if state then newline else space
;;

let fill ctx ?(style=default_style) ch =
  for row = ctx.row1 to ctx.row2 - 1 do
    for col = ctx.col1 to ctx.col2 - 1 do
      let point = ctx.matrix.(row).(col) in
      point.char <- ch;
      set_point_style point ~style
    done
  done
;;

let fill_style ctx ~style =
  for row = ctx.row1 to ctx.row2 - 1 do
    for col = ctx.col1 to ctx.col2 - 1 do
      let point = ctx.matrix.(row).(col) in
      set_point_style point ~style
    done
  done
;;

module type Text_drawing = sig
  type t

  val draw
    :  context
    -> row:int
    -> col:int
    -> ?style:Style.t
    -> t
    -> unit

  val draw_aligned
    :  context
    -> row:int
    -> align:Geom.Horz_alignment.t
    -> ?style:Style.t
    -> t
    -> unit
end

module Make_text_drawing(Text : sig
                           type t
                           val limit : t -> int
                           val char  : t -> int -> Uchar.t
                           val style : t -> int -> Style.t
                           val next  : t -> int -> int
                         end) : Text_drawing with type t = Text.t = struct
  type t = Text.t

  let draw ctx ~row ~col ?(style=default_style) txt =
    let rec loop row col ofs =
      if ofs < Text.limit txt then begin
        let ch = Text.char txt ofs in
        if ch = newline then begin
          if ctx.hidden_newlines && col = ctx.row2 then
            ctx.matrix.(row).(col).char <- newline;
          loop (row + 1) ctx.col1 (Text.next txt ofs)
        end else begin
          if row >= ctx.row1
          && row <  ctx.row2
          && col >= ctx.col1
          && col <  ctx.col2 then begin
            let point = ctx.matrix.(row).(col) in
            point.char <- ch;
            set_point_style point ~style;
            set_point_style point ~style:(Text.style txt ofs)
          end;
          loop row (col + 1) (Text.next txt ofs)
        end
      end
    in
    loop (ctx.row1 + row) (ctx.col1 + col) 0
  ;;

  let draw_aligned ctx ~row ~(align:Geom.Horz_alignment.t)
        ?(style=default_style) txt =
    let rec line_length ofs len =
      if ofs = Text.limit txt then
        len
      else
        let ch = Text.char txt ofs in
        if ch = newline then
          len
        else
          line_length (Text.next txt ofs) (len + 1)
    in
    let rec loop row col ofs =
      if ofs < Text.limit txt then begin
        let ch = Text.char txt ofs in
        if ch = newline then begin
          Text.next txt ofs
        end else begin
          if row >= ctx.row1
          && row <  ctx.row2
          && col >= ctx.col1
          && col <  ctx.col2 then begin
            let point = ctx.matrix.(row).(col) in
            point.char <- ch;
            set_point_style point ~style;
            set_point_style point ~style:(Text.style txt ofs)
          end;
          loop row (col + 1) (Text.next txt ofs)
        end
      end else
        ofs
    in
    let rec loop_lines row ofs =
      if ofs < Text.limit txt then begin
        let ofs =
          loop row
            (match align with
             | Left ->
               ctx.col1
             | Center ->
               ctx.col1 + (ctx.col2 - ctx.col1 - line_length ofs 0) / 2
             | Right ->
               ctx.col2 - line_length ofs 0)
            ofs
        in
        loop_lines (row + 1) ofs
      end
    in
    loop_lines (ctx.row1 + row) 0
  ;;
end

module UTF8 = Make_text_drawing(struct
  type t = Zed_utf8.t
  let limit = String.length
  let char = Zed_utf8.extract
  let style _ _ = Style.none
  let next = Zed_utf8.next
end)

module Latin1 = Make_text_drawing(struct
  type t = string
  let limit = String.length
  let char  s i = Uchar.of_char s.[i]
  let style _ _ = Style.none
  let next  _ i = i + 1
end)

module Text = Make_text_drawing(struct
  type t = LTerm_text.t
  let limit = Array.length
  let char  (t:t) i = t.(i).char
  let style (t:t) i = t.(i).style
  let next _ i = i + 1
end)

module Connection = struct
  type t =
    | Blank
    | Light
    | Heavy
    | Double
  [@@deriving sexp]
end

module Piece = struct
  type t = int

  let int_of_connection : Connection.t -> int = function
    | Blank  -> 0x00
    | Light  -> 0x01
    | Heavy  -> 0x03
    | Double -> 0x02
  ;;

  let connection_of_int : int -> Connection.t = function
    | 0x00 -> Blank
    | 0x01 -> Light
    | 0x03 -> Heavy
    | 0x02 -> Double
    | _ -> assert false
  ;;

  external swap_int32 : int32 -> int32 = "%bswap_int32"

  module Bits = struct
    let top    = 0x7f000000
    let left   = 0x007f0000
    let right  = 0x00007f00
    let bottom = 0x0000007f
  end

  let reverse t = Int32.to_int (swap_int32 (Int32.of_int t))

  let ( + ) = ( lor )
  let ( - ) a b = a land (lnot b)

  let make ~top ~bottom ~left ~right =
    int_of_connection top    lsl 24 +
    int_of_connection left   lsl 16 +
    int_of_connection right  lsl  8 +
    int_of_connection bottom
  ;;

  let top    t = connection_of_int (t lsr 24        )
  let left   t = connection_of_int (t lsr 16 land 15)
  let right  t = connection_of_int (t lsr  8 land 15)
  let bottom t = connection_of_int (t        land 15)

  let set_top    t x = t - Bits.top    + (int_of_connection x lsl 24)
  let set_left   t x = t - Bits.left   + (int_of_connection x lsl 16)
  let set_right  t x = t - Bits.right  + (int_of_connection x lsl  8)
  let set_bottom t x = t - Bits.bottom + (int_of_connection x       )

  let hline c = make ~top:Blank ~bottom:Blank ~left:c ~right:c
  let vline c = make ~top:c ~bottom:c ~left:Blank ~right:Blank

  let br c = make ~top:Blank ~bottom:c ~left:Blank ~right:c
  let bl c = make ~top:Blank ~bottom:c ~left:c ~right:Blank
  let tr c = make ~top:c ~bottom:Blank ~left:Blank ~right:c
  let tl c = make ~top:c ~bottom:Blank ~left:c ~right:Blank

  (* These two arrays are generated by gen/gen_pieces.ml *)

  let ucode_to_piece = [| 0x00010100; 0x00030300; 0x01000001; 0x03000003
                        ;         -1;         -1;         -1;         -1
                        ;         -1;         -1;         -1;         -1
                        ; 0x00000101; 0x00000301; 0x00000103; 0x00000303
                        ; 0x00010001; 0x00030001; 0x00010003; 0x00030003
                        ; 0x01000100; 0x01000300; 0x03000100; 0x03000300
                        ; 0x01010000; 0x01030000; 0x03010000; 0x03030000
                        ; 0x01000101; 0x01000301; 0x03000101; 0x01000103
                        ; 0x03000103; 0x03000301; 0x01000303; 0x03000303
                        ; 0x01010001; 0x01030001; 0x03010001; 0x01010003
                        ; 0x03010003; 0x03030001; 0x01030003; 0x03030003
                        ; 0x00010101; 0x00030101; 0x00010301; 0x00030301
                        ; 0x00010103; 0x00030103; 0x00010303; 0x00030303
                        ; 0x01010100; 0x01030100; 0x01010300; 0x01030300
                        ; 0x03010100; 0x03030100; 0x03010300; 0x03030300
                        ; 0x01010101; 0x01030101; 0x01010301; 0x01030301
                        ; 0x03010101; 0x01010103; 0x03010103; 0x03030101
                        ; 0x03010301; 0x01030103; 0x01010303; 0x03030301
                        ; 0x01030303; 0x03030103; 0x03010303; 0x03030303
                        ;         -1;         -1;         -1;         -1
                        ; 0x00020200; 0x02000002; 0x00000201; 0x00000102
                        ; 0x00000202; 0x00020001; 0x00010002; 0x00020002
                        ; 0x01000200; 0x02000100; 0x02000200; 0x01020000
                        ; 0x02010000; 0x02020000; 0x01000201; 0x02000102
                        ; 0x02000202; 0x01020001; 0x02010002; 0x02020002
                        ; 0x00020201; 0x00010102; 0x00020202; 0x01020200
                        ; 0x02010100; 0x02020200; 0x01020201; 0x02010102
                        ; 0x02020202;         -1;         -1;         -1
                        ;         -1;         -1;         -1;         -1
                        ; 0x00010000; 0x01000000; 0x00000100; 0x00000001
                        ; 0x00030000; 0x03000000; 0x00000300; 0x00000003
                        ; 0x00010300; 0x01000003; 0x00030100; 0x03000001
                       |]

  let piece_to_ucode = [| 0x0020; 0x2577; 0x257b; 0x257b
                        ; 0x2576; 0x250c; 0x2553; 0x250e
                        ; 0x257a; 0x2552; 0x2554; 0x2554
                        ; 0x257a; 0x250d; 0x2554; 0x250f
                        ; 0x2574; 0x2510; 0x2556; 0x2512
                        ; 0x2500; 0x252c; 0x2565; 0x2530
                        ; 0x2550; 0x2566; 0x2566; 0x2566
                        ; 0x257c; 0x252e; 0x2566; 0x2532
                        ; 0x2578; 0x2555; 0x2557; 0x2557
                        ; 0x2550; 0x2566; 0x2566; 0x2566
                        ; 0x2550; 0x2564; 0x2566; 0x2566
                        ; 0x2550; 0x2566; 0x2566; 0x2566
                        ; 0x2578; 0x2511; 0x2557; 0x2513
                        ; 0x257e; 0x252d; 0x2566; 0x2531
                        ; 0x2550; 0x2566; 0x2566; 0x2566
                        ; 0x2501; 0x252f; 0x2566; 0x2533
                        ; 0x2575; 0x2502; 0x2551; 0x257d
                        ; 0x2514; 0x251c; 0x2560; 0x251f
                        ; 0x2558; 0x255e; 0x2560; 0x2560
                        ; 0x2515; 0x251d; 0x2560; 0x2522
                        ; 0x2518; 0x2524; 0x2563; 0x2527
                        ; 0x2534; 0x253c; 0x256c; 0x2541
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2536; 0x253e; 0x256c; 0x2546
                        ; 0x255b; 0x2561; 0x2563; 0x2563
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2567; 0x256a; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2519; 0x2525; 0x2563; 0x252a
                        ; 0x2535; 0x253d; 0x256c; 0x2545
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2537; 0x253f; 0x256c; 0x2548
                        ; 0x2579; 0x2551; 0x2551; 0x2551
                        ; 0x2559; 0x2560; 0x255f; 0x2560
                        ; 0x255a; 0x2560; 0x2560; 0x2560
                        ; 0x255a; 0x2560; 0x2560; 0x2560
                        ; 0x255c; 0x2563; 0x2562; 0x2563
                        ; 0x2568; 0x256c; 0x256b; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x255d; 0x2563; 0x2563; 0x2563
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x255d; 0x2563; 0x2563; 0x2563
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2579; 0x257f; 0x2551; 0x2503
                        ; 0x2516; 0x251e; 0x2560; 0x2520
                        ; 0x255a; 0x2560; 0x2560; 0x2560
                        ; 0x2517; 0x2521; 0x2560; 0x2523
                        ; 0x251a; 0x2526; 0x2563; 0x2528
                        ; 0x2538; 0x2540; 0x256c; 0x2542
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x253a; 0x2544; 0x256c; 0x254a
                        ; 0x255d; 0x2563; 0x2563; 0x2563
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x251b; 0x2529; 0x2563; 0x252b
                        ; 0x2539; 0x2543; 0x256c; 0x2549
                        ; 0x2569; 0x256c; 0x256c; 0x256c
                        ; 0x253b; 0x2547; 0x256c; 0x254b
                       |]


  let of_ucode c =
    if c < 0x2500 || c > 0x257f then
      -1
    else
      ucode_to_piece.(c - 0x2500)
  ;;

  let of_char c = of_ucode (Uchar.to_int c)

  let to_ucode t =
    let n =
      ((t land Bits.top   ) lsr 18) lor
      ((t land Bits.left  ) lsr 12) lor
      ((t land Bits.right ) lsr  6) lor
      ((t land Bits.bottom))
    in
    piece_to_ucode.(n)
  ;;

  let to_char piece = Uchar.of_int (to_ucode piece)
end

let update_piece ctx piece1 row col bits1 bits2 =
  let point2 = ctx.matrix.(row).(col) in
  let piece2 = Piece.of_char point2.char in
  if piece2 < 0 then
    piece1
  else begin
    let new_piece1 = piece1 lor (Piece.reverse (piece2 land bits2)) in
    let new_piece2 = piece2 lor (Piece.reverse (piece1 land bits1)) in
    point2.char <- Piece.to_char new_piece2;
    new_piece1
  end

let draw_piece ctx ~row ~col ?(style=default_style) piece =
  let row = ctx.row1 + row and col = ctx.col1 + col in
  if row >= ctx.row1
  && col >= ctx.col1
  && row < ctx.row2
  && col < ctx.col2 then begin
    let piece =
      if row > ctx.row1 then
        update_piece ctx piece (row - 1) col Piece.Bits.top Piece.Bits.bottom
      else
        piece
    in
    let piece =
      if row < ctx.row2 - 1 then
        update_piece ctx piece (row + 1) col Piece.Bits.bottom Piece.Bits.top
      else
        piece
    in
    let piece =
      if col > ctx.col1 then begin
        update_piece ctx piece row (col - 1) Piece.Bits.left Piece.Bits.right
      end else
        piece
    in
    let piece =
      if col < ctx.col2 - 1 then begin
        update_piece ctx piece row (col + 1) Piece.Bits.right Piece.Bits.left
      end else
        piece
    in
    let point = ctx.matrix.(row).(col) in
    point.char <- Piece.to_char piece;
    set_point_style point ~style
  end;
;;

let draw_hline ctx ~row ~col ~len ?(style=default_style) connection =
  let piece = Piece.hline connection in
  for i = 0 to len - 1 do
    draw_piece ctx ~row ~col:(col + i) ~style piece
  done;
;;

let draw_vline ctx ~row ~col ~len ?(style=default_style) connection =
  let piece = Piece.vline connection in
  for i = 0 to len - 1 do
    draw_piece ctx ~row:(row + i) ~col ~style piece
  done;
;;

let draw_frame ctx rect ?(style=default_style)
      connection =
  let { Geom. col1; col2; row1; row2 } = rect in
  let hline = Piece.hline connection in
  let vline = Piece.vline connection in
  for col = col1 + 1 to col2 - 2 do
    draw_piece ctx ~row:(row1 + 0) ~col ~style hline;
    draw_piece ctx ~row:(row2 - 1) ~col ~style hline;
  done;
  for row = row1 + 1 to row2 - 2 do
    draw_piece ctx ~row ~col:(col1 + 0) ~style vline;
    draw_piece ctx ~row ~col:(col2 - 1) ~style vline;
  done;
  draw_piece ctx ~row:(row1 + 0) ~col:(col1 + 0) ~style (Piece.br connection);
  draw_piece ctx ~row:(row1 + 0) ~col:(col2 - 1) ~style (Piece.bl connection);
  draw_piece ctx ~row:(row2 - 1) ~col:(col2 - 1) ~style (Piece.tl connection);
  draw_piece ctx ~row:(row2 - 1) ~col:(col1 + 0) ~style (Piece.tr connection);
;;
