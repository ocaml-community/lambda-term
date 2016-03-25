type connection = B (* blank *) | L (* light *) | H (* heavy *) | D (* double *)

type piece =
  { t : connection
  ; b : connection
  ; l : connection
  ; r : connection
  }

(* XXXX is for top, bottom, left, right *)
let unicode_chars =
  [ "    ─    ━    │    ┃    ┄    ┅    ┆    ┇    ┈    ┉    ┊    ┋    ┌    ┍    ┎    ┏"
  ; " BBLL BBHH LLBB HHBB .... .... .... .... .... .... .... .... BLBL BLBH BHBL BHBH"
  ; "    ┐    ┑    ┒    ┓    └    ┕    ┖    ┗    ┘    ┙    ┚    ┛    ├    ┝    ┞    ┟"
  ; " BLLB BLHB BHLB BHHB LBBL LBBH HBBL HBBH LBLB LBHB HBLB HBHB LLBL LLBH HLBL LHBL"
  ; "    ┠    ┡    ┢    ┣    ┤    ┥    ┦    ┧    ┨    ┩    ┪    ┫    ┬    ┭    ┮    ┯"
  ; " HHBL HLBH LHBH HHBH LLLB LLHB HLLB LHLB HHLB HLHB LHHB HHHB BLLL BLHL BLLH BLHH"
  ; "    ┰    ┱    ┲    ┳    ┴    ┵    ┶    ┷    ┸    ┹    ┺    ┻    ┼    ┽    ┾    ┿"
  ; " BHLL BHHL BHLH BHHH LBLL LBHL LBLH LBHH HBLL HBHL HBLH HBHH LLLL LLHL LLLH LLHH"
  ; "    ╀    ╁    ╂    ╃    ╄    ╅    ╆    ╇    ╈    ╉    ╊    ╋    ╌    ╍    ╎    ╏"
  ; " HLLL LHLL HHLL HLHL HLLH LHHL LHLH HLHH LHHH HHHL HHLH HHHH .... .... .... ...."
  ; "    ═    ║    ╒    ╓    ╔    ╕    ╖    ╗    ╘    ╙    ╚    ╛    ╜    ╝    ╞    ╟"
  ; " BBDD DDBB BLBD BDBL BDBD BLDB BDLB BDDB LBBD DBBL DBBD LBDB DBLB DBDB LLBD DDBL"
  ; "    ╠    ╡    ╢    ╣    ╤    ╥    ╦    ╧    ╨    ╩    ╪    ╫    ╬    ╭    ╮    ╯"
  ; " DDBD LLDB DDLB DDDB BLDD BDLL BDDD LBDD DBLL DBDD LLDD DDLL DDDD .... .... ...."
  ; "    ╰    ╱    ╲    ╳    ╴    ╵    ╶    ╷    ╸    ╹    ╺    ╻    ╼    ╽    ╾    ╿"
  ; " .... .... .... .... BBLB LBBB BBBL BLBB BBHB HBBB BBBH BHBB BBLH LHBB BBHL HLBB"
  ]

let con_of_char = function
  | 'B' -> B
  | 'L' -> L
  | 'H' -> H
  | 'D' -> D
  | _ -> assert false

let nothing = { t = B; b = B; l = B; r = B }

let parse_line s =
  let arr = Array.make 16 nothing in
  for i = 0 to 15 do
    let ofs = i * 5 in
    if s.[ofs + 1] <> '.' then  begin
      let t = con_of_char s.[ofs + 1] in
      let b = con_of_char s.[ofs + 2] in
      let l = con_of_char s.[ofs + 3] in
      let r = con_of_char s.[ofs + 4] in
      arr.(i) <- { t; b; l; r }
    end
  done;
  arr

let existing_pieces =
  let rec odds = function
    | _ :: x :: l -> x :: odds l
    | _ -> []
  in
  Array.concat (List.map parse_line (odds unicode_chars))

let int_of_con = function
  | B -> 0
  | L -> 1
  | H -> 3
  | D -> 2

let con_of_int = function
  | 0 -> B
  | 1 -> L
  | 2 -> D
  | 3 -> H
  | _ -> assert false

let int_of_piece p =
  (* same order as in LTerm_draw.Piece *)
  (int_of_con p.t lsl 24) lor
  (int_of_con p.l lsl 16) lor
  (int_of_con p.r lsl  8) lor
  int_of_con p.b
;;

let all_pieces =
  let a = Array.make (4 * 4 * 4 * 4) nothing in
  for t = 0 to 3 do
    for b = 0 to 3 do
      for l = 0 to 3 do
        for r = 0 to 3 do
          (* same order as in LTerm_draw.Piece *)
          a.((t lsl 6) lor
             (l lsl 4) lor
             (r lsl 2) lor
             b) <- { t = con_of_int t
                   ; b = con_of_int b
                   ; l = con_of_int l
                   ; r = con_of_int r
                   }
        done
      done
    done
  done;
  a

let find_piece x =
  let rec loop i =
    if i = Array.length existing_pieces then
      None
    else if x = existing_pieces.(i) then
      Some i
    else
      loop (i + 1)
  in
  loop 0
;;

let simplify = function
  | B -> B
  | H -> D
  | D -> D
  | L -> D


let simplify2 = function
  | B -> B
  | H -> H
  | D -> H
  | L -> H

let map f p =
  { t = f p.t
  ; b = f p.b
  ; l = f p.l
  ; r = f p.r
  }

let mapping =
  Array.map (fun p ->
    match find_piece p with
    | Some i -> i
    | None ->
      match find_piece (map simplify p) with
      | Some i -> i
      | None ->
        match find_piece (map simplify2 p) with
        | Some i -> i
        | None -> assert false
  ) all_pieces

open Printf

let () =
  printf "let ucode_to_piece = [| ";
  for i = 0 to 127 do
    if i > 0 then printf "; ";
    let p = existing_pieces.(i) in
    if p = nothing then
      printf "        -1"
    else
      printf "0x%08x" (int_of_piece p);
    if i land 3 = 3 then printf "\n%s" (String.make 22 ' ');
  done;
  printf "|]\n\n"

let () =
  printf "let piece_to_ucode = [| ";
  for i = 0 to 255 do
    if i > 0 then printf "; ";
    if i = 0 then
      printf "0x0020"
    else begin
      let j = mapping.(i) in
      printf "0x%04x" (0x2500 + j)
    end;
    if i land 3 = 3 then printf "\n%s" (String.make 22 ' ');
  done;
  printf "|]\n\n"

let () =
  printf "let ucode_to_ascii_approx = \"";
  for i = 0 to 127 do
    let p = existing_pieces.(i) in
    if p = nothing then
      printf "."
    else
      printf "%c"
        (match map simplify p with
         | { t = D; b = D; l = B; r = B } -> '|'
         | { t = B; b = B; l = D; r = D } -> '-'
         | _ -> '+');
    if i < 127 && i land 31 = 31 then printf "\\\n%s" (String.make 29 ' ');
  done;
  printf "\"\n\n"
