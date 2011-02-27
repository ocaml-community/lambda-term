(*
 * lt_utf8.ml
 * ----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type t = string
type code_point = int
exception Invalid of string * string
exception Out_of_bounds

let fail str pos msg = raise (Invalid(Printf.sprintf "at position %d: %s" pos msg, str))

let byte str i = Char.code (String.unsafe_get str i)
let set_byte str i n = String.unsafe_set str i (Char.unsafe_chr n)

(* +-----------------------------------------------------------------+
   | Validation                                                      |
   +-----------------------------------------------------------------+ *)

let check s =
  let fail i msg = Some(Printf.sprintf "at position %d: %s" i msg) in
  let len = String.length s in
  let rec main i =
    if i = len then
      None
    else
      let ch = String.unsafe_get s i in
      match ch with
        | '\x00' .. '\x7f' ->
            main (i + 1)
        | '\xc0' .. '\xdf' ->
            if i + 1 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x1f) lsl 6) lor (byte1 land 0x3f) < 0x80 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 2)
            end
        | '\xe0' .. '\xef' ->
            if i + 2 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1))
              and byte2 = Char.code (String.unsafe_get s (i + 2)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if byte2 land 0xc0 != 0x80 then
                fail (i + 2) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x0f) lsl 12) lor ((byte1 land 0x3f) lsl 6) lor (byte2 land 0x3f) < 0x800 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 3)
            end
        | '\xf0' .. '\xf7' ->
            if i + 3 >= len then
              fail len "premature end of UTF8 sequence"
            else begin
              let byte1 = Char.code (String.unsafe_get s (i + 1))
              and byte2 = Char.code (String.unsafe_get s (i + 2))
              and byte3 = Char.code (String.unsafe_get s (i + 3)) in
              if byte1 land 0xc0 != 0x80 then
                fail (i + 1) "malformed UTF8 sequence"
              else if byte2 land 0xc0 != 0x80 then
                fail (i + 2) "malformed UTF8 sequence"
              else if byte3 land 0xc0 != 0x80 then
                fail (i + 3) "malformed UTF8 sequence"
              else if ((Char.code ch land 0x07) lsl 18) lor ((byte1 land 0x3f) lsl 12) lor ((byte2 land 0x3f) lsl 6) lor (byte3 land 0x3f) < 0x10000 then
                fail i "overlong UTF8 sequence"
              else
                main (i + 4)
            end
        | _ ->
            fail i "invalid start of UTF8 sequence"
  in
  main 0

let validate s =
  match check s with
    | Some error ->
        raise (Invalid(error, s))
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Unsafe UTF-8 manipulation                                       |
   +-----------------------------------------------------------------+ *)

let unsafe_next str ofs =
  match String.unsafe_get str ofs with
    | '\x00' .. '\x7f' ->
        ofs + 1
    | '\xc0' .. '\xdf' ->
        if ofs + 2 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ofs + 2
    | '\xe0' .. '\xef' ->
        if ofs + 3 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ofs + 3
    | '\xf0' .. '\xf7' ->
        if ofs + 4 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ofs + 4
    | _ ->
        fail str ofs "invalid start of UTF-8 sequence"

let unsafe_prev str ofs =
  match String.unsafe_get str (ofs - 1) with
    | '\x00' .. '\x7f' ->
        ofs - 1
    | '\x80' .. '\xbf' ->
        if ofs >= 2 then
          match String.unsafe_get str (ofs - 2) with
            | '\xc0' .. '\xdf' ->
                ofs - 2
            | '\x80' .. '\xbf' ->
                if ofs >= 3 then
                  match String.unsafe_get str (ofs - 3) with
                    | '\xe0' .. '\xef' ->
                        ofs - 3
                    | '\x80' .. '\xbf' ->
                        if ofs >= 4 then
                          match String.unsafe_get str (ofs - 4) with
                            | '\xf0' .. '\xf7' ->
                                ofs - 4
                            | _ ->
                                fail str (ofs - 4) "invalid start of UTF-8 sequence"
                        else
                          fail str (ofs - 3) "invalid start of UTF-8 string"
                    | _ ->
                        fail str (ofs - 3) "invalid middle of UTF-8 sequence"
                else
                  fail str (ofs - 2) "invaild start of UTF-8 string"
            | _ ->
                fail str (ofs - 2) "invalid middle of UTF-8 sequence"
        else
          fail str (ofs - 1) "invalid start of UTF-8 string"
    | _ ->
        fail str (ofs - 1) "invalid end of UTF-8 sequence"

let unsafe_extract str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
        Char.code ch
    | '\xc0' .. '\xdf' ->
        if ofs + 2 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ((Char.code ch land 0x1f) lsl 6) lor (byte str (ofs + 1) land 0x3f)
    | '\xe0' .. '\xef' ->
        if ofs + 3 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ((Char.code ch land 0x0f) lsl 12) lor ((byte str (ofs + 1) land 0x3f) lsl 6) lor (byte str (ofs + 2) land 0x3f)
    | '\xf0' .. '\xf7' ->
        if ofs + 4 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          ((Char.code ch land 0x07) lsl 18) lor ((byte str (ofs + 1) land 0x3f) lsl 12) lor ((byte str (ofs + 2) land 0x3f) lsl 6) lor (byte str (ofs + 3) land 0x3f)
    | _ ->
        fail str ofs "invalid start of UTF-8 sequence"

let unsafe_extract_next str ofs =
  let ch = String.unsafe_get str ofs in
  match ch with
    | '\x00' .. '\x7f' ->
        (Char.code ch, ofs + 1)
    | '\xc0' .. '\xdf' ->
        if ofs + 2 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          (((Char.code ch land 0x1f) lsl 6) lor (byte str (ofs + 1) land 0x3f), ofs + 2)
    | '\xe0' .. '\xef' ->
        if ofs + 3 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          (((Char.code ch land 0x0f) lsl 12) lor ((byte str (ofs + 1) land 0x3f) lsl 6) lor (byte str (ofs + 2) land 0x3f), ofs + 3)
    | '\xf0' .. '\xf7' ->
        if ofs + 4 > String.length str then
          fail str ofs "unterminated UTF-8 sequence"
        else
          (((Char.code ch land 0x07) lsl 18) lor ((byte str (ofs + 1) land 0x3f) lsl 12) lor ((byte str (ofs + 2) land 0x3f) lsl 6) lor (byte str (ofs + 3) land 0x3f), ofs + 4)
    | _ ->
        fail str ofs "invalid start of UTF-8 sequence"

let unsafe_extract_prev str ofs =
  let ch1 = String.unsafe_get str (ofs - 1) in
  match ch1 with
    | '\x00' .. '\x7f' ->
        (Char.code ch1, ofs - 1)
    | '\x80' .. '\xbf' ->
        if ofs >= 2 then
          let ch2 = String.unsafe_get str (ofs - 2) in
          match ch2 with
            | '\xc0' .. '\xdf' ->
                (((Char.code ch2 land 0x1f) lsl 6) lor (Char.code ch1 land 0x3f), ofs - 2)
            | '\x80' .. '\xbf' ->
                if ofs >= 3 then
                  let ch3 = String.unsafe_get str (ofs - 3) in
                  match ch3 with
                    | '\xe0' .. '\xef' ->
                        (((Char.code ch3 land 0x0f) lsl 12) lor ((Char.code ch2 land 0x3f) lsl 6) lor (Char.code ch1 land 0x3f), ofs - 3)
                    | '\x80' .. '\xbf' ->
                        if ofs >= 4 then
                          let ch4 = String.unsafe_get str (ofs - 4) in
                          match ch4 with
                            | '\xf0' .. '\xf7' ->
                                (((Char.code ch4 land 0x07) lsl 18) lor ((Char.code ch3 land 0x3f) lsl 12) lor ((Char.code ch2 land 0x3f) lsl 6) lor (Char.code ch1 land 0x3f), ofs + 4)
                            | _ ->
                                fail str (ofs - 4) "invalid start of UTF-8 sequence"
                        else
                          fail str (ofs - 3) "invalid start of UTF-8 string"
                    | _ ->
                        fail str (ofs - 3) "invalid middle of UTF-8 sequence"
                else
                  fail str (ofs - 2) "invaild start of UTF-8 string"
            | _ ->
                fail str (ofs - 2) "invalid middle of UTF-8 sequence"
        else
          fail str (ofs - 1) "invalid start of UTF-8 string"
    | _ ->
        fail str (ofs - 1) "invalid end of UTF-8 sequence"

let rec move_l str ofs len =
  if len = 0 then
    ofs
  else if ofs = String.length str then
    raise Out_of_bounds
  else
    move_l str (unsafe_next str ofs) (len - 1)

let rec move_r str ofs len =
  if len = 0 then
    ofs
  else if ofs = 0 then
    raise Out_of_bounds
  else
    move_r str (unsafe_prev str ofs) (len - 1)

let unsafe_sub str ofs len =
  let res = String.create len in
  String.unsafe_blit str ofs res 0 len;
  res

(* +-----------------------------------------------------------------+
   | Construction                                                    |
   +-----------------------------------------------------------------+ *)

let singleton code =
  if code < 0 then
    invalid_arg "Lt_utf8.singleton"
  else if code < 0x80 then begin
    let s = String.create 1 in
    set_byte s 0 code;
    s
  end else if code <= 0x800 then begin
    let s = String.create 2 in
    set_byte s 0 ((code lsr 6) lor 0xc0);
    set_byte s 1 ((code land 0x3f) lor 0x80);
    s
  end else if code <= 0x10000 then begin
    let s = String.create 3 in
    set_byte s 0 ((code lsr 12) lor 0xe0);
    set_byte s 1 (((code lsr 6) land 0x3f) lor 0x80);
    set_byte s 2 ((code land 0x3f) lor 0x80);
    s
  end else if code <= 0x10ffff then begin
    let s = String.create 4 in
    set_byte s 0 ((code lsr 18) lor 0xf0);
    set_byte s 1 (((code lsr 12) land 0x3f) lor 0x80);
    set_byte s 2 (((code lsr 6) land 0x3f) lor 0x80);
    set_byte s 3 ((code land 0x3f) lor 0x80);
    s
  end else
    invalid_arg "Lt_utf8.singleton"

let make n code =
  let str = singleton code in
  let len = String.length str in
  let res = String.create (n * len) in
  let ofs = ref 0 in
  for i = 1 to n do
    String.unsafe_blit str 0 res !ofs len;
    ofs := !ofs + len
  done;
  res

let init n f =
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_string buf (singleton (f i))
  done;
  Buffer.contents buf

let rev_init n f =
  let buf = Buffer.create n in
  for i = n - 1 downto 0 do
    Buffer.add_string buf (singleton (f i))
  done;
  Buffer.contents buf

(* +-----------------------------------------------------------------+
   | Informations                                                    |
   +-----------------------------------------------------------------+ *)

let rec length_rec str ofs len =
  if ofs = String.length str then
    len
  else
    length_rec str (unsafe_next str ofs) (len + 1)

let length str =
  length_rec str 0 0

(* +-----------------------------------------------------------------+
   | Comparison                                                      |
   +-----------------------------------------------------------------+ *)

let rec compare_rec str1 ofs1 str2 ofs2 =
  if ofs1 = String.length str1 then
    if ofs2 = String.length str2 then
      0
    else
      -1
  else if ofs2 = String.length str2 then
    1
  else
    let code1, ofs1 = unsafe_extract_next str1 ofs1
    and code2, ofs2 = unsafe_extract_next str2 ofs2 in
    let d = code1 - code2 in
    if d <> 0 then
      d
    else
      compare_rec str1 ofs1 str2 ofs2

let compare str1 str2 =
  compare_rec str1 0 str2 0

(* +-----------------------------------------------------------------+
   | Random access                                                   |
   +-----------------------------------------------------------------+ *)

let get str idx =
  if idx < 0 then
    raise Out_of_bounds
  else
    unsafe_extract str (move_l str 0 idx)

(* +-----------------------------------------------------------------+
   | Manipulation                                                    |
   +-----------------------------------------------------------------+ *)

let sub str idx len =
  if idx < 0 || len < 0 then
    raise Out_of_bounds
  else
    let ofs1 = move_l str 0 idx in
    let ofs2 = move_l str ofs1 len in
    unsafe_sub str ofs1 (ofs2 - ofs1)

let break str idx =
  if idx < 0 then
    raise Out_of_bounds
  else
    let ofs = move_l str 0 idx in
    (unsafe_sub str 0 ofs, unsafe_sub str ofs (String.length str - ofs))

let before str idx =
  if idx < 0 then
    raise Out_of_bounds
  else
    let ofs = move_l str 0 idx in
    unsafe_sub str 0 ofs

let after str idx =
  if idx < 0 then
    raise Out_of_bounds
  else
    let ofs = move_l str 0 idx in
    unsafe_sub str ofs (String.length str - ofs)

let concat3 a b c =
  let lena = String.length a
  and lenb = String.length b
  and lenc = String.length c in
  let res = String.create (lena + lenb + lenc) in
  String.unsafe_blit a 0 res 0 lena;
  String.unsafe_blit b 0 res lena lenb;
  String.unsafe_blit c 0 res (lena + lenb) lenc;
  res

let insert str idx sub =
  let a, b = break str idx in
  concat3 a sub b

let remove str idx len =
  if idx < 0 || len < 0 then
    raise Out_of_bounds
  else
    let ofs1 = move_l str 0 idx in
    let ofs2 = move_l str ofs1 len in
    unsafe_sub str 0 ofs1 ^ unsafe_sub str ofs2 (String.length str - ofs2)

let replace str idx len repl =
  if idx < 0 || len < 0 then
    raise Out_of_bounds
  else
    let ofs1 = move_l str 0 idx in
    let ofs2 = move_l str ofs1 len in
    concat3 (unsafe_sub str 0 ofs1) repl (unsafe_sub str ofs2 (String.length str - ofs2))

(* +-----------------------------------------------------------------+
   | Exploding and imploding                                         |
   +-----------------------------------------------------------------+ *)

let rec rev_rec res str ofs_src ofs_dst =
  if ofs_src = String.length str then
    res
  else begin
    let ofs_src' = unsafe_next str ofs_src in
    let len = ofs_src' - ofs_src in
    let ofs_dst = ofs_dst - len in
    String.unsafe_blit str ofs_src res ofs_dst len;
    rev_rec res str ofs_src' ofs_dst
  end

let rev str =
  let len = String.length str in
  rev_rec (String.create len) str 0 len

let concat sep l =
  match l with
    | [] ->
        ""
    | x :: l ->
        let sep_len = String.length sep in
        let len = List.fold_left (fun len str -> len + sep_len + String.length str) (String.length x) l in
        let res = String.create len in
        String.unsafe_blit x 0 res 0 (String.length x);
        ignore
          (List.fold_left
             (fun ofs str ->
                String.unsafe_blit sep 0 res ofs sep_len;
                let ofs = ofs + sep_len in
                let len = String.length str in
                String.unsafe_blit str 0 res ofs len;
                ofs + len)
             (String.length x) l);
        res

let rev_concat sep l =
  match l with
    | [] ->
        ""
    | x :: l ->
        let sep_len = String.length sep in
        let len = List.fold_left (fun len str -> len + sep_len + String.length str) (String.length x) l in
        let res = String.create len in
        let ofs = len - String.length x in
        String.unsafe_blit x 0 res ofs (String.length x);
        ignore
          (List.fold_left
             (fun ofs str ->
                let ofs = ofs - sep_len in
                String.unsafe_blit sep 0 res ofs sep_len;
                let len = String.length str in
                let ofs = ofs - len in
                String.unsafe_blit str 0 res ofs len;
                ofs)
             ofs l);
        res

let rec explode_rec str ofs acc =
  if ofs = 0 then
    acc
  else
    let x, ofs = unsafe_extract_prev str ofs in
    explode_rec str ofs (x :: acc)

let explode str =
  explode_rec str (String.length str) []

let rec rev_explode_rec str ofs acc =
  if ofs = String.length str then
    acc
  else
    let x, ofs = unsafe_extract_next str ofs in
    rev_explode_rec str ofs (x :: acc)

let rev_explode str =
  rev_explode_rec str 0 []

let implode l =
  let l = List.map singleton l in
  let len = List.fold_left (fun len str -> len + String.length str) 0 l in
  let res = String.create len in
  ignore
    (List.fold_left
       (fun ofs str ->
          let len = String.length str in
          String.unsafe_blit str 0 res ofs len;
          ofs + len)
       0 l);
  res

let rev_implode l =
  let l = List.map singleton l in
  let len = List.fold_left (fun len str -> len + String.length str) 0 l in
  let res = String.create len in
  ignore
    (List.fold_left
       (fun ofs str ->
          let len = String.length str in
          let ofs = ofs - len in
          String.unsafe_blit str 0 res ofs len;
          ofs)
       len l);
  res

(* +-----------------------------------------------------------------+
   | Text transversal                                                |
   +-----------------------------------------------------------------+ *)

let rec iter_rec f str ofs =
  if ofs = String.length str then
    ()
  else begin
    let chr, ofs = unsafe_extract_next str ofs in
    f chr;
    iter_rec f str ofs
  end

let iter f str =
  iter_rec f str 0

let rec rev_iter_rec f str ofs =
  if ofs = 0 then
    ()
  else begin
    let chr, ofs = unsafe_extract_prev str ofs in
    f chr;
    rev_iter_rec f str ofs
  end

let rev_iter f str =
  rev_iter_rec f str (String.length str)

let rec fold_rec f str ofs acc =
  if ofs = String.length str then
    acc
  else begin
    let chr, ofs = unsafe_extract_next str ofs in
    fold_rec f str ofs (f chr acc)
  end

let fold f str acc =
  fold_rec f str 0 acc

let rec rev_fold_rec f str ofs acc =
  if ofs = 0 then
    acc
  else begin
    let chr, ofs = unsafe_extract_prev str ofs in
    rev_fold_rec f str ofs (f chr acc)
  end

let rev_fold f str acc =
  rev_fold_rec f str (String.length str) acc

let rec map_rec buf f str ofs =
  if ofs = String.length str then
    Buffer.contents buf
  else begin
    let chr, ofs = unsafe_extract_next str ofs in
    Buffer.add_string buf (singleton (f chr));
    map_rec buf f str ofs
  end

let map f str =
  map_rec (Buffer.create (String.length str)) f str 0

let rec rev_map_rec buf f str ofs =
  if ofs = 0 then
    Buffer.contents buf
  else begin
    let chr, ofs = unsafe_extract_prev str ofs in
    Buffer.add_string buf (singleton (f chr));
    rev_map_rec buf f str ofs
  end

let rev_map f str =
  rev_map_rec (Buffer.create (String.length str)) f str (String.length str)

let rec filter_rec buf f str ofs =
  if ofs = String.length str then
    Buffer.contents buf
  else begin
    let chr, ofs = unsafe_extract_next str ofs in
    if f chr then
      Buffer.add_string buf (singleton chr);
    filter_rec buf f str ofs
  end

let filter f str =
  filter_rec (Buffer.create (String.length str)) f str 0

let rec rev_filter_rec buf f str ofs =
  if ofs = 0 then
    Buffer.contents buf
  else begin
    let chr, ofs = unsafe_extract_prev str ofs in
    if f chr then
      Buffer.add_string buf (singleton chr);
    rev_filter_rec buf f str ofs
  end

let rev_filter f str =
  rev_filter_rec (Buffer.create (String.length str)) f str (String.length str)

let rec filter_map_rec buf f str ofs =
  if ofs = String.length str then
    Buffer.contents buf
  else begin
    let chr, ofs = unsafe_extract_next str ofs in
    (match f chr with
       | Some chr ->
           Buffer.add_string buf (singleton chr)
       | None ->
           ());
    filter_map_rec buf f str ofs
  end

let filter_map f str =
  filter_map_rec (Buffer.create (String.length str)) f str 0

let rec rev_filter_map_rec buf f str ofs =
  if ofs = 0 then
    Buffer.contents buf
  else begin
    let chr, ofs = unsafe_extract_prev str ofs in
    (match f chr with
       | Some chr ->
           Buffer.add_string buf (singleton chr)
       | None ->
           ());
    rev_filter_map_rec buf f str ofs
  end

let rev_filter_map f str =
  rev_filter_map_rec (Buffer.create (String.length str)) f str (String.length str)

(* +-----------------------------------------------------------------+
   | Scanning                                                        |
   +-----------------------------------------------------------------+ *)

let rec for_all_rec f str ofs =
  if ofs = String.length str then
    true
  else
    let chr, ofs = unsafe_extract_next str ofs in
    f chr && for_all_rec f str ofs

let for_all f str =
  for_all_rec f str 0

let rec exists_rec f str ofs =
  if ofs = String.length str then
    false
  else
    let chr, ofs = unsafe_extract_next str ofs in
    f chr || exists_rec f str ofs

let exists f str =
  exists_rec f str 0

let rec count_rec f str ofs n =
  if ofs = String.length str then
    n
  else
    let chr, ofs = unsafe_extract_next str ofs in
    count_rec f str ofs (if f chr then n + 1 else n)

let count f str =
  count_rec f str 0 0

(* +-----------------------------------------------------------------+
   | Tests                                                           |
   +-----------------------------------------------------------------+ *)

let rec unsafe_sub_equal str ofs sub ofs_sub =
  if ofs_sub = String.length sub then
    true
  else
    (String.unsafe_get str ofs = String.unsafe_get sub ofs_sub)
    && unsafe_sub_equal str (ofs + 1) sub (ofs_sub + 1)

let rec contains_rec str sub ofs =
  if ofs + String.length sub > String.length str then
    false
  else
    unsafe_sub_equal str ofs sub 0 || contains_rec str sub (unsafe_next str ofs)

let contains str sub =
  contains_rec str sub 0

let starts_with str prefix =
  if String.length prefix > String.length str then
    false
  else
    unsafe_sub_equal str 0 prefix 0

let ends_with str suffix =
  let ofs = String.length str - String.length suffix in
  if ofs < 0 then
    false
  else
    unsafe_sub_equal str ofs suffix 0

(* +-----------------------------------------------------------------+
   | Stripping                                                       |
   +-----------------------------------------------------------------+ *)

let rec lfind predicate str ofs =
  if ofs = String.length str then
    ofs
  else
    let chr, ofs' = unsafe_extract_next str ofs in
    if predicate chr then
      lfind predicate str ofs'
    else
      ofs

let rec rfind predicate str ofs =
  if ofs = 0 then
    0
  else
    let chr, ofs' = unsafe_extract_prev str ofs in
    if predicate chr then
      rfind predicate str ofs'
    else
      ofs

let is_space ch =
  ch = Char.code ' ' || ch = Char.code '\t' || ch = Char.code '\r' || ch = Char.code '\n'

let strip ?(predicate=is_space) str =
  let lofs = lfind predicate str 0 and rofs = rfind predicate str (String.length str) in
  if lofs < rofs then
    unsafe_sub str lofs (rofs - lofs)
  else
    ""

let lstrip ?(predicate=is_space) str =
  let lofs = lfind predicate str 0 in
  unsafe_sub str lofs (String.length str - lofs)

let rstrip ?(predicate=is_space) str =
  let rofs = rfind predicate str (String.length str) in
  unsafe_sub str 0 rofs

let lchop = function
  | "" ->
      ""
  | str ->
      let ofs = unsafe_next str 0 in
      unsafe_sub str ofs (String.length str - ofs)

let rchop = function
  | "" ->
      ""
  | str ->
      let ofs = unsafe_prev str (String.length str) in
      unsafe_sub str 0 ofs
