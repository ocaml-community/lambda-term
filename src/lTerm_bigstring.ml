(*
 * lTerm_bigstring.ml
 * ------------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 *)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create len = Array1.create char c_layout len

let length (t : t) = Array1.dim t

external unsafe_blit
  : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  = "lt_bigstring_blit" "noalloc"

external unsafe_blit_string
  : src:string -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
  = "lt_bigstring_blit_string" "noalloc"

external unsafe_blit_to_bytes
  : src:t -> src_pos:int -> dst:string -> dst_pos:int -> len:int -> unit
  = "lt_bigstring_blit_to_bytes" "noalloc"

external unsafe_read : Unix.file_descr -> buf:t -> pos:int -> len:int -> int
  = "lt_bigstring_read"

external unsafe_write : Unix.file_descr -> buf:t -> pos:int -> len:int -> int
  = "lt_bigstring_write"

let check_pos_len ~pos ~len ~actual_len =
  let stop = pos + len in
  assert (pos lor len lor stop lor (actual_len - stop) >= 0)
;;

let blit ~src ~src_pos ~dst ~dst_pos ~len =
  check_pos_len ~pos:src_pos ~len:len ~actual_len:(length src);
  check_pos_len ~pos:dst_pos ~len:len ~actual_len:(length dst);
  unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
;;

let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
  check_pos_len ~pos:src_pos ~len:len ~actual_len:(String.length src);
  check_pos_len ~pos:dst_pos ~len:len ~actual_len:(length dst);
  unsafe_blit_string ~src ~src_pos ~dst ~dst_pos ~len
;;

let sub_string t ~pos ~len =
  check_pos_len ~pos ~len ~actual_len:(length t);
  let res = Bytes.create len in
  unsafe_blit_to_bytes ~src:t ~dst:res ~src_pos:pos ~dst_pos:0 ~len;
  Bytes.unsafe_to_string res
;;

let read fd ~buf ~pos ~len =
  check_pos_len ~pos ~len ~actual_len:(length buf);
  unsafe_read fd ~buf ~pos ~len
;;

let write fd ~buf ~pos ~len =
  check_pos_len ~pos ~len ~actual_len:(length buf);
  unsafe_write fd ~buf ~pos ~len
;;
