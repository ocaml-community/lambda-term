(*
 * lTerm_bigstring.mli
 * -------------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 *)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

val create : int -> t

val length : t -> int

val blit
  :  src:t
  -> src_pos:int
  -> dst:t
  -> dst_pos:int
  -> len:int
  -> unit

val blit_string
  :  src:string
  -> src_pos:int
  -> dst:t
  -> dst_pos:int
  -> len:int
  -> unit

val sub_string : t -> pos:int -> len:int -> string

val read  : Unix.file_descr -> buf:t -> pos:int -> len:int -> int
val write : Unix.file_descr -> buf:t -> pos:int -> len:int -> int
