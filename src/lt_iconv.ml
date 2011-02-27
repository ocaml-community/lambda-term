(*
 * lt_iconv.ml
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

type t

exception Closed
exception Unsupported of string * string
exception Invalid_sequence
exception Unterminated_sequence
exception Insufficient_space

let () =
  Callback.register_exception "lambda-term:iconv:closed" Closed;
  Callback.register_exception "lambda-term:iconv:unsupported" (Unsupported("", ""));
  Callback.register_exception "lambda-term:iconv:invalid-sequence" Invalid_sequence;
  Callback.register_exception "lambda-term:iconv:unterminated-sequence" Unterminated_sequence;
  Callback.register_exception "lambda-term:iconv:insufficient-space" Insufficient_space

type data = {
  mutable bytes : string;
  mutable index : int;
  mutable limit : int;
}

external iconv_open : to_code : string -> of_code : string -> t = "lt_iconv_iconv_open"
external iconv_close : t -> unit = "lt_iconv_iconv_close"
external iconv : t -> src : data -> dst : data -> unit = "lt_iconv_iconv" "noalloc"
external reset : t -> unit = "lt_iconv_reset" "noalloc"

let iconv cd ~src ~dst =
  if src.index < 0 || src.index > src.limit || src.limit > String.length src.bytes then invalid_arg "Lt_iconv.iconv";
  if dst.index < 0 || dst.index > dst.limit || dst.limit > String.length dst.bytes then invalid_arg "Lt_iconv.iconv";
  iconv cd ~src ~dst

let recode ~to_code ~of_code str =
  let cd = iconv_open ~to_code ~of_code in
  let src = {
    bytes = str;
    index = 0;
    limit = String.length str;
  }
  and dst = {
    bytes = String.create (String.length str);
    index = 0;
    limit = String.length str;
  } in
  while src.index < src.limit do
    try
      iconv cd ~src ~dst
    with
      | (Invalid_sequence | Unterminated_sequence) as exn ->
          iconv_close cd;
          raise exn
      | Insufficient_space ->
          let buf = String.create (String.length dst.bytes * 2) in
          String.unsafe_blit dst.bytes 0 buf 0 (String.length dst.bytes);
          dst.bytes <- buf;
          dst.limit <- String.length buf
      | exn ->
          iconv_close cd;
          raise exn
  done;
  iconv_close cd;
  String.sub dst.bytes 0 dst.index
