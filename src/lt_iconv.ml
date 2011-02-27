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

let equal enc1 enc2 =
  let rec loop i1 i2 =
    if i1 = String.length enc1 || String.unsafe_get enc1 i1 = '/' then
      i2 = String.length enc2 || String.unsafe_get enc2 i2 = '/'
    else if i2 = String.length enc2 || String.unsafe_get enc2 i2 = '/' then
      false
    else
      let ch1 = Char.lowercase (String.unsafe_get enc1 i1)
      and ch2 = Char.lowercase (String.unsafe_get enc2 i2) in
      match ch1, ch2 with
        | ('a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':'), ('a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':') ->
            ch1 = ch2 && loop (i1 + 1) (i2 + 1)
        | ('a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':'), _ ->
            loop i1 (i2 + 1)
        | _, ('a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':') ->
            loop (i1 + 1) i2
        | _ ->
            loop (i1 + 1) (i2 + 1)
  in
  loop 0 0

let recode_with cd str =
  reset cd;
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
      | Insufficient_space ->
          let buf = String.create (String.length dst.bytes * 2) in
          String.unsafe_blit dst.bytes 0 buf 0 (String.length dst.bytes);
          dst.bytes <- buf;
          dst.limit <- String.length buf
  done;
  String.sub dst.bytes 0 dst.index

let recode ~to_code ~of_code str =
  if equal to_code of_code then
    str
  else begin
    let cd = iconv_open ~to_code ~of_code in
    let str = try recode_with cd str with exn -> iconv_close cd; raise exn in
    iconv_close cd;
    str
  end
