(*
 * lt_iconv.mli
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Iconv bindings. *)

type t
  (** Type of an iconv conversion descriptor. *)

exception Closed
  (** Exception raised when trying to use a closed iconv
      descriptor. *)

exception Unsupported of string * string
  (** [Unsupported(src, dst)] is the exception raised when the
      conversion from [src] to [dst] is not supported. *)

val iconv_open : to_code : string -> of_code : string -> t
  (** [iconv_open ~to_code ~of_code] creates a iconv descriptor which
      converts from strings encoded in [of_code] to string encoded in
      [to_code]. *)

val iconv_close : t -> unit
  (** [iconv_close cd] closes the given conversion descriptor. It does
      nothing if it is already closed. *)

(** Type of a chunk of data. *)
type data = {
  mutable bytes : string;
  (** The string containing the data. *)
  mutable index : int;
  (** The start of data in [bytes]. *)
  mutable limit : int;
  (** The end of data in [bytes]. *)
}

exception Invalid_sequence
  (** Exception raised when an invalid encoded sequence is encountered
      in the input, or when a character cannot be encoded in the
      destination encoding. *)

exception Unterminated_sequence
  (** Exception raised when a unterminated sequence is encountered at
      the end of the input. *)

exception Insufficient_space
  (** Excpetion raised when there is no more room in the output. *)

val iconv : t -> src : data -> dst : data -> unit
  (** [iconv cd ~src ~dst] converts bytes from [src] to [dst] using
      the given conversion descriptor and updates [src.index] and
      [dst.index]. *)

val reset : t -> unit
  (** [reset cd] resets [cd] to its initial state. *)

val recode : to_code : string -> of_code : string -> string -> string
  (** [recode ~to_code ~of_code str] recodes [str] from [of_code] to
      [to_code]. *)
