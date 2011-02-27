(*
 * lt_utf8.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** UTF-8 manipulation *)

type t = string
    (** Type of UTF-8 encoded strings. *)

type code_point = int
    (** Type of unicode code-point. *)

exception Invalid of string * string
  (** [Invalid(error, text)] Exception raised when an invalid UTF-8
      encoded string is encountered. [text] is the faulty text and
      [error] is a description of the first error in [text]. *)

exception Out_of_bounds
  (** Exception raised when trying to access a character which is
      outside the bounds of a string. *)

(** {6 Validation} *)

val check : t -> string option
  (** [check str] checks that [str] is a valid UTF-8 encoded
      string. Returns [None] if it is the case, or [Some error]
      otherwise. *)

val validate : t -> unit
  (** Same as check but raises an exception in case the argument is
      not a valid text. *)

(** {6 Construction} *)

val singleton : code_point -> t
  (** [singleton ch] creates a string of length 1 containing only the
      given character. *)

val make : int -> code_point -> t
  (** [make n ch] creates a string of length [n] filled with [ch]. *)

val init : int -> (int -> code_point) -> t
  (** [init n f] returns the contenation of [singleton (f 0)],
      [singleton (f 1)], ..., [singleton (f (n - 1))]. *)

val rev_init : int -> (int -> code_point) -> t
  (** [rev_init n f] returns the contenation of [singleton (f (n -
      1))], ..., [singleton (f 1)], [singleton (f 0)]. *)

(** {6 Informations} *)

val length : t -> int
  (** Returns the length of the given string. *)

(** {6 Comparison} *)

val compare : t -> t -> int
  (** Compares two strings (in code point order). *)

(** {6 Random access} *)

val get : t -> int -> code_point
  (** [get str idx] returns the character at index [idx] in
      [str]. *)

(** {6 String manipulation} *)

val sub : t -> int -> int -> t
  (** [sub str ofs len] Returns the sub-string of [str] starting at
      [ofs] and of length [len]. *)

val break : t -> int -> t * t
  (** [break str pos] returns the sub-strings before and after [pos]
      in [str]. It is more efficient than creating two sub-strings
      with {!sub}. *)

val before : t -> int -> t
  (** [before str pos] returns the sub-string before [pos] in [str] *)

val after : t -> int -> t
  (** [after str pos] returns the sub-string after [pos] in [str] *)

val insert : t -> int -> t -> t
  (** [insert str pos sub] inserts [sub] in [str] at position
      [pos]. *)

val remove : t -> int -> int -> t
  (** [remove str pos len] removes the [len] characters at position
      [pos] in [str] *)

val replace : t -> int -> int -> t -> t
  (** [replace str pos len repl] replaces the [len] characters at
      position [pos] in [str] by [repl]. *)

(** {6 Tranformation} *)

val rev : t -> t
  (** [rev str] reverses all characters of [str]. *)

val concat : t -> t list -> t
  (** [concat sep l] returns the concatenation of all strings of [l]
      separated by [sep]. *)

val rev_concat : t -> t list -> t
  (** [concat sep l] returns the concatenation of all strings of [l]
      in reverse order separated by [sep]. *)

val explode : t -> code_point list
  (** [explode str] returns the list of all characters of [str]. *)

val rev_explode : t -> code_point list
  (** [rev_explode str] returns the list of all characters of [str] in
      reverse order. *)

val implode : code_point list -> t
  (** [implode l] returns the concatenation of all characters of [l]. *)

val rev_implode : code_point list -> t
  (** [rev_implode l] is the same as [implode (List.rev l)] but more
      efficient. *)

(** {6 Text traversals} *)

val iter : (code_point -> unit) -> t -> unit
  (** [iter f str] applies [f] an all characters of [str] starting
      from the left. *)

val rev_iter : (code_point -> unit) -> t -> unit
  (** [rev_iter f str] applies [f] an all characters of [str] starting
      from the right. *)

val fold : (code_point -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f str acc] applies [f] on all characters of [str]
      starting from the left, accumulating a value. *)

val rev_fold : (code_point -> 'a -> 'a) -> t -> 'a -> 'a
  (** [rev_fold f str acc] applies [f] on all characters of [str]
      starting from the right, accumulating a value. *)

val map : (code_point -> code_point) -> t -> t
  (** [map f str] maps all characters of [str] with [f]. *)

val rev_map : (code_point -> code_point) -> t -> t
  (** [rev_map f str] maps all characters of [str] with [f] in reverse
      order. *)

val filter : (code_point -> bool) -> t -> t
  (** [filter f str] filters characters of [str] with [f]. *)

val rev_filter : (code_point -> bool) -> t -> t
  (** [rev_filter f str] filters characters of [str] with [f] in
      reverse order. *)

val filter_map : (code_point -> code_point option) -> t -> t
  (** [filter_map f str] filters and maps characters of [str] with
      [f]. *)

val rev_filter_map : (code_point -> code_point option) -> t -> t
  (** [rev_filter_map f str] filters and maps characters of [str] with
      [f] in reverse order. *)

(** {6 Scanning} *)

val for_all : (code_point -> bool) -> t -> bool
  (** [for_all f text] returns whether all characters of [text] verify
      the predicate [f]. *)

val exists : (code_point -> bool) -> t -> bool
  (** [exists f text] returns whether at least one character of [text]
      verify [f]. *)

val count : (code_point -> bool) -> t -> int
  (** [count f text] returhs the number of characters of [text]
      verifying [f]. *)

(** {6 Tests} *)

val contains : t -> t -> bool
  (** [contains text sub] returns whether [sub] appears in [text] *)

val starts_with : t -> t -> bool
  (** [starts_with text prefix] returns [true] iff [s] starts with
      [prefix]. *)

val ends_with : t -> t -> bool
  (** [ends_with text suffix] returns [true] iff [s] ends with
      [suffix]. *)

(** {6 Stripping} *)

val strip : ?predicate : (code_point -> bool) -> t -> t
  (** [strip ?predicate text] returns [text] without its firsts and
      lasts characters that match [predicate]. [predicate] default to
      testing whether [text] is one of [' '], ['\t'], ['\r'] or
      ['\n']. For example:

      {[
        strip "\n  foo\n  " = "foo"
      ]}
  *)

val lstrip : ?predicate : (code_point -> bool) -> t -> t
  (** [lstrip ?predicate text] is the same as {!strip} but it only
      removes characters at the left of [text]. *)

val rstrip : ?predicate : (code_point -> bool) -> t -> t
  (** [lstrip ?predicate text] is the same as {!strip} but it only
      removes characters at the right of [text]. *)

val lchop : t -> t
  (** [lchop t] returns [t] without is first character. Returns [""]
      if [t = ""] *)

val rchop : t -> t
  (** [rchop t] returns [t] without is last character. Returns [""] if
      [t = ""]. *)
