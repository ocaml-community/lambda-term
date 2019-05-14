open CamomileLibrary

module type Type =
  sig
    type char_intf
    type string_intf
    val empty_string : unit -> string_intf
    val of_char : Zed_char.t -> char_intf
    val of_string : Zed_string.t -> string_intf
    val to_char : char_intf -> Zed_char.t option * UChar.t list
    val to_string : string_intf -> Zed_string.t * UChar.t list
    val to_char_exn : char_intf -> Zed_char.t
    val to_string_exn : string_intf -> Zed_string.t
  end

module Zed : Type
  with type char_intf= Zed_char.t
  and type string_intf= Zed_string.t =
struct
  external id : 'a -> 'a = "%identity"
  type char_intf= Zed_char.t
  type string_intf= Zed_string.t

  let empty_string ()= Zed_string.empty ()
  let of_char= id
  let of_string= id
  let to_char ch= Some ch, []
  let to_string str= str, []
  let to_char_exn= id
  let to_string_exn= id
end

module UTF8 : Type
  with type char_intf= Zed_utf8.t
  and type string_intf= Zed_utf8.t =
struct
  module Zed_char_UTF8 = Zed_char.US(UTF8)
  module Zed_string_UTF8 = Zed_string.US(UTF8)

  type char_intf= Zed_utf8.t
  type string_intf= Zed_utf8.t

  let empty_string ()= ""
  let of_char= Zed_char.to_utf8
  let of_string= Zed_string.to_utf8
  let to_char= Zed_char_UTF8.to_t
  let to_string= Zed_string_UTF8.to_t
  let to_char_exn= Zed_char_UTF8.to_t_exn
  let to_string_exn= Zed_string.unsafe_of_utf8
end

