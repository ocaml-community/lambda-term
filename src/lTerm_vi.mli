(*
 * lTerm_vi.mli
 * ------------
 * Copyright : (c) 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Vi mode *)

(** Module for handling concurrent operations. *)
module Concurrent :
  sig
    (** Module for thread management and synchronization. *)
    module Thread :
      sig
        (** Thread type. *)
        type 'a t = 'a Lwt.t

        (** Binds a thread's result to a function. *)
        val bind : 'a t -> ('a -> 'b t) -> 'b t

        (** Create a thread returning a value directly. *)
        val return : 'a -> 'a t

        (** Combines two threads into one. *)
        val both : 'a t -> 'b t -> ('a * 'b) t

        (** Joins a list of threads. *)
        val join : unit t list -> unit t

        (** Picks the first thread that becomes resolved and tries to canel all other threads that are still pending, using {!Thread.cancle}. *)
        val pick : 'a t list -> 'a t

        (** The same as {!Thread.pick}, but it does not try to cancel the ramaining threads. *)
        val choose : 'a t list -> 'a t

        (** Runs an asynchronous action. *)
        val async : (unit -> unit t) -> unit

        (** Cancels a thread. *)
        val cancel : 'a t -> unit

        (** Sleeps for a given number of seconds. *)
        val sleep : float -> unit t

        (** Runs a thread and waits for its result. *)
        val run : 'a t -> 'a
      end

    (** Module for thread message box. *)
    module MsgBox :
      sig
        (** Message box type. *)
        type 'a t = 'a Lwt_mvar.t

        (** Creates a new message box. *)
        val create : unit -> 'a t

        (** Puts a value into the message box. *)
        val put : 'a t -> 'a -> unit Lwt.t

        (** Gets a value from the message box. *)
        val get : 'a t -> 'a Lwt.t
      end
  end

module Query :
  sig
    val left : int -> 'a Zed_edit.context -> int * int
    val right : ?newline:bool -> int -> 'a Zed_edit.context -> int * int
    val line_FirstChar : 'a -> 'b Zed_edit.context -> int * int
    val line_LastChar : ?newline:bool -> int -> 'a Zed_edit.context -> int
    val get_category :
      ?nl_as_sp:bool ->
      Uchar.t ->
      Uucp.Gc.t
    val get_boundary : bool -> 'a Zed_edit.context -> int * int
    val is_space : [> `Cc | `Mn | `Zl | `Zp | `Zs ] -> bool
    val is_not_space : [> `Cc | `Mn | `Zl | `Zp | `Zs ] -> bool
    val category_equal : ([> `Ll | `Lu ] as 'a) -> 'a -> bool
    val category_equal_blank :
      [> `Cc | `Mn | `Zl | `Zp | `Zs ] ->
      [> `Cc | `Mn | `Zl | `Zp | `Zs ] -> bool
    val next_category :
      ?nl_as_sp:bool ->
      ?is_equal:(Uucp.Gc.t -> Uucp.Gc.t -> bool) ->
      pos:int -> stop:int -> Zed_rope.t -> int
    val prev_category :
      ?nl_as_sp:bool ->
      ?is_equal:(Uucp.Gc.t -> Uucp.Gc.t -> bool) ->
      pos:int -> start:int -> Zed_rope.t -> int
    val goto_line : 'a Zed_edit.context -> int -> int
    val next_line : 'a Zed_edit.context -> int -> int
    val prev_line : 'a Zed_edit.context -> int -> int
    val next_word' :
      ?multi_line:bool ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:int -> Zed_rope.t -> int) ->
      pos:int -> stop:int -> Zed_rope.t -> int
    val next_word :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int
    val next_WORD :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int
    val line_FirstNonBlank : 'a -> 'b Zed_edit.context -> int
    val prev_word' :
      ?multi_line:bool ->
      prev_category:(nl_as_sp:bool -> pos:int -> start:int -> Zed_rope.t -> int) ->
      pos:int -> start:int -> Zed_rope.t -> int
    val prev_word :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int
    val prev_WORD :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int
    val next_word_end' :
      ?multi_line:bool ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:int -> Zed_rope.t -> int) ->
      pos:int -> stop:int -> Zed_rope.t -> int
    val next_word_end :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int
    val next_WORD_end :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int
    val prev_word_end' :
      ?multi_line:bool ->
      prev_category:(nl_as_sp:bool -> pos:int -> start:int -> Zed_rope.t -> int) ->
      pos:int -> start:int -> Zed_rope.t -> int
    val prev_word_end :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int
    val prev_WORD_end :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int
    val occurrence_char :
      pos:int -> stop:int -> Zed_char.t -> Zed_rope.t -> int option
    val occurrence_char_back :
      pos:int -> start:int -> Zed_char.t -> Zed_rope.t -> int option
    val occurrence :
      pos:int ->
      stop:int ->
      cmp:(Zed_char.t -> bool) -> Zed_rope.t -> (int * Zed_char.t) option
    val occurrence_back :
      pos:int ->
      start:int ->
      cmp:(Zed_char.t -> bool) -> Zed_rope.t -> (int * Zed_char.t) option
    val occurrence_pare_raw :
      pos:int ->
      level:int ->
      start:int ->
      stop:int -> Zed_char.t * Zed_char.t -> Zed_rope.t -> (int * int) option
    val occurrence_pare :
      pos:int ->
      level:int ->
      start:int ->
      stop:int -> Zed_char.t * Zed_char.t -> Zed_rope.t -> (int * int) option
    val item_match : start:int -> stop:int -> int -> Zed_rope.t -> int option
    val include_word' :
      ?multi_line:bool ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:int -> Zed_rope.t -> int) ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option
    val include_word :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option
    val include_WORD :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option
    val inner_word' :
      ?multi_line:bool ->
      prev_category:(nl_as_sp:bool -> pos:int -> start:int -> Zed_rope.t -> int) ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:'a -> Zed_rope.t -> int) ->
      pos:int -> stop:'a -> Zed_rope.t -> (int * int) option
    val inner_word :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option
    val inner_WORD :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option
  end


(**
 * Module for the Vi editor interface.
 *)
module Vi : module type of Mew_vi.Core.Make (Concurrent)


(**
 * An instance of the edit class for Vi mode.
 *)
class edit : state ->
  object
    (** MsgBox for action output. *)
    val action_output : Vi.Edit_action.t Concurrent.MsgBox.t

    (** Current mode of the editor. *)
    val mutable curr_mode : Vi.Base.Mode.t

    (** Resolver configuration. *)
    val config : Vi.Interpret.Resolver.config

    (** Action output MsgBox. *)
    method action_output : Vi.Edit_action.t Concurrent.MsgBox.t

    (** Bindings for the current mode. *)
    method bindings : Vi.Base.Mode.action Vi.Base.Mode.KeyTrie.node

    (** Get the current mode. *)
    method getMode : Vi.Base.Mode.t

    (** Get the input key MsgBox. *)
    method i : Mew_vi.Key.t Concurrent.MsgBox.t

    (** Input a key into the [i] MsgBox. *)
    method keyin : Mew_vi.Key.t -> unit Concurrent.Thread.t

    (** The output key MsgBox. *)
    method o : Mew_vi.Key.t Concurrent.MsgBox.t

    (** Set the current mode by name. *)
    method setMode : Vi.Base.Mode.name -> unit

    (** Get the timeout value. *)
    method timeout : float

    (** Get the content of a register by name. *)
    method get_register : string -> Vi.Interpret.Register.content option

    (** Set the content of a register by name. *)
    method set_register : string -> Vi.Interpret.Register.content -> unit
  end
and state :
  object
    (** The default mode of the editor. *)
    val mutable default_mode : Vi.Base.Mode.name * Vi.Base.Mode.t

    (** The timeout value of the editor. *)
    val mutable timeout : float

    (** Get the default mode. *)
    method default_mode : Vi.Base.Mode.name * Vi.Base.Mode.t

    (** The edit object. *)
    method edit : Vi.Base.edit

    (** The set of modes. *)
    method modes : Vi.Base.Mode.t Vi.Base.Mode.Modes.t

    (** Get the timeout value. *)
    method timeout : float

    (** Get the content of a register by name. *)
    method get_register : string -> Vi.Interpret.Register.content option

    (** Set the content of a register by name. *)
    method set_register : string -> Vi.Interpret.Register.content -> unit

    (** Get all registers. *)
    method get_registers : Vi.Interpret.Register.content Vi.Interpret.RegisterMap.t

    (** Set all registers. *)
    method set_registers : Vi.Interpret.Register.content Vi.Interpret.RegisterMap.t -> unit

    (** Get the Vi edit object. *)
    method vi_edit : edit
  end


(** Convert an LTerm key code to a Mew_vi key code. *)
val of_lterm_code : LTerm_key.code -> Mew_vi.Key.code

(** Convert a Mew_vi key code to an LTerm key code. *)
val of_vi_code : Mew_vi.Key.code -> LTerm_key.code

(** Convert an LTerm key to a Mew_vi key. *)
val of_lterm_key : LTerm_key.t -> Mew_vi.Key.t

(** Convert a Mew_vi key to an LTerm key. *)
val of_vi_key : Mew_vi.Key.t -> LTerm_key.t

open LTerm_read_line_base


(**
 * Performs one step of the Vi editing loop.
 *)
val perform :
  edit ->
  'a Zed_edit.context ->
  (action list -> 'b loop_result Concurrent.Thread.t) ->
  Vi.Vi_action.t -> 'b loop_result Concurrent.Thread.t

