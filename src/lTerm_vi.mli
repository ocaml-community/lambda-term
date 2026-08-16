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

(** Module for querying and calculating vi command navigating through text. *)
module Query :
  sig
    (** Returns the new position and the difference between the new position and the current position after n times [h] command. *)
    val left : int -> 'a Zed_edit.context -> int * int

    (** Returns the new position and the difference between the new position and the current position after n times [l] command. *)
    val right : ?newline:bool -> int -> 'a Zed_edit.context -> int * int

    (** Returns the position of the first character of the current line and the difference between the new position and the current position. *)
    val line_FirstChar : int -> 'b Zed_edit.context -> int * int

    (** Returns the position of the last character of the current line, with optional newline handling. *)
    val line_LastChar : ?newline:bool -> int -> 'a Zed_edit.context -> int

    (** Gets the category of a character. *)
    val get_category :
      ?nl_as_sp:bool ->
      Uchar.t ->
      Uucp.Gc.t

    (** Gets the boundary of the current line, i.e. the start and the stop of the line. *)
    val get_boundary : bool -> 'a Zed_edit.context -> int * int

    (** Checks if the category is a space. *)
    val is_space : Uucp.Gc.t -> bool

    (** Checks if a character is not a space. *)
    val is_not_space : Uucp.Gc.t -> bool

    (** Checks if two categories are equal for letters. *)
    val category_equal : Uucp.Gc.t -> Uucp.Gc.t -> bool

    (** Checks if two categories are all blanks or neither is. *)
    val category_equal_blank :
      Uucp.Gc.t ->
      Uucp.Gc.t -> bool

    (** Scan forward for the first character with a different Unicode category than the current one and return its positon. *)
    val next_category :
      ?nl_as_sp:bool ->
      ?is_equal:(Uucp.Gc.t -> Uucp.Gc.t -> bool) ->
      pos:int -> stop:int -> Zed_rope.t -> int

    (** Scan backward for the first character with a different Unicode category than the current one and return its positon. *)
    val prev_category :
      ?nl_as_sp:bool ->
      ?is_equal:(Uucp.Gc.t -> Uucp.Gc.t -> bool) ->
      pos:int -> start:int -> Zed_rope.t -> int

    (** Moves to the nth line. *)
    val goto_line : 'a Zed_edit.context -> int -> int

    (** Moves to the next line. *)
    val next_line : 'a Zed_edit.context -> int -> int

    (** Moves to the previous line. *)
    val prev_line : 'a Zed_edit.context -> int -> int

    (** Finds the next word, with custom category function to adjust its behavior. *)
    val next_word' :
      ?multi_line:bool ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:int -> Zed_rope.t -> int) ->
      pos:int -> stop:int -> Zed_rope.t -> int

    (** Finds the next word, i.e. vi command [w] *)
    val next_word :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int

    (** Finds the next WORD, i.e. vi command [W] *)
    val next_WORD :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int

    (** Finds the first non-blank character of the line, i.e. vi command [^] *)
    val line_FirstNonBlank : 'a -> 'b Zed_edit.context -> int

    (** Finds the previous word, with custom category function to adjust its behavior. *)
    val prev_word' :
      ?multi_line:bool ->
      prev_category:(nl_as_sp:bool -> pos:int -> start:int -> Zed_rope.t -> int) ->
      pos:int -> start:int -> Zed_rope.t -> int

    (** Finds the previous word, i.e. vi command [b] *)
    val prev_word :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int

    (** Finds the previous WORD, i.e. vi command [B] *)
    val prev_WORD :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int

    (** Finds the next word end, with custom category function to adjust its behavior. *)
    val next_word_end' :
      ?multi_line:bool ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:int -> Zed_rope.t -> int) ->
      pos:int -> stop:int -> Zed_rope.t -> int

    (** Finds the next word end, i.e. vi command [e] *)
    val next_word_end :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int

    (** Finds the next WORD end, i.e. vi command [E] *)
    val next_WORD_end :
      ?multi_line:bool -> pos:int -> stop:int -> Zed_rope.t -> int

    (** Finds the previous word end, with custom category function to adjust its behavior. *)
    val prev_word_end' :
      ?multi_line:bool ->
      prev_category:(nl_as_sp:bool -> pos:int -> start:int -> Zed_rope.t -> int) ->
      pos:int -> start:int -> Zed_rope.t -> int

    (** Finds the previous word end, i.e. vi command [ge] *)
    val prev_word_end :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int

    (** Finds the previous WORD end, i.e. vi command [gE] *)
    val prev_WORD_end :
      ?multi_line:bool -> pos:int -> start:int -> Zed_rope.t -> int

    (** Finds the first occurrence of a character. *)
    val occurrence_char :
      pos:int -> stop:int -> Zed_char.t -> Zed_rope.t -> int option

    (** Finds the first occurrence of a character backwards. *)
    val occurrence_char_back :
      pos:int -> start:int -> Zed_char.t -> Zed_rope.t -> int option

    (** Finds the first occurrence of a character satisfying a condition. *)
    val occurrence :
      pos:int ->
      stop:int ->
      cmp:(Zed_char.t -> bool) -> Zed_rope.t -> (int * Zed_char.t) option

    (** Finds the first occurrence of a character satisfying a condition backwards. *)
    val occurrence_back :
      pos:int ->
      start:int ->
      cmp:(Zed_char.t -> bool) -> Zed_rope.t -> (int * Zed_char.t) option

    (** Finds the first occurrence of a pair of left and right characters. *)
    val occurrence_pare_raw :
      pos:int ->
      level:int ->
      start:int ->
      stop:int -> Zed_char.t * Zed_char.t -> Zed_rope.t -> (int * int) option

    (** Finds the first occurrence of a pair of left and right characters, handling nesting.  *)
    val occurrence_pare :
      pos:int ->
      level:int ->
      start:int ->
      stop:int -> Zed_char.t * Zed_char.t -> Zed_rope.t -> (int * int) option

    (** Finds the occurrence of a bracket pair. *)
    val item_match : start:int -> stop:int -> int -> Zed_rope.t -> int option

    (** Finds a word and the appended spaces, returns its left and right position. *)
    val include_word' :
      ?multi_line:bool ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:int -> Zed_rope.t -> int) ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option

    (** Finds a word and the appended spaces, returns its left and right position, i.e. vi command [aw] *)
    val include_word :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option

    (** Finds a WORD and the appended spaces, returns its left and right position, i.e. vi command [aW] *)
    val include_WORD :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option

    (** Finds a word, returns its left and right position. *)
    val inner_word' :
      ?multi_line:bool ->
      prev_category:(nl_as_sp:bool -> pos:int -> start:int -> Zed_rope.t -> int) ->
      next_category:(nl_as_sp:bool -> pos:int -> stop:'a -> Zed_rope.t -> int) ->
      pos:int -> stop:'a -> Zed_rope.t -> (int * int) option

    (** Finds a word, returns its left and right position, i.e. vi command [iw] *)
    val inner_word :
      ?multi_line:bool ->
      pos:int -> stop:int -> Zed_rope.t -> (int * int) option

    (** Finds a WORD, returns its left and right position, i.e. vi command [iw] *)
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

