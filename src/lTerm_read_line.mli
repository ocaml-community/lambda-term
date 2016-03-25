(*
 * lTerm_read_line.mli
 * -------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Interactive line input *)

(** For a complete example of usage of this module, look at the shell
    example (examples/shell.ml) distributed with Lambda-Term. *)

open Zed
open React
open Core.Std
open Async.Std

type prompt = LTerm_text.t
    (** Type of prompts. *)

type history = Zed_utf8.t list
    (** Type of histories. It is a list of entries from the most
        recent to the oldest. *)

(** {6 Completion} *)

val common_prefix : string list -> string
  (** Returns the common prefix of a list of words. *)

val lookup : Zed_utf8.t -> Zed_utf8.t list -> Zed_utf8.t list
  (** [lookup word words] lookup for completion of [word] into
      [words]. It returns all words starting with [word]. *)

val lookup_assoc : Zed_utf8.t -> (Zed_utf8.t * 'a) list -> (Zed_utf8.t * 'a) list
  (** [lookup_assoc word words] does the same as {!lookup} but works
      on associative list. *)

(** {6 Actions} *)

(** Type of actions. *)
type action =
  [ LTerm_edit.action
  | `interrupt_or_delete_next_char
  | `complete
  | `complete_bar_next
  | `complete_bar_prev
  | `complete_bar_first
  | `complete_bar_last
  | `complete_bar
  | `history_prev
  | `history_next
  | `accept
  | `clear_screen
  | `prev_search
  | `next_search
  | `cancel_search
  | `break
  ] [@@deriving sexp, enumerate]

val bindings : action list Zed_input.Make(LTerm_event).t ref
  (** Bindings. *)

val bind : LTerm_event.t list -> action list -> unit
  (** [bind seq actions] associates [actions] to the given
      sequence. *)

val unbind : LTerm_event.t list -> unit
  (** [unbind seq] unbinds [seq]. *)

val doc_of_action : action -> string
  (** [doc_of_action action] returns a short description of the
      action. *)

(** {6 The read-line engine} *)

val macro : action Zed_macro.t
  (** The global macro recorder. *)

(** The current read-line mode. *)
type mode =
  | Edition
      (** Editing. *)
  | Search
      (** Backward search. *)
  | Set_counter
      (** Setting the macro counter value. *)
  | Add_counter
      (** Adding a value to the macro counter. *)

(** The read-line engine. If no clipboard is provided,
    {!LTerm_edit.clipboard} is used. If no macro recorder is provided,
    {!macro} is used. *)
class virtual ['a] engine : ?history : history -> ?clipboard : Zed_edit.clipboard -> ?macro : action Zed_macro.t -> unit -> object

  (** {6 Result} *)

  method virtual eval : 'a
    (** Evaluates the contents of the engine. *)

  (** {6 Actions} *)

  method insert : Zed_utf8.t -> unit
    (** Inserts the given string. Note that is it also possible to
        manipulate directly the edition context. *)

  method send_action : action -> unit
    (** Evolves according to the given action. *)

  (** {6 State} *)

  method edit : unit Zed_edit.t
    (** The edition engine used by this read-line engine. *)

  method context : unit Zed_edit.context
    (** The context for the edition engine. *)

  method clipboard : Zed_edit.clipboard
    (** The clipboard used by the edition engine. *)

  method macro : action Zed_macro.t
    (** The macro recorder. *)

  method input_prev : Zed_rope.t
    (** The input before the cursor. *)

  method input_next : Zed_rope.t
    (** The input after the cursor. *)

  method mode : mode signal
    (** The current mode. *)

  method stylise : bool -> LTerm_text.t * int
    (** Returns the stylised input and the position of the cursor. The
        argument is [true] if this is for the last drawing or [false]
        otherwise. *)

  method history : (Zed_utf8.t list * Zed_utf8.t list) signal
    (** The history zipper. *)

  method message : LTerm_text.t option signal
    (** A message to display in the completion box. When [None] the
        completion should be displayed, and when [Some msg] [msg]
        should be displayed. *)

  (** {6 Completion} *)

  method completion_words : (Zed_utf8.t * Zed_utf8.t) list signal
    (** Current possible completions. Each completion is of the form
        [(word, suffix)] where [word] is the completion itself and
        [suffix] is a suffix to add if the completion is choosen. *)

  method completion_index : int signal
    (** The position in the completion bar. *)

  method set_completion : ?index:int -> int -> (Zed_utf8.t * Zed_utf8.t) list -> unit
    (** [set_completion ?index start words] sets the current
        completions. [start] is the position of the beginning of the word
        being completed and [words] is the list of possible
        completions with their suffixes. [index] is the position in the completion
        bar, default to [0]. The result is made available
        through the {!completion_words} signal. *)

  method completion : unit
    (** Ask for computing completion for current input. This method
        should call {!set_completion}. *)

  method complete : unit
    (** Complete current input. This is the method called when the
        user presses Tab. *)

  method show_box : bool
    (** Whether to show the box or not. It default to [true]. *)
end

(** Abstract version of {!engine}. *)
class virtual ['a] abstract : object
  method virtual eval : 'a
  method virtual send_action : action -> unit
  method virtual insert : Zed_utf8.t -> unit
  method virtual edit : unit Zed_edit.t
  method virtual context : unit Zed_edit.context
  method virtual clipboard : Zed_edit.clipboard
  method virtual macro : action Zed_macro.t
  method virtual stylise : bool -> LTerm_text.t * int
  method virtual history : (Zed_utf8.t list * Zed_utf8.t list) signal
  method virtual message : LTerm_text.t option signal
  method virtual input_prev : Zed_rope.t
  method virtual input_next : Zed_rope.t
  method virtual completion_words : (Zed_utf8.t * Zed_utf8.t) list signal
  method virtual completion_index : int signal
  method virtual set_completion : ?index:int -> int -> (Zed_utf8.t * Zed_utf8.t) list -> unit
  method virtual completion : unit
  method virtual complete : unit
  method virtual show_box : bool
  method virtual mode : mode signal
end

(** {6 Predefined classes} *)

(** Simple read-line engine which returns the result as a string. *)
class read_line : ?history : history -> unit -> object
  inherit [Zed_utf8.t] engine

  method eval : Zed_utf8.t
    (** Returns the result as a UTF-8 encoded string. *)
end

(** Read-line engine for reading a password. The [stylise] method
    default to replacing all characters by ['*']. You can also for
    example completely disable displaying the password by doing:

    {[
      method stylise = ([||], 0)
    ]}

    Also showing completion is disabled.
*)
class read_password : unit -> object
  inherit [Zed_utf8.t] engine

  method eval : Zed_utf8.t
    (** Returns the result as a UTF-8 encoded string. *)
end

(** Read a keyword. *)
class ['a] read_keyword : ?history : history -> unit -> object
  inherit [('a, Zed_utf8.t) Result.t] engine

  method eval : ('a, Zed_utf8.t) Result.t
  (** If the input correspond to a keyword, returns its associated value. otherwise
      returns [Error input]. *)

  method keywords : (string * 'a) list
    (** List of keywords with their associated values. *)
end

(** {6 Running in a terminal} *)

module Res : sig
  type 'a t =
    | Ok of 'a
    | Interrupt (** Ctrl+D *)
    | Break     (** Ctrl+C *)
    | Error of exn
  [@@deriving sexp_of]
end

(** Class for read-line instances running in a terminal. *)
class virtual ['a] term : LTerm.t -> object
  inherit ['a] abstract

  method run : 'a Res.t Deferred.t
    (** Run this read-line instance. *)

  method private exec : action list -> 'a Res.t Deferred.t
    (** Executes a list of actions. Rememver to call [Zed_macro.add
        self#macro action] if you overload this method. *)

  method bind : LTerm_event.t list -> action list -> unit

  method private draw_update : unit Deferred.t
    (** Updates current display and put the cursor at current edition
        position. *)

  method private draw_success : unit Deferred.t
    (** Draws after accepting current input. *)

  method private draw_failure : unit Deferred.t
    (** Draws after an exception has been raised. *)

  method prompt : prompt signal
    (** The signal holding the prompt. *)

  method set_prompt : prompt signal -> unit
    (** Sets the prompt signal. *)

  method size : LTerm_geom.size signal
    (** The size of the terminal. This can be used for computing the
        prompt. *)

  method event_sequence : LTerm_event.t list signal
    (** The current event sequence. *)

  method completion_start : int signal
    (** Index of the first displayed word in the completion bar. *)

  method hide : unit
    (** Hide this read-line instance. It remains invisible until
        {!show} is called. *)

  method show : unit
    (** Show this read-line instance if it has been previously
        hidden. *)

  val mutable visible : bool
    (** Whether the instance is visible. *)
end
