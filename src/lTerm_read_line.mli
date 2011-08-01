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

open CamomileLibrary
open React

exception Interrupt
  (** Exception raised when the user presses [Ctrl^D] with an empty
      input. *)

type prompt = LTerm_text.t
    (** Type of prompts. *)

(** {6 Completion} *)

val common_prefix : string list -> string
  (** Returns the common prefix of a list of words. *)

val lookup : Zed_utf8.t -> Zed_utf8.t list -> Zed_utf8.t list
  (** [lookup word words] lookup for completion of [word] into
      [words]. It returns all words starting with [word]. *)

val lookup_assoc : Zed_utf8.t -> (Zed_utf8.t * 'a) list -> (Zed_utf8.t * 'a) list
  (** [lookup_assoc word words] does the same as {!lookup} but works
      on associative list. *)

(** {8 History} *)

type history = string list
    (** Type of an history *)

val add_entry : string -> history -> history
  (** [add_entry line history] returns the history [history] plus
      [line] at the beginning. If [line] already appears at the
      beginning or contains only spaces, it is discarded. *)

val save_history : string -> history -> unit Lwt.t
  (** [save_history filename history] saves [history] to
      [filename]. The contents of [filename] is merged with [history]
      before saving. *)

val load_history : string -> history Lwt.t
  (** [load_history filename] loads history from [filename]. Returns
      the empty history if the the file does not exit. It fails with
      [Zed_utf8.Invalid] if one of the line of the history is not
      correctly UTF-8 encoded. *)

(** {6 Actions} *)

(** Type of actions. *)
type action =
  | Edit of Zed_edit.action
      (** An edition action. *)
  | Interrupt_or_delete_next_char
      (** Interrupt if at the beginning of an empty line, or delete
          the next character. *)
  | Complete
      (** Complete current input. *)
  | Complete_bar_next
      (** Go to the next possible completion in the completion bar. *)
  | Complete_bar_prev
      (** Go to the previous possible completion in the completion
          bar. *)
  | Complete_bar_first
      (** Goto the beginning of the completion bar. *)
  | Complete_bar_last
      (** Goto the end of the completion bar. *)
  | Complete_bar
      (** Complete current input using the completion bar. *)
  | History_prev
      (** Go to the previous entry of the history. *)
  | History_next
      (** Go to the next entry of the history. *)
  | Accept
      (** Accept the current input. *)
  | Clear_screen
      (** Clear the screen. *)
  | Prev_search
      (** Search backward in the history. *)
  | Cancel_search
      (** Cancel search mode. *)

val bindings : action list Zed_input.Make(LTerm_key).t ref
  (** Bindings. *)

val bind : LTerm_key.t list -> action list -> unit
  (** [bind seq actions] associates [actions] to the given
      sequence. *)

val unbind : LTerm_key.t list -> unit
  (** [unbind seq] unbinds [seq]. *)

val actions : (action * string) list
  (** List of actions with their names, except {!Edit}. *)

val doc_of_action : action -> string
  (** [doc_of_action action] returns a short description of the
      action. *)

val action_of_name : string -> action
  (** [action_of_name str] converts the given action name into an
      action. Action name are the same as variants name but lowercased
      and with '_' replaced by '-'. It raises [Not_found] if the name
      does not correspond to an action. It also recognizes zed
      actions. *)

val name_of_action : action -> string
  (** [name_of_action act] returns the name of the given action. *)

(** {6 The read-line engine} *)

class virtual ['a] engine : ?history : history -> unit -> object

  (** {6 Result} *)

  method virtual eval : 'a
    (** Evaluates the contents of the engine. *)

  (** {6 Actions} *)

  method insert : UChar.t -> unit
    (** Inserts the given character. Note that is it also possible to
        manipulate directly the edition context. *)

  method send_action : action -> unit
    (** Evolves according to the given action. *)

  (** {6 State} *)

  method edit : unit Zed_edit.t
    (** The edition engine used by this read-line engine. *)

  method context : unit Zed_edit.context
    (** The context for the edition engine. *)

  method input_prev : Zed_rope.t
    (** The input before the cursor. *)

  method input_next : Zed_rope.t
    (** The input after the cursor. *)

  method search_mode : bool signal
    (** Whether we are currently in backward search mode. *)

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

  method set_completion : int -> (Zed_utf8.t * Zed_utf8.t) list -> unit
    (** [set_completion index words] sets the current
        completions. [index] is the index of the beginning of the word
        being completed and [words] is the list of possible
        completions with their suffixes. The result is made available
        through the {!compltion_words} signal. *)

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
  method virtual insert : UChar.t -> unit
  method virtual edit : unit Zed_edit.t
  method virtual context : unit Zed_edit.context
  method virtual stylise : bool -> LTerm_text.t * int
  method virtual history : (Zed_utf8.t list * Zed_utf8.t list) signal
  method virtual message : LTerm_text.t option signal
  method virtual input_prev : Zed_rope.t
  method virtual input_next : Zed_rope.t
  method virtual completion_words : (Zed_utf8.t * Zed_utf8.t) list signal
  method virtual completion_index : int signal
  method virtual set_completion : int -> (Zed_utf8.t * Zed_utf8.t) list -> unit
  method virtual completion : unit
  method virtual complete : unit
  method virtual show_box : bool
  method virtual search_mode : bool signal
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

(** The result of reading a keyword. *)
type 'a read_keyword_result =
  | Rk_value of 'a
      (** The user typed a correct keyword and this is its associated
          value. *)
  | Rk_error of Zed_utf8.t
      (** The user did not enter a correct keyword and this is what he
          typed instead. *)

(** Read a keyword. *)
class ['a] read_keyword : ?history : history -> unit -> object
  inherit ['a read_keyword_result] engine

  method eval : 'a read_keyword_result
    (** If the input correspond to a keyword, returns its associated
        value. otherwise returns [`Error input]. *)

  method keywords : (string * 'a) list
    (** List of keywords with their associated values. *)
end

(** {6 Running in a terminal} *)

(** Class for read-line instances running in a terminal. *)
class virtual ['a] term : LTerm.t -> object
  inherit ['a] abstract

  method run : 'a Lwt.t
    (** Run this read-line instance. *)

  method draw_update : unit Lwt.t
    (** Updates current display and put the cursor at current edition
        position. *)

  method draw_success : unit Lwt.t
    (** Draws after accepting current input. *)

  method draw_failure : unit Lwt.t
    (** Draws after an exception has been raised. *)

  method prompt : prompt signal
    (** The signal holding the prompt. *)

  method set_prompt : prompt signal -> unit
    (** Sets the prompt signal. *)

  method size : LTerm_geom.size signal
    (** The size of the terminal. This can be used for computing the
        prompt. *)

  method completion_start : int signal
    (** Index of the first displayed word in the completion bar. *)

  method hide : unit Lwt.t
    (** Hide this read-line instance. It remains invisible until
        {!show} is called. *)

  method show : unit Lwt.t
    (** Show this read-line instance if it has been previously
        hidden. *)

  val mutable visible : bool
    (** Whether the instance is visible. *)
end
