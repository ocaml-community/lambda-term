(*
 * lt_read_line.mli
 * ----------------
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

type prompt = Lt_style.text
    (** Type of prompts. *)

(** {6 Completion} *)

val lookup : string -> Zed_utf8.t list -> string * Zed_utf8.t list
  (** [lookup word words] lookup for completion of [word] into
      [words]. It returns [(prefix, possibilities)] where
      [possibilities] are all words starting with [word] and [prefix]
      is the longest common prefix of [possibilities]. *)

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

val bindings : (Lt_key.t, action) Hashtbl.t
  (** Bindings. *)

val bind : Lt_key.t -> action option
  (** [bind key] returns the action associated to the given key, if
      any. *)

(** {6 The read-line engine} *)

class virtual ['a] engine : ?history : history -> unit -> object
  method virtual eval : 'a
    (** Evaluates the contents of the engine. *)

  method send_action : action -> unit
    (** Evolve according to the given action. *)

  method edit : unit Zed_edit.t
    (** The edition engine used by this read-line engine. *)

  method context : unit Zed_edit.context
    (** The context for the edition engine. *)

  method stylise : Lt_style.text * Lt_style.text
    (** Stylise current input. It must returns the text before the
        cursor and the text after the cursor. *)

  method input_prev : Zed_rope.t
    (** The input before the cursor. *)

  method input_next : Zed_rope.t
    (** The input after the cursor. *)

  method completion_words : (Zed_utf8.t * Zed_utf8.t) list signal
    (** Current possible completions. Each completion is of the form
        [(word, suffix)] where [word] is the completion itself and
        [suffix] is a suffix to add if the completion is choosen. *)

  method completion_index : int signal
    (** The position in the completion bar. *)

  method complete : (int * (Zed_utf8.t * Zed_utf8.t) list) Lwt.t
    (** Compute possible completions for current input. It returns the
        index of the beginning of the word being completed and the
        list of possible completions with their suffixes. The result
        is made available through the {!completions} signal. This
        thread may be canceled using {!Lwt.cancel}. *)
end

(** Abstract version of {!engine}. *)
class virtual ['a] abstract : object
  method virtual eval : 'a
  method virtual send_action : action -> unit
  method virtual edit : unit Zed_edit.t
  method virtual context : unit Zed_edit.context
  method virtual stylise : Lt_style.text * Lt_style.text
  method virtual input_prev : Zed_rope.t
  method virtual input_next : Zed_rope.t
  method virtual completion_words : (Zed_utf8.t * Zed_utf8.t) list signal
  method virtual completion_index : int signal
  method virtual complete : (int * (Zed_utf8.t * Zed_utf8.t) list) Lwt.t
end

(** {6 Predefined classes} *)

class virtual read_line : ?history : history -> unit -> object
  inherit [Zed_utf8.t] engine
  method eval : Zed_utf8.t
    (** Returns the result as a UTF-8 encoded string. *)
end

(** {6 Running in a terminal} *)

(** Class for read-line instances running in a terminal. *)
class virtual ['a] term : Lt_term.t -> object
  inherit ['a] abstract

  method run : 'a Lwt.t
    (** Run this read-line instance. *)

  method draw_update : unit Lwt.t
    (** Updates current display and put the cursor at current edition
        position. *)

  method draw_simple : unit Lwt.t
    (** Draws everything but without the intention of updating
        latter. Also the cursor should be left after the text
        displayed. *)

  method draw_accept : unit Lwt.t
    (** Draws after accepting. *)

  method prompt : prompt signal
    (** The signal holding the prompt. *)

  method set_prompt : prompt signal -> unit
    (** Sets the prompt signal. *)

  method size : Lt_types.size signal
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
