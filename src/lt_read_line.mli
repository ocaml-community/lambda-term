(*
 * lt_read_line.mli
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Interactive line input *)

open CamomileLibrary
open React

exception Interrupt
  (** Exception raised when the user presses [Ctrl^D] with an empty
      input. *)

type prompt = Lt_style.text
    (** Type of prompts. *)

(** {6 Completion} *)

type string_set = Set.Make(String).t
    (** Type of sets of UTF-8 encoded strings. *)

type completion = unit Zed_edit.context -> string_set Lwt.t
  (** Type of a completion function. It takes as argument an edition
      context and must:

      - insert the found completion at the context cursor
      - returns a list of possibilities

      Note: the thread launched by the completion function is
      cancelled using {!Lwt.cancel} if the user continue typing
      text before the completion function terminates. *)

type completion_mode = [ `Classic | `Real_time | `None ]
    (** The completion mode.

        - [`Classic] means that when the user hit [Tab] a list of
          possible completions is proposed,

        - [`Real_time] means that possible completions are shown to
          the user as he types, and he can navigate in them with
          [Meta+left], [Meta+right]

        - [`None] means no completion at all *)

val lookup : string -> string_set -> string * string_set
  (** [lookup word words] lookup for completion of [word] into
      [words]. It returns [(prefix, possibilities)] where
      [possibilities] are all words starting with [word] and [prefix]
      is the longest common prefix of [possibilities]. *)

val complete : ?suffix : string -> unit Zed_edit.context -> string -> string_set -> string_set
  (** [complete ?suffix ctx word words] basic completion
      functions.

      - [ctx] is a context for editing the edition engine, pointing to
        the end of the word to complete,
      - [word] is the word to complete,
      - [words] is a list of possible completions for [word].

      It works as follow:

      - if there is no completion, nothing append,
      - if there is only one, the word is completed and [suffix] is
        inserted after (it default to [" "]),
      - if there are several possible completions, the word is
        completed using the longest prefix.

      The result is the set of all possible completions. *)

val print_words : Lt_term.t -> string list -> unit Lwt.t
  (** [print_words term strs] pretty-prints a list of words. *)

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

(** {6 High-level functions} *)

val read_line :
  ?term : Lt_term.t ->
  ?history : history ->
  ?complete : completion ->
  ?clipboard : Zed_edit.clipboard ->
  ?completion_mode : completion_mode ->
  ?prompt : prompt -> unit -> string Lwt.t
  (** [readline ?history ?complete ?mode ?prompt ()] inputs some text
      from the user. If input is not a terminal, it defaults to
      [Lwt_text.read_line Lwt_text.stdin].

      If @param mode contains the current completion mode. It defaults
      to [`real_time].

      @param prompt defaults to [Lwt_term.Text "# "] *)
(*
type password_style = [ `Empty | `Clear | `Text of string ]
    (** Style which indicate how the password is echoed to the user:

        - with [`Empty] nothing is printed
        - with [`Clear] the password is displayed has it
        - with [`Text ch] all characters are replaced by [ch] *)

val read_password :
  ?clipboard : Zed_edit.clipboard ->
  ?style : password_style ->
  ?prompt : prompt -> unit -> string Lwt.t
  (** [read_password ?clipboard ?clear ~prompt ()] inputs a password
      from the user. This function fails if input is not a terminal.

      @param style defaults to [`text "*"].
  *)

val read_keyword :
  ?history : history ->
  ?case_sensitive : bool ->
  ?mode : completion_mode ->
  ?prompt : prompt ->
  values :  (string * 'value) list -> unit -> 'value Lwt.t
  (** [read_keyword ?history ?case_sensitive ?mode ~prompt ~keywords
      ()] reads one word which is a member of [words]. And returns
      which keyword the user choosed.

      [case_sensitive] default to [false]. *)

val read_yes_no : ?history : history -> ?mode : completion_mode -> ?prompt : prompt -> unit -> bool Lwt.t
  (** [read_yes_no ?history ?dynamic prompt ()] is the same as:

      {[
        read_keyword ?history ?dynamic prompt [("yes", true); ("no", false)] ()
      ]}
  *)
*)
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

class virtual ['a] engine : ?history : history -> ?completion_mode : completion_mode -> unit -> object
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

  method complete : string_set Lwt.t
    (** Complete current input. *)

  method virtual print_completion : string_set -> unit Lwt.t
    (** Prints possible completion on the terminal. *)
end

(** Abstract version of {!engine}. *)
class virtual ['a] abstract : object
  method virtual eval : 'a
  method virtual send_action : action -> unit
  method virtual edit : unit Zed_edit.t
  method virtual context : unit Zed_edit.context
  method virtual stylise : Lt_style.text * Lt_style.text
  method virtual complete : string_set Lwt.t
  method virtual print_completion : string_set -> unit Lwt.t
end

(** {6 Predefined classes} *)

class virtual read_line : ?history : history -> ?completion_mode : completion_mode -> unit -> object
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

  val mutable prompt : prompt signal
    (** The signal holding the prompt. *)

  method size : Lt_types.size signal
    (** The size of the terminal. This can be used for computing the
        prompt. *)

  method hide : unit Lwt.t
    (** Hide this read-line instance. It remains invisible until
        {!show} is called. *)

  method show : unit Lwt.t
    (** Show this read-line instance if it has been previously
        hidden. *)

  val mutable visible : bool
    (** Whether the instance is visible. *)

  method print_completion : string_set -> unit Lwt.t
    (** Prints possible completion on the terminal. *)
end
