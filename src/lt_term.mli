(*
 * lt_term.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Terminal definitions *)

open React

type size = Lt_event.size = { lines : int; columns : int }
    (** Type of terminal sizes. *)

(** {6 Terminal class} *)

(** Class for terminals.

    - [model] is the type of the terminal, such as "rxvt" or
    "xterm". This is usually the contents of the "TERM" environment
    variable,

    - [input] is the file descriptor used to read events from the
    keyboard,

    - |input_encoding] is the encoding used to converts the input to
    unicode,

    - [output] is the file descriptor used to print to the terminal,

    - [output_encoding] is the encoding used to converts from unicode
    to the terminal encoding,

    - [windows] defaults to [Lwt_sys.windows]: if [true] it uses
    windows mode, if [false] it uses unix mode. *)
class t :
  ?model : string ->
  input : Lwt_unix.file_descr ->
  input_encoding : Encoding.t ->
  output : Lwt_unix.file_descr ->
  output_encoding : Encoding.t ->
  ?windows : bool -> unit ->
object

  (** Informations *)

  method model : string
    (** The model of the terminal. *)

  method windows : bool
    (** Whether the terminal is in windows mode or not. *)

  method get_size : size Lwt.t
    (** Returns the current size of the terminal. *)

  method set_size : size -> unit Lwt.t
    (** Sets the current size of the terminal. *)

  (** Modes *)

  method raw_mode : bool signal
    (** Whether the terminal is in ``raw mode''. In this mode keyboard
        events are returned as they happen. In normal mode only
        complete line are returned. *)

  method enter_raw_mode : unit Lwt.t
    (** Put the terminal in raw mode. On windows this does nothing
        except setting {!raw_mode} to [true]. *)

  method leave_raw_mode : unit Lwt.t
    (** Put the terminal in normal mode. On windows this does nothing
        except setting {!raw_mode} to [false]. *)

  (** State *)

  method save : unit Lwt.t
    (** Save the current state of the terminal so it can be restored
        latter. *)

  method load : unit Lwt.t
    (** Load the previously saved state of the terminal. *)

  (** {6 Events} *)

  method read_event : Lt_event.t Lwt.t
    (** Reads and returns one event. This method can be called only
        when the terminal is in raw mode. Otherwise several kind of
        events will not be reported. *)
end

(** {6 Well known instances} *)

val stdout : t
  (** Terminal using {!Lwt_unix.stdin} as input and {!Lwt_unix.stdout}
      as output. *)

val stderr : t
  (** Terminal using {!Lwt_unix.stdin} as input and {!Lwt_unix.stderr}
      as output. *)

(** {6 Low-level functions} *)

val get_size : Lwt_unix.file_descr -> size Lwt.t
  (** [get_size fd] returns the size of the terminal accessible via
      the given file descriptor. *)

val set_size : Lwt_unix.file_descr -> size -> unit Lwt.t
  (** [set_size fd size] tries to set the size of the terminal
      accessible via the given file descriptor. *)
