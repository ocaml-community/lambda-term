(*
 * lt_windows.mli
 * --------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Windows specific functions *)

(** All these functions return [Lwt_sys.Not_available] on Unix. *)

(** {6 Codepage functions} *)

val get_acp : unit -> int
  (** Returns the current ANSI codepage. *)

val get_console_cp : unit -> int
  (** Returns the input codepage used by the console attached to the
      current process. *)

val set_console_cp : int -> unit
  (** Sets the input codepage used by the console attached to the
      current process. *)

val get_console_output_cp : unit -> int
  (** Returns the output codepage used by the console attached to the
      current process. *)

val set_console_output_cp : int -> unit
  (** Sets the output codepage used by the console attached to the
      current process. *)

(** {6 Console input} *)

type input =
  | Resize
  | Key of Lt_key.t
  | Mouse of Lt_mouse.t

val read_console_input : Lwt_unix.file_descr -> input Lwt.t
  (** [read_console_input fd] reads one input from the given file
      descriptor. *)

(** {6 Text attributes} *)

val set_console_text_attribute : Lwt_unix.file_descr -> int -> int -> unit
  (** [set_console_text_attribute fd foreground background] *)
