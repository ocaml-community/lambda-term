(*
 * lt_term.mli
 * -----------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(** Terminal definitions *)

type t
  (** Type of terminals. *)

(** {6 Creation} *)

val create :
  ?windows : bool ->
  ?model : string ->
  ?incoming_encoding : string ->
  ?outgoing_encoding : string ->
  Lwt_unix.file_descr -> Lwt_unix.file_descr -> t
  (** [create ?windows ?model ?incoming_encoding ?outgoing_encoding
      input_fd outout_fd] creates a new terminal using [input_fd] for
      inputs and [output_fd] for outputs.

      - [windows] is a flag telling whether windows hack should be
      used. It defaults to [Lwt_sys.windows].

      - [model] is the type of the terminal, such as "rxvt" or
      "xterm". It defaults to the contents of the "TERM" environment
      variable, or to "dumb" if this one is not found. It is used to
      determine capabilities of the terminal, such as the number of
      colors. This is not used if [windows] is [true].

      - [incoming_encoding] is the encoding used for incoming data. It
      defaults to [Lt_windows.get_console_cp] if [windows] is [true]
      and [Lt_unix.system_encoding] otherwise.

      - [outgoing_encoding] is the encoding used for outgoing data. It
      defaults to [Lt_windows.get_console_output_cp] if [windows] is
      [true] and [Lt_unix.system_encoding] otherwise. Note that
      transliteration is used so printing unicode character on the
      terminal will never fail. *)

(** {6 Informations} *)

val model : t -> string
  (** Returns the model of the terminal. *)

val colors : t -> int
  (** Number of colors of the terminal. *)

val windows : t -> bool
  (** Whether the terminal is in windows mode or not. *)

val is_a_tty : t -> bool Lwt.t
  (** [is_a_tty term] returns whether the intput and output of the
      given terminal are connected to a tty device. *)

val incoming_is_a_tty : t -> bool Lwt.t
  (** [incoming_is_a_tty term] returns whether the input of [term] is
      a tty device. *)

val outgoing_is_a_tty : t -> bool Lwt.t
  (** [incoming_is_a_tty term] returns whether the output of [term] is
      a tty device. *)

exception Not_a_tty
  (** Exception raised when trying to use a function that can only be
      used on terminals. *)

(** {6 Sizes} *)

val get_size : t -> Lt_types.size Lwt.t
  (** Returns the current size of the terminal.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val set_size : t -> Lt_types.size -> unit Lwt.t
  (** Sets the current size of the terminal.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

(** {6 Modes} *)

val with_raw_mode : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_raw_mode term f] executes [f] while the terminal is in
      ``raw mode''. In this mode keyboard events are returned as they
      happen. In normal mode only complete line are returned.

      It raises {!Not_a_tty} if input of the given terminal is not
      tty. *)

val enable_mouse : t -> unit Lwt.t
  (** Enable mouse events reporting.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val disable_mouse : t -> unit Lwt.t
  (** Disable mouse events reporting.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

(** {6 Cursor} *)

val show_cursor : t -> unit Lwt.t
  (** Make the cursor visible.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val hide_cursor : t -> unit Lwt.t
  (** Make the cursor invisible.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val goto : t -> Lt_types.coord -> unit Lwt.t
  (** [goto term coord] moves the cursor to the given coordinates.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val goto_bol : t -> int -> unit Lwt.t
  (** [goto_bol term n] moves the cursor to the beginning of the [n]th
      next/previous line. [goto_bol term 0] moves the cursor to the
      beginning of the current line, [goto_bol term 1] moves the
      cursor to the beginning of the next line, [goto_bol term (-1)]
      moves the cursor the beginning of the previous line...

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

(** {6 State} *)

val save_state : t -> unit Lwt.t
  (** Save the current state of the terminal so it can be restored
      latter.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val load_state : t -> unit Lwt.t
  (** Load the previously saved state of the terminal.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

(** {6 Events} *)

val read_event : t -> Lt_event.t Lwt.t
  (** Reads and returns one event.

      It raises {!Not_a_tty} if the input of the given terminal is not
      a tty. *)

(** {6 Printing} *)

(** The general name of a printing function is [<prefix>print<suffixes>].

    Where [<prefix>] is one of:
    - ['f'], which means that the function takes as argument a terminal
    - nothing, which means that the function prints on {!stdout}
    - ['e'], which means that the function prints on {!stderr}

    and [<suffixes>] is a combination of:
    - ['l'] which means that a new-line character is printed after the message
    - ['f'] which means that the function takes as argument a {b format} instead
    of a string
    - ['s'] which means that the function takes as argument a styled
    string instead of a string

    Notes:
    - if the terminal is not a tty, styles are stripped.
    - non-ascii characters are recoded on the fly using the terminal
    encoding
*)

val fprint : t -> string -> unit Lwt.t
val fprintl : t -> string -> unit Lwt.t
val fprintf : t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val fprints : t -> Lt_style.text -> unit Lwt.t
val fprintlf : t -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val fprintls : t -> Lt_style.text -> unit Lwt.t
val print : string -> unit Lwt.t
val printl : string -> unit Lwt.t
val printf : ('a, unit, string, unit Lwt.t) format4 -> 'a
val prints : Lt_style.text -> unit Lwt.t
val printlf : ('a, unit, string, unit Lwt.t) format4 -> 'a
val printls : Lt_style.text -> unit Lwt.t
val eprint : string -> unit Lwt.t
val eprintl : string -> unit Lwt.t
val eprintf : ('a, unit, string, unit Lwt.t) format4 -> 'a
val eprints : Lt_style.text -> unit Lwt.t
val eprintlf : ('a, unit, string, unit Lwt.t) format4 -> 'a
val eprintls : Lt_style.text -> unit Lwt.t

(** {6 Rendering} *)

val render : t -> Lt_draw.matrix -> unit Lwt.t
  (** Render an offscreen array to the given terminal.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

val render_update : t -> Lt_draw.matrix -> Lt_draw.matrix -> unit Lwt.t
  (** [render_update displayed to_display] does the same as [render
      to_display] but assumes that [displayed] contains the current
      displayed text. This reduces the amount of text sent to the
      terminal.

      It raises {!Not_a_tty} if the output of the given terminal is
      not a tty. *)

(** {6 Misc} *)

val flush : t -> unit Lwt.t
  (** Flushes the underlying output channel used by the terminal. *)

(** {6 Well known instances} *)

val stdout : t
  (** Terminal using {!Lwt_unix.stdin} as input and {!Lwt_unix.stdout}
      as output. *)

val stderr : t
  (** Terminal using {!Lwt_unix.stdin} as input and {!Lwt_unix.stderr}
      as output. *)

(** {6 Low-level functions} *)

val get_size_from_fd : Lwt_unix.file_descr -> Lt_types.size Lwt.t
  (** [get_size_from_fd fd] returns the size of the terminal accessible via
      the given file descriptor. *)

val set_size_from_fd : Lwt_unix.file_descr -> Lt_types.size -> unit Lwt.t
  (** [set_size_from_fd fd size] tries to set the size of the terminal
      accessible via the given file descriptor. *)
