(*
 * lTerm.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open StdLabels
module Bigstring = LTerm_bigstring

module Screen = struct
  type t = Main | Alternative

  let state_change_sequence a b =
    match a, b with
    | Alternative, Main -> "\027[?1049l"
    | Main, Alternative -> "\027[?1049h"
    | _ -> ""
end

module Mouse_events = struct
  type t =
    | Disabled
    | Buttons
    | Any

  let state_change_sequence a b =
    match a, b with
    | Disabled, Disabled
    | Buttons , Buttons
    | Any     , Any -> ""
    | Disabled, Buttons ->
      "\027[?1000h\027[?1002h\027[?1005h"
    | Disabled, Any ->
      "\027[?1000h\027[?1003h\027[?1005h"
    | Buttons, Any ->
      "\027[?1002l\027[?1003h"
    | Any, Buttons ->
      "\027[?1003l\027[?1002h"
    | Buttons, Disabled ->
      "\027[?1002l\027[?1000l\027[?1005l"
    | Any, Disabled ->
      "\027[?1003l\027[?1000l\027[?1005l"
end

module Mode = struct
  type t =
    { echo    : bool
    ; raw     : bool
    ; signals : bool
    ; mouse   : Mouse_events.t
    ; screen  : Screen.t
    }

  let echo    t = t.echo
  let raw     t = t.raw
  let signals t = t.signals
  let mouse   t = t.mouse
  let screen  t = t.screen

  let make ?(echo=true) ?(raw=false) ?(signals=true)
        ?(mouse=Mouse_events.Disabled)
        ?(screen=Screen.Main) () =
    { echo
    ; raw
    ; signals
    ; mouse
    ; screen
    }
  ;;

  let default = make ()

  let set ?echo ?raw ?signals ?mouse ?screen t =
    let choose orig repl =
      match repl with
      | None -> orig
      | Some x -> x
    in
    { echo    = choose t.echo    echo
    ; raw     = choose t.raw     raw
    ; signals = choose t.signals signals
    ; mouse   = choose t.mouse   mouse
    ; screen  = choose t.screen  screen
    }

  let termios_fields_equal a b =
    a.echo    = b.echo    &&
    a.raw     = b.raw     &&
    a.signals = b.signals
  ;;

  let non_termios_fields_equal a b =
    a.mouse  = b.mouse &&
    a.screen = b.screen
  ;;

  let copy_non_termios_fields t ~from =
    { t with mouse = from.mouse; screen = from.screen }
  ;;

  let erase_non_termios_fields t = copy_non_termios_fields t ~from:default
end

module Cursor = struct
  type t = Visible | Hidden [@@deriving sexp_of]
end

module Terminal_io = struct
  let rec get_attr fd =
    try
      Unix.tcgetattr fd
    with Unix.Unix_error (EINTR, _, _) ->
      get_attr fd
  ;;

  let rec set_attr fd attr =
    try
      Unix.tcsetattr fd TCSAFLUSH attr
    with Unix.Unix_error (EINTR, _, _) ->
      set_attr fd attr
  ;;

  let get_attr_no_err fd =
    match get_attr fd with
    | x -> Some x
    | exception _ -> None
  ;;

  let set_attr_no_err fd attr =
    try
      set_attr fd attr
    with _ ->
      ()
  ;;
end

(* +-----------------------------------------------------------------+
   | TTYs sizes                                                      |
   +-----------------------------------------------------------------+ *)

external get_size_from_fd : Unix.file_descr -> LTerm_geom.size
  = "lt_term_get_size_from_fd"
(*
  external set_size_from_fd : Unix.file_descr -> LTerm_geom.size -> unit
  = "lt_term_set_size_from_fd"
*)

(* +-----------------------------------------------------------------+
   | Terminal synchronisation                                        |
   +-----------------------------------------------------------------+ *)

let protect mutex ~f =
  Mutex.lock mutex;
  match f () with
  | x           -> Mutex.unlock mutex; x
  | exception e -> Mutex.unlock mutex; raise e

module Notifier = struct
  type ('a, 'b) methods =
    { new_request : unit -> 'a * 'b
    ; notify      : 'a -> unit
    }

  type 'a t = T : (_, 'a) methods -> 'a t

  let make ~new_request ~notify () =
    T { new_request
      ; notify
      }

  module Request = struct
    type 'a notifier = 'a t
    type t = T : ('a, _) methods * 'a -> t

    let notify (T (notifier, request)) =
      notifier.notify request
    ;;

    let dummy = T ({ new_request = (fun () -> ((), ()))
                   ; notify      = ignore
                   },
                   ())
  end

  let new_request (T methods) =
    let sender, receiver = methods.new_request () in
    (receiver, Request.T (methods, sender))
  ;;

  module Default = struct
    type request =
      { mutable notified : bool
      ; mutex            : Mutex.t
      ; cond             : Condition.t
      }

    let create () =
      { notified = false
      ; mutex    = Mutex.create ()
      ; cond     = Condition.create ()
      }

    (* Pool conditions and mutexes as creating a lot of them is currently costly
       Gc-wise *)
    let pool = ref []
    let pool_mutex = Mutex.create ()

    let new_request () =
      protect pool_mutex ~f:(fun () ->
        match !pool with
        | [] -> create ()
        | x :: l ->
          pool := l;
          x)
    ;;

    let free t =
      protect pool_mutex ~f:(fun () ->
        pool := t :: !pool)
    ;;

    let notify t =
      protect t.mutex ~f:(fun () ->
        t.notified <- true;
        Condition.signal t.cond)
    ;;

    let wait t =
      protect t.mutex ~f:(fun () ->
        if not t.notified then Condition.wait t.cond t.mutex;
        t.notified <- false;
        free t)
    ;;

    let wait_without_freeing t =
      protect t.mutex ~f:(fun () ->
        if not t.notified then Condition.wait t.cond t.mutex;
        t.notified <- false)
    ;;

    let t = make ~new_request:(fun () -> let r = new_request () in (r, r)) ~notify ()
  end

  module Blocking = struct
    let new_request () =
      let req = Default.new_request () in
      (req, lazy (Default.wait req))
    ;;

    let notify = Default.notify

    let t = make ~new_request ~notify ()
  end

  let blocking = Blocking.t
end

module Sync = struct
  type end_of_display = Do_not_change | Set of int option

  type t =
    { len            : int
    ; end_of_display : end_of_display
    ; mode           : Mode.t
    ; cursor         : Cursor.t
    ; request        : Notifier.Request.t
    }

  let refresh ~mode ~cursor =
    { len = 0
    ; end_of_display = Do_not_change
    ; mode
    ; cursor
    ; request = Notifier.Request.dummy
    }
  ;;
(*
  let merge l =
    match l with
    | [] -> assert false
    | [t] -> t
    | first :: rest ->
      let ivars = List.map l ~f:(fun t -> t.finished) in
      let finished = Ivar.create () in
      upon (Ivar.read finished) (fun () ->
        List.iter ivars ~f:(fun ivar -> Ivar.fill ivar ()));
      List.fold rest ~init:{ first with finished } ~f:(fun acc t ->
        { acc with
           len = acc.len + t.len
         ; end_of_display = (match t.end_of_display with
           | `Do_not_change -> acc.end_of_display
           | `Set _ as x -> x)
         ; mode = t.mode
         ; cursor = t.cursor })
  ;;
*)
end

module Thread_safe_queue : sig
  type 'a t [@@deriving sexp_of]

  val create  : unit -> _ t
  val send    : 'a t -> 'a -> unit
  val wait    : 'a t -> 'a
end = struct
  type 'a t =
    { queue           : 'a Queue.t
    ; mutex           : Mutex.t
    ; wait            : Condition.t
    ; mutable waiters : int
    }
  [@@deriving sexp_of]

  let create () =
    { queue   = Queue.create ()
    ; mutex   = Mutex.create ()
    ; wait    = Condition.create ()
    ; waiters = 0
    }
  ;;

  let wait t =
    protect t.mutex ~f:(fun () ->
      if Queue.is_empty t.queue then begin
        t.waiters <- t.waiters + 1;
        Condition.wait t.wait t.mutex;
      end;
      Queue.pop t.queue)
  ;;

  let send t x =
    protect t.mutex ~f:(fun () ->
      Queue.push x t.queue;
      if t.waiters > 0 then begin
        t.waiters <- t.waiters - 1;
        Condition.signal t.wait
      end)
  ;;
end

module State = struct
  type t = Active | Inactive | Closed [@@deriving sexp_of]

  let is_closed = function
    | Closed -> true
    | _      -> false
end

type ('a, 'b) poll_result =
  | Ready   of 'a
  | Pending of 'b

module Event_queue : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  val send : t -> LTerm_event.t -> unit
  val send_many : t -> LTerm_event.t list -> unit
  val poll : t -> notifier:'a Notifier.t -> (LTerm_event.t, 'a) poll_result

  val wait_for_reader : t -> unit

  val set_active : t -> bool -> unit
  val close      : t -> unit
end = struct
  module Queue : sig
    (* Event queue with the following invariants:

       - it contains at most one Resize
       - it contains at most one Resume
       - it never contains a Resume followed (immediately or not) by a Resize

       It can however contains a Resize followed (immediately or not) by a Resume. *)
    type t [@@deriving sexp_of]
    val create  : unit -> t
    val enqueue : t -> LTerm_event.t -> unit
    val enqueue_many : t -> LTerm_event.t list -> unit
    val dequeue : t -> LTerm_event.t option
    val clear   : t -> unit
    val is_empty : t -> bool
  end = struct
    type t =
      { queue : LTerm_event.t Queue.t
      ; mutable resume_in_queue : bool
      ; mutable resize_in_queue : bool
      } [@@deriving sexp_of]

    let create () =
      { queue = Queue.create ()
      ; resume_in_queue = false
      ; resize_in_queue = false
      }
    ;;

    let enqueue t (ev : LTerm_event.t) =
      match ev with
      | Resize ->
        (* If the user already has to handle a Resize/Resume there is no need to queue
           another Resize. This avoid accumulating events when the terminal is not in
           use. *)
        if not (t.resize_in_queue || t.resume_in_queue) then begin
          t.resize_in_queue <- true;
          Queue.push ev t.queue
        end;
      | Resume ->
        if not t.resume_in_queue then begin
          t.resume_in_queue <- true;
          Queue.push ev t.queue
        end
      | _ ->
        Queue.push ev t.queue
    ;;

    let rec enqueue_many t l =
      match l with
      | [] -> ()
      | ev :: l -> enqueue t ev; enqueue_many t l
    ;;

    let dequeue t =
      if Queue.is_empty t.queue then
        None
      else begin
        let ev = Queue.pop t.queue in
        (match ev with
         | Resize -> t.resize_in_queue <- false
         | Resume -> t.resume_in_queue <- false
         | _           -> ());
        Some ev
      end
    ;;

    let clear t =
      Queue.clear t.queue;
      t.resume_in_queue <- false;
      t.resize_in_queue <- false;
    ;;

    let is_empty t = Queue.is_empty t.queue
  end

  type t =
    { pending_events     : Queue.t
    ; mutex              : Mutex.t
    ; mutable waiters    : Notifier.Request.t list
    ; mutable state      : State.t
    ; mutable got_reader : Condition.t
    } [@@deriving sexp_of]

  let create () =
    { pending_events = Queue.create ()
    ; mutex          = Mutex.create ()
    ; waiters        = []
    ; state          = Active
    ; got_reader     = Condition.create ()
    }
  ;;

  let notify_all_locked t =
    let waiters = t.waiters in
    t.waiters <- [];
    List.iter Notifier.Request.notify waiters
  ;;

  let send t ev =
    protect t.mutex ~f:(fun () ->
      match t.state with
      | Closed   -> assert false
      | Inactive -> Queue.enqueue t.pending_events ev
      | Active   ->
        Queue.enqueue t.pending_events ev;
        notify_all_locked t)
  ;;

  let send_many t evs =
    if evs <> [] then begin
      protect t.mutex ~f:(fun () ->
        match t.state with
        | Closed   -> assert false
        | Inactive -> Queue.enqueue_many t.pending_events evs
        | Active   ->
          Queue.enqueue_many t.pending_events evs;
          notify_all_locked t)
    end
  ;;

  let add_notify_locked t ~notifier =
    let result, request = Notifier.new_request notifier in
    t.waiters <- request :: t.waiters;
    Condition.broadcast t.got_reader;
    Pending result
  ;;

  let poll t ~notifier =
    protect t.mutex ~f:(fun () ->
      match t.state with
      | Closed   -> Ready LTerm_event.Closed
      | Inactive -> add_notify_locked t ~notifier
      | Active   ->
        match Queue.dequeue t.pending_events with
        | None -> add_notify_locked t ~notifier
        | Some ev -> Ready ev)
  ;;

  let wait_for_reader t =
    protect t.mutex ~f:(fun () ->
      match t.state with
      | Closed -> ()
      | Inactive | Active ->
        match t.waiters with
        | [] -> Condition.wait t.got_reader t.mutex
        | _  -> ())
  ;;

  let set_active t active =
    protect t.mutex ~f:(fun () ->
      match t.state, active with
      | Closed   , _     -> assert false
      | Active   , true  -> ()
      | Inactive , false -> ()
      | Active   , false -> t.state <- Inactive
      | Inactive , true  ->
        t.state <- Active;
        if not (Queue.is_empty t.pending_events) then
          notify_all_locked t)
  ;;

  let close t =
    protect t.mutex ~f:(fun () ->
      assert (not (State.is_closed t.state));
      t.state <- Closed;
      Queue.clear t.pending_events;
      notify_all_locked t)
  ;;
end

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

module Int_map = Map.Make(struct
    type t = int
    let compare (a : int) b = compare a b
  end)

type t =
  { mutable state              : State.t
  ; windows                    : bool
  ; model                      : string
  ; colors                     : int
  ; bold_is_bright             : bool
  ; color_map                  : LTerm_color_mappings.map
  ; fd_in                      : Unix.file_descr
  ; fd_out                     : Unix.file_descr
  ; out_buf_mutex              : Mutex.t (* For moving data in this buffer *)
  ; mutable out_buf            : Bigstring.t
  ; mutable out_pos_written    : int
  ; mutable out_pos            : int
  ; mutable size               : LTerm_geom.size
  ; event_parser               : LTerm_unix.Event_parser.t
  ; mutable syncs              : Sync.t Thread_safe_queue.t
  ; mutable syncing_up_to      : int
  ; mutable reading_event      : bool
  ; mutable initial_attr       : Unix.terminal_io option
  ; mutable attr_modified      : bool (* Whether the current term attributes are different
                                         from the initial ones *)
  ; mutable end_of_display     : int option
  ; mutable events             : Event_queue.t
  ; mutable real_mode          : Mode.t (* Real mode of the terminal                *)
  ; mutable mode_when_synced   : Mode.t (* Mode after all the current sync are done *)
  ; mutable mode               : Mode.t
  (* We handle cursor outside of [mode] as the position of the [show]/[hide] instructions
     are important for rendering, so it is not something that can be managed the same way
     as [mode] fields. *)
  ; mutable real_cursor        : Cursor.t
  ; mutable cursor_when_synced : Cursor.t
  ; mutable cursor             : Cursor.t
  ; closed                     : Notifier.Default.request
  }

let mode   t = t.mode
let size   t = t.size
let colors t = t.colors
let model  t = t.model

module Global = struct
  module State = struct
    type t =
      | Normal
      | Suspending
      | Exiting
  end

  let mutex = Mutex.create ()

  let state     = ref State.Normal
  let terminals = ref []

  (* Number of writer threads currently working *)
  let writers = ref 0
  (* Notified when [state] is [Suspending] or [Exiting] *)
  let writers_done = Condition.create ()
  (* Notified after a resume *)
  let resume = Condition.create ()

  let add_terminal t =
    protect mutex ~f:(fun () -> terminals := t :: !terminals)
  ;;

  let remove_terminal t =
    protect mutex ~f:(fun () ->
      terminals := List.filter !terminals ~f:(fun t' -> not (t == t')))
  ;;

  let wait_for_writers_holding_mutex () =
    if !writers <> 0 then Condition.wait writers_done mutex
  ;;

  type start_writing_result =
    | Exit_now
    | Do_write

  let start_writing =
    let rec loop () =
      match !state with
      | Exiting ->
        Exit_now
      | Normal ->
        incr writers;
        Do_write
      | Suspending ->
        Condition.wait resume mutex;
        loop ()
    in
    fun () -> protect mutex ~f:loop
  ;;

  let end_writing () =
    protect mutex ~f:(fun () ->
      assert (!writers > 0);
      decr writers;
      if !writers = 0 then Condition.signal writers_done)
  ;;
end

let add_event t ev =
  match t.state with
  | Closed -> ()
  | _      -> Event_queue.send t.events ev

let send_event t ev =
  match t.state with
  | Closed -> ()
  | _      -> Event_queue.send t.events ev

let send_events t evs =
  match t.state with
  | Closed -> ()
  | _      -> Event_queue.send_many t.events evs

let escape_time t = LTerm_unix.Event_parser.escape_time t.event_parser
let set_escape_time t span = LTerm_unix.Event_parser.set_escape_time t.event_parser span

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

module Signals = struct
  module State = struct
    type t =
      | Not_managed
      | Generate_event
      | Handled
    [@@deriving sexp]
  end

  type t =
    { intr : State.t
    ; quit : State.t
    ; susp : State.t
    }
  [@@deriving sexp]

  let t = ref { intr = Not_managed
              ; quit = Not_managed
              ; susp = Not_managed }

  let handled = { intr = Handled
                ; quit = Handled
                ; susp = Handled }

  let get () = !t

  type signal =
    | Intr
    | Quit
    | Susp
    | Winch

  let char_of_signal = function
    | Intr  -> 'i'
    | Quit  -> 'q'
    | Susp  -> 's'
    | Winch -> 'w'

  let signal_of_char = function
    | 'i' -> Intr
    | 'q' -> Quit
    | 's' -> Susp
    | 'w' -> Winch
    | _   -> assert false

  let signal_of_signo signo =
    if signo = Sys.sigint then
      Intr
    else if signo = Sys.sigquit then
      Quit
    else if signo = Sys.sigtstp then
      Susp
    else
      Winch
  ;;

  let state_of_signal = function
    | Intr -> !t.intr
    | Quit -> !t.quit
    | Susp -> !t.susp
    | Winch -> Generate_event
  ;;

  let send t s =
    try
      let i = ref 0 in
      let len = String.length s in
      while !i < len do
        let n =
          try
            Unix.write t.fd_out s !i (len - !i)
          with Unix.Unix_error (EINTR, _, _) ->
            0
        in
        i := !i + n
      done
    with _ -> ()
  ;;

  let reset t =
    (match t.initial_attr with
     | Some attr -> if t.attr_modified then Terminal_io.set_attr_no_err t.fd_in attr
     | None -> ());
    let cmd =
      let mode = t.real_mode in
      String.concat ""
        [ Mouse_events.state_change_sequence mode.mouse  Disabled
        ; Screen.      state_change_sequence mode.screen Main
        ; if t.real_cursor = Hidden then "\027[?25h" else ""
        ; match t.end_of_display with
        | None -> ""
        | Some ofs ->
          (* Move down, beginning of line *)
          Printf.sprintf "\027[%dB\r" ofs
        ]
    in
    if cmd <> "" then send t cmd
  ;;

  let abort () =
    let l = !Global.terminals in
    Global.terminals := [];
    List.iter
      (fun t ->
         match t.state with
         | Closed | Inactive -> ()
         | Active ->
           t.state <- Inactive;
           (try reset t with _ -> ()))
      l;
  ;;

  let () = at_exit abort

  external sys_exit : int -> unit = "caml_sys_exit"

  let clean_quit_by_signal () =
    protect Global.mutex ~f:(fun () ->
      Global.state := Exiting;
      Global.wait_for_writers_holding_mutex ();
      abort ();
      sys_exit 1)
  ;;

  let suspend () =
    protect Global.mutex ~f:(fun () ->
      Global.state := Suspending;
      Global.wait_for_writers_holding_mutex ();
      let terminals   = !Global.terminals in
      let saved_attrs = List.map terminals ~f:(fun t ->
        Terminal_io.get_attr_no_err t.fd_in)
      in
      List.iter terminals ~f:reset;

      let behavior = Sys.signal Sys.sigtstp Signal_default in
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      Sys.set_signal Sys.sigtstp behavior;

      List.iter2 terminals saved_attrs ~f:(fun t attr ->
        t.real_mode   <- Mode.erase_non_termios_fields t.real_mode;
        t.real_cursor <- Visible;
        (match attr with
         | None -> ()
         | Some attr -> Terminal_io.set_attr_no_err t.fd_in attr);
        add_event t Resume;
        (* Be sure that if the program does nothing to restore this terminal, we restore
           at least some basic stuff. *)
        let sync = Sync.refresh ~mode:t.mode_when_synced ~cursor:t.cursor_when_synced in
        Thread_safe_queue.send t.syncs sync;
      );
      List.iter (fun t -> add_event t Resume) !Global.terminals;
      Condition.broadcast Global.resume)
  ;;

  let broadcast_event event =
    protect Global.mutex ~f:(fun () ->
      List.iter (fun t -> add_event t event) !Global.terminals)
  ;;

  let broadcast_resize () =
    let send t =
      match get_size_from_fd t.fd_out with
      | size ->
        if size <> t.size then begin
          t.size <- size;
          add_event t Resize
        end
      | exception _ -> ()
    in
    protect Global.mutex ~f:(fun () ->
      List.iter send !Global.terminals)
  ;;

  (* Called by the signal manager thread *)
  let process_signal signal =
    match state_of_signal signal with
    | Not_managed -> ()
    | Generate_event ->
      (match signal with
       | Intr -> broadcast_event (Signal Intr)
       | Quit -> broadcast_event (Signal Quit)
       | Susp -> broadcast_event (Signal Susp)
       | Winch -> broadcast_resize ())
    | Handled ->
      (match signal with
       | Intr | Quit -> clean_quit_by_signal ()
       | Susp -> suspend ()
       | Winch -> assert false)
  ;;

  external init_signal_manager_thread : unit -> unit = "lt_term_init_signal_manager_thread"
  external ocaml_signal_of_signo : int -> int = "lt_term_ocaml_signal_of_signo"

  let all_sigs =
    let sigs = Sys.[sigint; sigquit; sigtstp] in
    match LTerm_unix.sigwinch with
    | Some n -> n :: sigs
    | None   -> sigs
  ;;

  let signal_manager_loop () =
    init_signal_manager_thread ();
    ignore (Thread.sigmask SIG_BLOCK all_sigs : int list);
    while true do
      let signo = Thread.wait_signal all_sigs |> ocaml_signal_of_signo in
      process_signal (signal_of_signo signo)
    done
  ;;

  let sigwinch_emul () =
    while true do
      (try Thread.delay 0.5 with Unix.Unix_error (EINTR, _, _) ->  ());
      process_signal Winch
    done;
  ;;

  let prev_intr = ref Sys.Signal_default
  let prev_quit = ref Sys.Signal_default
  let prev_susp = ref Sys.Signal_default

  external set : int -> unit = "lt_term_set_signal"

  let init = lazy(
    ignore (Thread.create signal_manager_loop () : Thread.t);
    match LTerm_unix.sigwinch with
    | Some n -> set n
    | None   -> ignore (Thread.create sigwinch_emul () : Thread.t);
  )

  let set_one prev signo (old_state:State.t) (new_state:State.t) =
    Lazy.force init;
    match old_state, new_state with
    | Not_managed, Not_managed ->
      ()
    | _, Not_managed ->
      Sys.set_signal signo !prev;
      prev := Signal_default
    | Not_managed, _ ->
      set signo
    | _ ->
      ()
  ;;

  let set new_t =
    let old_t = !t in
    set_one prev_intr Sys.sigint  old_t.intr new_t.intr;
    set_one prev_quit Sys.sigquit old_t.quit new_t.quit;
    set_one prev_susp Sys.sigtstp old_t.susp new_t.susp;
    t := new_t;
  ;;
end

let abort = Signals.abort

(* +-----------------------------------------------------------------+
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

module Event_reader = struct
  let rec loop t =
    (* Do not read stdin eagerly, wait for someone to request it *)
    Event_queue.wait_for_reader t.events;
    let evs = LTerm_unix.Event_parser.read t.event_parser in
    Event_queue.send_many t.events evs;
    loop t
end

let poll_event t ~notifier =
  Event_queue.poll t.events ~notifier
;;

let rec read_event_sync t =
  match poll_event t ~notifier:Notifier.Default.t with
  | Ready   ev  -> ev
  | Pending req -> Notifier.Default.wait req; read_event_sync t
;;

(* +-----------------------------------------------------------------+
   | String recoding                                                 |
   +-----------------------------------------------------------------+ *)

(* Generated by gen/gen_pieces.ml *)
let ucode_to_ascii_approx = "--||........++++++++++++++++++++\
                             ++++++++++++++++++++++++++++++++\
                             ++++++++++++....-|++++++++++++++\
                             +++++++++++++.......++++++++-|-|"

(* Map characters that cannot be encoded to ASCII ones. *)
let ascii_approx char =
  let c = Uchar.to_int char in
  if c >= 0x2500 && c <= 0x257f then
    match ucode_to_ascii_approx.[c - 0x2500] with
    | '.' -> char
    | ch  -> Uchar.of_char ch
  else
    char
;;

let map_char =
  if LTerm_unix.system_encoding = "UTF-8" then (fun x -> x) else ascii_approx
;;

(* +-----------------------------------------------------------------+
   | Output buffer                                                   |
   +-----------------------------------------------------------------+ *)

let grow_size n =
  let n = n * 2 in
  if n < 0 then
    max_int
  else begin
    let res = ref 1 in
    while !res < n && !res > 0 do res := !res lsl 1 done;
    if !res <= 0 then
      max_int
    else
      !res
  end
;;

let grow_out_buf t len =
  let _f () = () in (* prevents inlining *)
  if State.is_closed t.state then failwith "LTerm: closed terminal";
  protect t.out_buf_mutex ~f:(fun () ->
    if len - t.out_pos_written <= Bigstring.length t.out_buf then begin
      let n = t.out_pos_written in
      Bigstring.blit
        ~src:t.out_buf ~dst:t.out_buf
        ~src_pos:n ~dst_pos:0 ~len:(t.out_pos - n);
      t.out_pos_written <- 0;
      t.out_pos         <- t.out_pos - n;
      t.syncing_up_to   <- t.syncing_up_to - n;
    end else begin
      let new_buf = Bigstring.create (grow_size len) in
      let n = t.out_pos_written in
      Bigstring.blit
        ~src:t.out_buf ~dst:new_buf
        ~src_pos:n ~dst_pos:0 ~len:(t.out_pos - n);
      t.out_pos_written <- 0;
      t.out_pos         <- t.out_pos - n;
      t.syncing_up_to   <- t.syncing_up_to - n;
      t.out_buf         <- new_buf
    end)
;;

let reserve t n =
  let len = t.out_pos + n in
  if len > Bigstring.length t.out_buf then grow_out_buf t len;
;;

let add_char t c =
  reserve t 1;
  let pos = t.out_pos in
  t.out_buf.{pos} <- c;
  t.out_pos <- pos + 1;
;;

let add_2chars t a b =
  reserve t 2;
  let buf = t.out_buf in
  let pos = t.out_pos in
  buf.{pos    } <- a;
  buf.{pos + 1} <- b;
  t.out_pos <- pos + 2;
;;

let add_3chars t a b c =
  reserve t 3;
  let buf = t.out_buf in
  let pos = t.out_pos in
  buf.{pos    } <- a;
  buf.{pos + 1} <- b;
  buf.{pos + 2} <- c;
  t.out_pos <- pos + 3;
;;

let add_substring t s ~pos ~len =
  if len > 0 then begin
    reserve t len;
    let out_pos = t.out_pos in
    Bigstring.blit_string ~src:s ~dst:t.out_buf ~src_pos:pos ~dst_pos:out_pos ~len;
    t.out_pos <- out_pos + len;
  end
;;

let add_string t s =
  add_substring t s ~pos:0 ~len:(String.length s)

let digit n = Char.chr (Char.code '0' + n)

let add_int t n =
  if n < 0 || n > 999 then
    add_string t (string_of_int n)
  else begin
    (* Fast path for the common case *)
    if n < 10 then
      add_char t (digit n)
    else if n < 100 then
      let i = n / 10 in
      add_2chars t
        (digit i)
        (digit (n - i * 10))
    else
      let i = n /     100 in
      let j = n - i * 100 in
      let k = j /      10 in
      add_3chars t
        (digit i)
        (digit k)
        (digit (j - k * 10))
  end;
;;

(* +-----------------------------------------------------------------+
   | State                                                           |
   +-----------------------------------------------------------------+ *)

let show_cursor t = t.cursor <- Visible; add_string t "\027[?25h"
let hide_cursor t = t.cursor <- Hidden ; add_string t "\027[?25l"

let cursor_visible t =
  match t.cursor with
  | Visible -> true
  | Hidden  -> false
;;

let set_mode t (mode : Mode.t) =
  add_string t
    (Mouse_events.state_change_sequence t.mode.mouse mode.mouse);
  add_string t
    (Screen.state_change_sequence t.mode.screen mode.screen);
  t.mode <- mode
;;

let modify_mode ?echo ?raw ?signals ?mouse ?screen t =
  set_mode t (Mode.set (mode t) ?echo ?raw ?signals ?mouse ?screen)
;;

let reset t =
  set_mode t Mode.default;
  if not (cursor_visible t) then show_cursor t;
;;

let add_move t n c =
  add_2chars t '\027' '[';
  add_int t n;
  add_char t c
;;

let goto t (coord : LTerm_geom.coord) =
  add_string t "\027[H";
  if coord.row > 0 then add_move t coord.row 'B';
  if coord.col > 0 then add_move t coord.col 'C';
;;

let move t ~rows ~cols =
  (match rows with
   | n when n < 0 -> add_move t (-n) 'A'
   | n when n > 0 -> add_move t   n  'B'
   | _ -> ());
  (match cols with
   | n when n < 0 -> add_move t (-n) 'D'
   | n when n > 0 -> add_move t   n  'C'
   | _ -> ());
;;

let clear_screen t = add_string t "\027[2J"

let clear_screen_next t = add_string t "\027[J"
let clear_screen_prev t = add_string t "\027[1J"

let clear_line t = add_string t "\027[2K"

let clear_line_next t = add_string t "\027[K"
let clear_line_prev t = add_string t "\027[1K"

let print = add_string
let print_sub = add_substring

(* +-----------------------------------------------------------------+
   | Rendering                                                       |
   +-----------------------------------------------------------------+ *)

module Codes = struct
  let reset      = '0'
  let bold       = '1'
  let underline  = '4'
  let blink      = '5'
  let reverse    = '7'
  let foreground = 30
  let background = 40
end

let add_index t base n =
  if n < 8 then begin
    add_char t ';';
    add_int t (base + n)
  end else if n < 16 &&  t.bold_is_bright then
    if base = Codes.foreground then begin
      add_3chars t ';' Codes.bold ';';
      add_int t (base + n - 8)
    end else begin
      add_char t ';';
      add_int t (base + n - 8)
    end
  else begin
    add_char t ';';
    add_int t (base + 8);
    add_3chars t ';' '5' ';';
    add_int t n
  end
;;

let add_color t base col =
  match LTerm_style.Color.kind col with
  | Transparent | Default -> ()
  | Index | RGB -> add_index t base (LTerm_style.Color.get_index col t.color_map)
;;

let is_on : LTerm_style.Switch.t -> bool = function
  | On -> true
  | Off | Unset -> false
;;

let add_style t ~style =
  add_3chars t '\027' '[' Codes.reset;
  if is_on (LTerm_style.bold      style) then add_2chars t ';' Codes.bold;
  if is_on (LTerm_style.underline style) then add_2chars t ';' Codes.underline;
  if is_on (LTerm_style.blink     style) then add_2chars t ';' Codes.blink;
  if is_on (LTerm_style.reverse   style) then add_2chars t ';' Codes.reverse;
  add_color t Codes.foreground (LTerm_style.foreground style);
  add_color t Codes.background (LTerm_style.background style);
  add_char t 'm'
;;

let set_style t style = add_style t ~style

let add_uchar t c =
  let code = Uchar.to_int c in
  if code < 128 then
    add_char t (Char.chr code)
  else begin
    reserve t 4;
    t.out_pos <- Zed_bigarray.encode t.out_buf c ~pos:t.out_pos
  end
;;

let unknown_char = Uchar.of_int 0xfffd

let render_char t ~char =
  (* Skip control characters, otherwise output will be messy. *)
  if Uchar.to_int char < 32 then
    add_uchar t unknown_char
  else
    add_uchar t (map_char char)
;;

let nothing_from line mark =
  let rec nothing_until_eol (line:LTerm_matrix_private.point array) x =
    if x = Array.length line then
      true
    else
      let c = Uchar.to_int line.(x).char in
      c = 10 || (c = 32 && nothing_until_eol line (x + 1))
  in
  let rec nothing_before (line:LTerm_matrix_private.point array) x mark =
    if x = mark then
      nothing_until_eol line x
    else
      let c = Uchar.to_int line.(x).char in
      c = 10 || (c = 32 && nothing_before line (x + 1) mark)
  in
  nothing_before line 0 mark
;;

let cast_matrix m = (m : LTerm_draw.Matrix.t :> LTerm_matrix_private.t)

let empty_matrix = LTerm_draw.Matrix.create { cols = 0; rows = 0 }

type raw_data = LTerm_matrix_private.point array array

let render_loop t (old_data:raw_data) (new_data:raw_data) =
  (* The last displayed style. *)
  let curr_style = ref LTerm_style.default in
  for y = 0 to Array.length new_data - 1 do
    let line = new_data.(y)          in
    let cols = Array.length line - 1 in
    (* If the current line is equal to the displayed one, skip it *)
    let same_as_before = y < Array.length old_data && line = old_data.(y) in
    let x = ref 0 in
    let continue = ref true in
    while !x < cols && !continue do
      let point = line.(!x) in
      let style = LTerm_style.on_default point.style in
      if not (LTerm_style.equal !curr_style style) then begin
        curr_style := style;
        add_style t ~style;
      end;
      let code = Uchar.to_int point.char in
      if code = 10 then begin
        (* Erase everything until the end of line, if needed. *)
        if not (y < Array.length old_data && nothing_from old_data.(y) !x) then
          add_3chars t '\027' '[' 'K';
        continue := false;
      end else begin
        render_char t ~char:point.char;
        incr x;
      end;
      if same_as_before then continue := false;
    done;
    if same_as_before then
      move t ~rows:1 ~cols:(-1)
    else begin
      let point = line.(!x) in
      if Uchar.to_int point.char = 10 && y < Array.length new_data - 1 then
        add_char t '\n';
    end
  done
;;

let render_gen t ?(old=empty_matrix) matrix =
  let matrix = cast_matrix matrix in
  let old    = cast_matrix old    in
  let old =
    if matrix.size <> old.size then
      (* No update if dimmensions changed *)
      cast_matrix empty_matrix
    else
      old
  in
  render_loop t old.data matrix.data
;;

let render t ?old matrix =
  add_string t "\027[H\027[0m"; (* Go the top-left and reset attributes *)
  render_gen t ?old matrix;
  add_string t "\027[0m"
;;

let print_box t ?old matrix =
  if (LTerm_draw.Matrix.size matrix).rows > 0 then begin
    add_string t "\r\027[0m";
    render_gen t ?old matrix;
    add_string t "\027[0m\r"
  end else
    add_char t '\r'
;;

(* +-----------------------------------------------------------------+
   | Writing                                                         |
   +-----------------------------------------------------------------+ *)

(* Compare attributes that we care about *)
let attr_equal (a : Unix.terminal_io) (b : Unix.terminal_io) =
  a.c_echo   = b.c_echo   &&
  a.c_isig   = b.c_isig   &&
  a.c_brkint = b.c_brkint &&
  a.c_inpck  = b.c_inpck  &&
  a.c_istrip = b.c_istrip &&
  a.c_ixon   = b.c_ixon   &&
  a.c_inlcr  = b.c_inlcr  &&
  a.c_icrnl  = b.c_icrnl  &&
  a.c_csize  = b.c_csize  &&
  a.c_parenb = b.c_parenb &&
  a.c_icanon = b.c_icanon &&
  a.c_vmin   = b.c_vmin   &&
  a.c_vtime  = b.c_vtime
;;

let rec really_write fd ~buf ~pos ~len =
  if len > 0 then
    match Bigstring.write fd ~buf ~pos ~len with
    | n -> really_write fd ~buf ~pos:(pos + n) ~len:(len - n)
    | exception Unix.Unix_error(EINTR, _, _) ->
      really_write fd ~buf ~pos ~len

let do_sync t (sync : Sync.t) =
  let real_mode = t.real_mode in
  let new_mode  = sync.mode   in
  let new_mode_termios_only =
    Mode.copy_non_termios_fields new_mode ~from:t.real_mode
  in
  if new_mode_termios_only <> real_mode then begin
    let attr = Terminal_io.get_attr t.fd_in in
    let init_attr =
      match t.initial_attr with
      | Some x -> x
      | None ->
        let copy = { attr with c_isig = attr.c_isig } in
        t.initial_attr <- Some copy;
        copy
    in
    attr.c_echo <- new_mode_termios_only.echo;
    attr.c_isig <- new_mode_termios_only.signals;
    if new_mode_termios_only.raw then begin
      (* Inspired from Python-3.0/Lib/tty.py: *)
      attr.c_brkint <- false;
      attr.c_inpck  <- false;
      attr.c_istrip <- false;
      attr.c_ixon   <- false;
      attr.c_inlcr  <- false;
      attr.c_icrnl  <- false;
      attr.c_csize  <- 8;
      attr.c_parenb <- false;
      attr.c_icanon <- false;
      attr.c_vmin   <- 1;
      attr.c_vtime  <- 0;
    end else begin
      (* We assume that initially the terminal is not in raw mode *)
      attr.c_brkint <- init_attr.c_brkint;
      attr.c_inpck  <- init_attr.c_inpck;
      attr.c_istrip <- init_attr.c_istrip;
      attr.c_ixon   <- init_attr.c_ixon;
      attr.c_inlcr  <- init_attr.c_inlcr;
      attr.c_icrnl  <- init_attr.c_icrnl;
      attr.c_csize  <- init_attr.c_csize;
      attr.c_parenb <- init_attr.c_parenb;
      attr.c_icanon <- init_attr.c_icanon;
      attr.c_vmin   <- init_attr.c_vmin;
      attr.c_vtime  <- init_attr.c_vtime;
    end;
    Terminal_io.set_attr t.fd_in attr;
    t.real_mode     <- new_mode_termios_only;
    t.attr_modified <- not (attr_equal attr init_attr);
  end;
  let cursor = t.cursor in
  protect t.out_buf_mutex ~f:(fun () ->
    really_write t.fd_out ~buf:t.out_buf ~pos:t.out_pos_written ~len:sync.len;
    t.out_pos_written <- t.out_pos_written + sync.len);
  t.real_mode      <- new_mode;
  t.real_cursor    <- cursor;
  (match sync.end_of_display with
   | Do_not_change -> ()
   | Set x -> t.end_of_display <- x);
  Notifier.Request.notify sync.request
;;

let rec sync_forever t =
  let sync = Thread_safe_queue.wait t.syncs in
  match Global.start_writing () with
  | Exit_now -> ()
  | Do_write ->
    match do_sync t sync with
    | ()          -> Global.end_writing (); sync_forever t
    | exception e -> Global.end_writing (); raise e
;;

let commit ?end_of_display ~notifier t =
  match t.state with
  | Closed -> failwith "LTerm.sync called on closed terminal"
  | Active | Inactive ->
    protect t.out_buf_mutex ~f:(fun () ->
      let mode         = t.mode                        in
      let cursor       = t.cursor                      in
      let current_pos  = t.out_pos                     in
      let len          = current_pos - t.syncing_up_to in
      let res, request = Notifier.new_request notifier in
      let sync : Sync.t =
        { len
        ; end_of_display = Set end_of_display
        ; mode
        ; cursor
        ; request
        }
      in
      t.  mode_when_synced <- mode;
      t.cursor_when_synced <- cursor;
      t.syncing_up_to      <- current_pos;
      Thread_safe_queue.send t.syncs sync;
      res)
;;

let commit_sync ?end_of_display t =
  commit t ?end_of_display ~notifier:Notifier.Default.t
  |> Notifier.Default.wait
;;

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let default_model =
  try
    Sys.getenv "TERM"
  with Not_found ->
    "dumb"

let colors_of_term = function
  | "Eterm-256color"        -> 256
  | "Eterm-88color"         ->  88
  | "gnome-256color"        -> 256
  | "iTerm.app"             -> 256
  | "konsole-256color"      -> 256
  | "mlterm-256color"       -> 256
  | "mrxvt-256color"        -> 256
  | "putty-256color"        -> 256
  | "rxvt-256color"         -> 256
  | "rxvt-88color"          ->  88
  | "rxvt-unicode-256color" -> 256
  | "rxvt-unicode"          ->  88
  | "screen-256color"       -> 256
  | "screen-256color-bce"   -> 256
  | "screen-256color-bce-s" -> 256
  | "screen-256color-s"     -> 256
  | "st-256color"           -> 256
  | "vte-256color"          -> 256
  | "xterm-256color"        -> 256
  | "xterm+256color"        -> 256
  | "xterm-88color"         ->  88
  | "xterm+88color"         ->  88
  | _                       ->  16

let bold_is_bright = function
  | "linux"       (* The linux frame buffer *)
  | "xterm-color" (* The MacOS-X terminal   *) ->
    true
  | _ ->
    false

let check_utf8 = lazy(
  match LTerm_unix.system_encoding with
  | "UTF-8" -> ()
  | _ ->
    Printf.eprintf
      "Warning: the detected system character encoding is not UTF-8.\n\
       Lambda term work best when it is UTF-8.\n%!"
)
;;

let create ?(windows=Sys.win32) ?(model=default_model) (fd_in, fd_out) =
  Lazy.force Signals.init;
  Lazy.force check_utf8;
  if Unix.isatty fd_in && (fd_in = fd_out || Unix.isatty fd_out) then begin
    (* Colors stuff. *)
    let colors = colors_of_term model in
    let color_map =
      match colors with
      | 16  -> LTerm_color_mappings.colors_16
      | 88  -> LTerm_color_mappings.colors_88
      | 256 -> LTerm_color_mappings.colors_256
      | _   -> assert false
    in
    let size = get_size_from_fd fd_out in
    let t =
      { state = Active
      ; windows
      ; model
      ; colors
      ; bold_is_bright = bold_is_bright model
      ; color_map
      ; fd_in
      ; fd_out
      ; out_buf = Bigstring.create 16384
      ; out_buf_mutex = Mutex.create ()
      ; out_pos = 0
      ; out_pos_written = 0
      ; size
      ; event_parser = LTerm_unix.Event_parser.create ~escape_time:0.1 fd_in
      ; syncs = Thread_safe_queue.create ()
      ; syncing_up_to = 0
      ; reading_event = false
      ; end_of_display = None
      ; initial_attr = None
      ; attr_modified = false
      ; events = Event_queue.create ()
      ; real_mode = Mode.default
      ; mode_when_synced = Mode.default
      ; mode = Mode.default
      ; real_cursor = Visible
      ; cursor_when_synced = Visible
      ; cursor = Visible
      ; closed = Notifier.Default.new_request ()
      }
    in
    Gc.finalise Notifier.Default.free t.closed;
    Global.add_terminal t;
    ignore (Thread.create sync_forever t : Thread.t);
    ignore (Thread.create Event_reader.loop t : Thread.t);
    t
  end else
    failwith "LTerm.create: input fd is not a TTY device"
;;

let dummy_bstr = Bigstring.create 0

let really_close t =
  reset t;
  let req = commit t ~notifier:Notifier.Default.t in
  t.state <- Closed;
  Event_queue.close t.events;
  LTerm_unix.Event_parser.set_active t.event_parser false;
  Notifier.Default.wait req;
  (* We remove the terminal from the list only at the end so that Ctrl+C still does the
     right thing until then. *)
  Global.remove_terminal t;
  (* Free resources now *)
  t.out_pos <- 0;
  t.out_buf <- dummy_bstr;
;;

let close t =
  if not (State.is_closed t.state) then
    really_close t
  else
    Notifier.Default.wait_without_freeing t.closed
;;

let set_active t active =
  match t.state, active with
  | Closed   ,  _    -> failwith "LTerm.set_active called on closed terminal"
  | Active   , true  -> ()
  | Inactive , false -> ()
  | Active   , false ->
    t.state <- Inactive;
    Global.remove_terminal t;
    Event_queue.set_active t.events false;
    LTerm_unix.Event_parser.set_active t.event_parser false;
  | Inactive , true  ->
    t.state <- Active;
    Global.add_terminal t;
    LTerm_unix.Event_parser.set_active t.event_parser true;
    Event_queue.set_active t.events true;
    let size = get_size_from_fd t.fd_out in
    if size <> t.size then begin
      add_event t Resize;
      t.size <- size;
    end;
;;

(* +-----------------------------------------------------------------+
   | Standard terminal                                               |
   +-----------------------------------------------------------------+ *)

let std = lazy(
  if Sys.win32 then
    create (Unix.stdin, Unix.stdout)
  else
    create (Unix.stdin, Unix.stdin)
)
