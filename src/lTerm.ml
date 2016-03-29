(*
 * lTerm.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

module Screen = struct
  type t = Main | Alternative
end

module Mode = struct
  type t =
    { echo    : bool
    ; raw     : bool
    ; signals : bool
    ; mouse   : bool
    ; screen  : Screen.t
    }

  let default =
    { echo    = true
    ; raw     = false
    ; signals = true
    ; mouse   = false
    ; screen  = Main
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

module Sync = struct
  type end_of_display = Do_not_change | Set of int option

  type t =
    { len            : int
    ; end_of_display : end_of_display
    ; mode           : Mode.t
    ; cursor         : Cursor.t
    ; callback       : unit -> unit
    }
  [@@deriving sexp_of]

  let refresh ~mode ~cursor =
    { len = 0
    ; end_of_display = `Do_not_change
    ; mode
    ; cursor
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

let protect mutex ~f =
  Mutex.lock t.mutex;
  match f () with
  | x           -> Mutex.unlock mutex; x
  | exception e -> Mutex.unlock mutex; raise e

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

  let pop t = Queue.dequeue t.data

  let wait t =
    protect t.mutex ~f:(fun () ->
      if Queue.is_empty t.queue then begin
        t.waiters <- t.waiters + 1;
        Condition.wait t.mutex t.wait;
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

module Event_queue : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  val send       : t -> LTerm_event.t -> unit
  val wait_async : t -> f:(LTerm_event.t -> unit)

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
    val dequeue : t -> LTerm_event.t option
    val clear   : t -> unit
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
          Queue.enqueue t.queue ev
        end;
      | Resume ->
        if not t.resume_in_queue then begin
          t.resume_in_queue <- true;
          Queue.enqueue t.queue ev
        end
      | _ ->
        Queue.enqueue t.queue ev
    ;;

    let dequeue t =
      let ev = Queue.dequeue t.queue in
      (match ev with
       | Some Resize -> t.resize_in_queue <- false
       | Some Resume -> t.resume_in_queue <- false
       | _           -> ());
      ev
    ;;

    let clear t =
      Queue.clear t.queue;
      t.resume_in_queue <- false;
      t.resize_in_queue <- false;
    ;;
  end

  type t =
    { pending_events : Queue.t
    ; mutex          : Mutex.t
    ; waiters        : (LTerm_event.t -> unit) Queue.t
    ; mutable state  : State.t
    } [@@deriving sexp_of]

  let create () =
    { pending_events = Queue.create ()
    ; mutex          = Mutex.create ()
    ; waiters        = Queue.create ()
    ; state          = Active
    }
  ;;

  let send t ev =
    protect t.mutex ~f:(fun () ->
      Mutex.lock t.mutex;
      (match t.state with
       | Closed   -> assert false
       | Inactive -> Queue.enqueue t.pending_events ev
       | Active   ->
         if not (Queue.is_empty t.waiters) then
           Queue.pop t.waiters ev
         else
           Queue.enqueue t.pending_events ev))
  ;;

  let really_wait t =
    let ivar = Ivar.create () in
    t.waiter <- Some ivar;
    Ivar.read ivar
  ;;

  let wait_async t ~f =
    protect t.mutex ~f:(fun () ->
      (match t.state with
       | Closed   -> assert false
       | Inactive -> Queue.push f t.waiters
       | Active   ->
         if not (Queue.is_empty t.pending_events) then
           f (Queue.pop t.pending_events)
         else
           Queue.enqueue t.waiters f))
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
        while not (Queue.is_empty t.waiters) && not (Queue.is_empty t.pending_events) do
          let f = Queue.pop t.waiters        in
          let x = Queue.pop t.pending_events in
          f x
        done)
  ;;

  let close t =
    protect t.mutex ~f:(fun () ->
      assert (not (State.is_closed t.state));
      t.state <- Closed;
      Queue.clear t.pending_events;
      let waiters = Queue.fold (fun l x -> x :: l) [] t.waiters |> List.rev in
      Queue.clear t.waiters;
      List.iter (fun f -> f Closed) waiters)
  ;;
end

(* +-----------------------------------------------------------------+
   | The terminal type                                               |
   +-----------------------------------------------------------------+ *)

type t =
  { mutable state              : State.t
  ; windows                    : bool
  ; model                      : string
  ; colors                     : int
  ; bold_is_bright             : bool
  ; color_map                  : LTerm_color_mappings.map sexp_opaque
  ; fd                         : Fd.t
  ; mutable out_buf            : Bigstring.t sexp_opaque
  ; mutable out_pos            : int
  ; mutable size               : LTerm_geom.size
  ; event_parser               : LTerm_unix.Event_parser.t
  ; mutable sync_mutex         : Mutex.t
  ; mutable syncing_up_to      : int
  ; mutable reading_event      : bool
  ; mutable initial_attr       : Unix.Terminal_io.t sexp_opaque option
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
  }
[@@deriving fields, sexp_of]

let suspending = ref None
let terminals  = ref []

let add_event t ev =
  match t.state with
  | Closed -> ()
  | _      -> Event_queue.send t.events ev

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

  (* We do not use async in these functions. Intr and Quit are for emergency so it doesn't
     really matters and even for Ctrl+Z, using async seems to create some visual delay. *)

  let get_attr t =
    try
      Some (Terminal_io.get_attr (Fd.file_descr_exn t.fd))
    with _ ->
      None
  ;;

  let set_attr t attr =
    try
      Terminal_io.set_attr (Fd.file_descr_exn t.fd) attr
    with _ ->
      ()
  ;;

  let send t s =
    try
      let i = ref 0 in
      let len = String.length s in
      while !i < len do
        let n =
          try
            Unix.write (Fd.file_descr_exn t.fd) s !i (len - !i)
          with Unix.Unix_error (EINTR, _, _) ->
            ()
        in
        i := !i + n
      done
    with _ -> ()
  ;;

  let reset t =
    (match t.initial_attr with
     | Some attr -> if t.attr_modified then set_attr t attr
     | None -> ());
    let cmd =
      let mode = t.real_mode in
      String.concat ~sep:""
        [ if mode.mouse                then "\027[?1000l" else ""
        ; if mode.screen = Alternative then "\027[?1049l" else ""
        ; if t.real_cursor = Hidden    then "\027[?25h"   else ""
        ; match t.end_of_display with
          | None -> ""
          | Some ofs ->
            (* Move down, beginning of line *)
            Printf.sprintf "\027[%dB\r" ofs
        ]
    in
    if cmd <> "" then send t cmd
  ;;

  let rec abort () =
    match !terminals with
    | [] -> ()
    | t :: l ->
      match t.state with
      | Closed | Inactive -> ()
      | Active ->
        t.state <- Inactive;
        terminals := l;
        (try reset t with _ -> ());
        abort ()
  ;;

  let () = Caml.at_exit abort

  let got_intr = ref false
  let got_quit = ref false
  let got_susp = ref false

  (* No way it's safe to call at_exit handler from a signal handler. It's supposed to be a
     dirty exit anyway. *)
  external sys_exit : int -> unit = "caml_sys_exit"

  let notify_async () =
    (* SIGWINCH is managed by async *)
    Option.iter LTerm_unix.sigwinch ~f:(fun signum ->
      Signal.send_i signum (`Pid (Unix.getpid ())))
  ;;

  let handler signo =
    let got, state =
      if signo = Signal.int then
        got_intr, !t.intr
      else if signo = Signal.quit then
        got_quit, !t.quit
      else if signo = Signal.tstp then
        got_susp, !t.susp
      else
        assert false
    in
    match state with
    | Not_managed -> ()
    | Generate_event -> got := true; notify_async ()
    | Handled ->
      if signo = Signal.tstp then
        (* This one is just impossible to get right in a concurrent context. If the
           program is stuck, Ctrl+Z will just not work. *)
        (got := true; notify_async ())
      else begin
        abort ();
        sys_exit 1
      end
  ;;

  let prev_intr = ref `Default
  let prev_quit = ref `Default
  let prev_susp = ref `Default

  let set_one prev signo (old_state:State.t) (new_state:State.t) =
    match old_state, new_state with
    | Not_managed, Not_managed ->
      ()
    | _, Not_managed ->
      Core.Std.Signal.Expert.set signo !prev;
      prev := `Default;
    | Not_managed, _ ->
      prev := Core.Std.Signal.Expert.signal signo (`Handle handler)
    | _ ->
      ()
  ;;

  let set new_t =
    let old_t = !t in
    set_one prev_intr Signal.int  old_t.intr new_t.intr;
    set_one prev_quit Signal.quit old_t.quit new_t.quit;
    set_one prev_susp Signal.tstp old_t.susp new_t.susp;
    t := new_t;
  ;;

  let got_resume = ref false

  let rec suspend () =
    match !suspending with
    | Some ivar -> Ivar.read ivar >>> suspend
    | None ->
      let resume_ivar = Ivar.create () in
      suspending := Some resume_ivar;
      let terminals = !terminals in
      (* Wait for everybody to finish drawing *)
      Deferred.all (List.map terminals ~f:current_write)
      >>> fun (_ : write_result list) ->
      let saved_attrs = List.map terminals ~f:get_attr in
      List.iter terminals ~f:reset;

      let behavior = Signal.Expert.signal Signal.tstp `Default in
      Signal.send_i Signal.tstp (`Pid (Unix.getpid ()));
      Signal.Expert.set Signal.tstp behavior;

      List.iter2_exn terminals saved_attrs ~f:(fun t attr ->
        t.real_mode   <- Mode.erase_non_termios_fields t.real_mode;
        t.real_cursor <- Visible;
        Option.iter attr ~f:(set_attr t));

      suspending := None;
      Ivar.fill resume_ivar ();
      got_resume := true;
      notify_async ();
  ;;
end

let abort = Signals.abort

let handle_signals t =
  match t.state with
  | Closed | Inactive -> ()
  | Active ->
    if !Signals.got_resume then begin
      add_event t Resume;
      (* Be sure that if the program does nothing to restore this terminal, we restore
         at least some basic stuff. *)
      Deferred.unit
      >>> fun () ->
      let sync = Sync.refresh ~mode:t.mode_when_synced ~cursor:t.cursor_when_synced in
      Thread_safe_queue.send t.syncs sync;
    end;
    if !Signals.got_intr then add_event t (Signal Intr);
    if !Signals.got_quit then add_event t (Signal Quit);
    if !Signals.got_susp then begin
      match !Signals.t.susp with
      | Not_managed | Handled -> ()
      | Generate_event -> add_event t (Signal Susp);
    end;
    match get_size_from_fd (Fd.file_descr_exn t.fd) with
    | size ->
      if size <> t.size then begin
        t.size <- size;
        add_event t Resize;
      end
    | exception _ -> ()
;;

let () =
  let check () =
    if !Signals.got_susp then begin
      match !Signals.t.susp with
      | Not_managed | Generate_event -> ()
      | Handled -> Signals.suspend ()
    end;
    List.iter !terminals ~f:handle_signals;
    Signals.got_resume := false;
    Signals.got_intr := false;
    Signals.got_quit := false;
    Signals.got_susp := false;
  in
  Deferred.unit
  >>> fun () ->
  match LTerm_unix.sigwinch with
  | None ->
    Clock.every (sec 1.0) check
  | Some signum ->
    Signal.handle [signum] ~f:(fun _ -> check ())
;;

(* +-----------------------------------------------------------------+
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

let read_event ?(no_text=false) t : LTerm_event.t Deferred.t =
  if State.is_closed t.state then
    failwiths "Term.read_event called on closed terminal" t sexp_of_t;
  match Event_queue.pop t.events with
  | Some ev -> return ev
  | None ->
    if t.reading_event then
      Event_queue.wait t.events
    else begin
      let next_event = LTerm_unix.Event_parser.read ~no_text t.event_parser in
      if Deferred.is_determined next_event then
        next_event
      else begin
        t.reading_event <- true;
        upon next_event (fun ev ->
          t.reading_event <- false;
          add_event t ev);
        Event_queue.wait t.events
      end
    end
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
  let c = Uchar.code char in
  if c >= 0x2500 && c <= 0x257f then
    match ucode_to_ascii_approx.[c - 0x2500] with
    | '.' -> char
    | ch  -> Uchar.of_char ch
  else
    char
;;

let map_char =
  if LTerm_unix.system_encoding = "UTF-8" then Fn.id else ascii_approx
;;

(* +-----------------------------------------------------------------+
   | Output buffer                                                   |
   +-----------------------------------------------------------------+ *)

let grow_out_buf t len =
  let _f () = () in (* prevents inlining *)
  if State.is_closed t.state then failwiths "closed terminal" t sexp_of_t;
  let new_buf = Bigstring.create (Int.ceil_pow2 (len * 2)) in
  Bigstring.blit ~src:t.out_buf ~dst:new_buf ~src_pos:0 ~dst_pos:0 ~len:t.out_pos;
  t.out_buf <- new_buf;
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
  reserve t len;
  let out_pos = t.out_pos in
  Bigstring.From_string.blit ~src:s ~dst:t.out_buf ~src_pos:pos ~dst_pos:out_pos ~len;
  t.out_pos <- out_pos + len;
;;

let add_string t s = add_substring t s ~pos:0 ~len:(String.length s)

let digit n = Char.of_int_exn (Char.to_int '0' + n)

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

let set_mode t mode =
  if t.mode.mouse <> mode.mouse then
    add_string t
      (match mode.mouse with
       | true  -> "\027[?1000h"
       | false -> "\027[?1000l");
  if t.mode.screen <> mode.screen then
    add_string t
      (match mode.screen with
       | Main        -> "\027[?1049l"
       | Alternative -> "\027[?1049h");
  t.mode <- mode
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
  let code = Uchar.code c in
  if code < 128 then
    add_char t (Char.of_int_exn code)
  else begin
    reserve t 4;
    t.out_pos <- Zed_utf8.encode_to_bigstring t.out_buf c ~pos:t.out_pos
  end
;;

let unknown_char = Uchar.of_int 0xfffd

let render_char t ~char =
  (* Skip control characters, otherwise output will be messy. *)
  if Uchar.code char < 32 then
    add_uchar t unknown_char
  else
    add_uchar t (map_char char)
;;

type render_kind = Render_screen | Render_box

let render_gen t kind ?(old=[||]) (matrix : LTerm_draw.matrix) =
  add_string t
    (match kind with
     | Render_screen -> "\027[H\027[0m" (* Go the top-left and reset attributes *)
     | Render_box    -> "\r\027[0m"     (* Go the beginnig of line and reset attributes *)
    );
  (* The last displayed style. *)
  let curr_style = ref LTerm_style.default in
  let rows = Array.length matrix in
  let old_rows =
    let old_rows = Array.length old in
    if rows = 0 || old_rows <> rows ||
       Array.length matrix.(0) <> Array.length old.(0) then
      0 (* No update if dimmensions changed *)
    else
      old_rows
  in
  for y = 0 to rows - 1 do
    let line = matrix.(y) in
    (* If the current line is equal to the displayed one, skip it *)
    if y >= old_rows || line <> old.(y) then begin
      for x = 0 to Array.length line - 1 do
        let point = line.(x) in
        let style = LTerm_style.on_default point.style in
        if not (LTerm_style.equal !curr_style style) then begin
          curr_style := style;
          add_style t ~style;
        end;
        render_char t ~char:point.char;
        curr_style := style
      done
    end;
    if y < rows - 1 then add_char t '\n'
  done;
  add_string t
    (match kind with
     | Render_screen -> "\027[0m"
     | Render_box    -> "\027[0m\r");
;;

let render t ?old matrix = render_gen t Render_screen ?old matrix

let print_box t ?old matrix =
  if Array.length matrix > 0 then
    render_gen t Render_box ?old matrix
  else
    add_char t '\r'
;;

let nothing_from line mark =
  let rec nothing_until_eol (line:LTerm_draw.point array) x =
    if x = Array.length line then
      true
    else
      let c = Uchar.code line.(x).char in
      c = 10 || (c = 32 && nothing_until_eol line (x + 1))
  in
  let rec nothing_before (line:LTerm_draw.point array) x mark =
    if x = mark then
      nothing_until_eol line x
    else
      let c = Uchar.code line.(x).char in
      c = 10 || (c = 32 && nothing_before line (x + 1) mark)
  in
  nothing_before line 0 mark
;;

let print_box_with_newlines t ?(old=[||]) (matrix : LTerm_draw.matrix) =
  (* Go the the beginnig of line and reset attributes *)
  add_string t "\r\027[0m";
  (* The last displayed style. *)
  let curr_style = ref LTerm_style.default in
  let rows = Array.length matrix in
  let old_rows =
    let old_rows = Array.length old in
    if rows = 0 || old_rows <> rows ||
       Array.length matrix.(0) <> Array.length old.(0) then
      0 (* No update if dimmensions changed *)
    else
      old_rows
  in
  for y = 0 to rows - 1 do
    let line = matrix.(y) in
    (* If the current line is equal to the displayed one, skip it *)
    if y >= old_rows || line <> old.(y) then begin
      let cols = Array.length line - 1 in
      if cols < 0 then invalid_arg "LTerm.print_box_with_newlines";
      let x = ref 0 in
      let continue = ref true in
      while !x < cols && !continue do
        let point = line.(!x) in
        let style = LTerm_style.on_default point.style in
        if not (LTerm_style.equal !curr_style style) then begin
          curr_style := style;
          add_style t ~style;
        end;
        let code = Uchar.code point.char in
        if code = 10 then begin
          (* Erase everything until the end of line, if needed. *)
          if not (y < old_rows && nothing_from old.(y) !x) then
            add_3chars t '\027' '[' 'K';
          continue := false;
        end else begin
          render_char t ~char:point.char;
          incr x;
        end
      done;
      let point = line.(!x) in
      if Uchar.code point.char = 10 && y < rows - 1 then add_char t '\n'
    end
  done;
  add_string t "\027[0m\r"
;;

let print_box_with_newlines t ?old matrix =
  if Array.length matrix > 0 then
    print_box_with_newlines t ?old matrix
  else
    add_char t '\r'
;;

(* +-----------------------------------------------------------------+
   | Writing                                                         |
   +-----------------------------------------------------------------+ *)

(* Compare attributes that we care about *)
let attr_equal (a : Unix.Terminal_io.t) (b : Unix.Terminal_io.t) =
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

external write : Unix.file_descr -> buf:bigstring -> pos:int -> len:int -> int
  = "lt_term_bigarray_read"

let rec really_write fd ~buf ~pos ~len =
  if len > 0 then
    match write fd ~buf ~pos ~len with
    | n -> really_write fd ~buf ~pos:(pos + n) ~len:(len - n)
    | exception Unix.Unix_error(EINTR, _, _) ->
      really_write fd ~buf ~pos ~len

let do_sync ?end_of_display t =
  let to_write  = t.out_pos   in
  let new_mode  = t.mode      in
  let real_mode = t.real_mode in
  let new_mode_termios_only =
    Mode.copy_non_termios_fields mode ~from:t.real_mode
  in
  if not (new_mode_termios_only <> real_mode) then begin
    let attr = Terminal_io.get_attr t.fd_in in
    let init_attr =
      match t.initial_attr with
      | Some x -> x
      | None ->
        let copy = { attr with c_isig = attr.c_isig } in
        t.initial_attr <- Some copy;
        copy
    in
    attr.c_echo <- mode.echo;
    attr.c_isig <- mode.signals;
    if mode.raw then begin
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
    Terminal_io.set_attr fd attr;
    t.real_mode <- new_mode_termios_only;
    t.attr_modified <- not (attr_equal attr init_attr);
  end;
  let cursor = t.cursor in
  really_write fd ~buf:t.out_buf ~pos:0 ~len:to_write;
  assert (t.out_pos = to_write);
  t.out_pos        <- 0;
  t.real_mode      <- new_mode;
  t.real_cursor    <- cursor;
  t.end_of_display <- end_of_display;
;;

let sync ?end_of_display t =
  match t.state with
  | Inactive -> ()
  | Closed -> failwiths "Term.sync called on closed terminal"
  | Active ->
  let mode        = t.mode                        in
  let cursor      = t.cursor                      in
  let current_pos = t.out_pos                     in
  let len         = current_pos - t.syncing_up_to in
  t.  mode_when_synced <- mode;
  t.cursor_when_synced <- cursor;
  t.syncing_up_to      <- current_pos;
  do_sync ?end_of_display t
;;

(* +-----------------------------------------------------------------+
   | Creation                                                        |
   +-----------------------------------------------------------------+ *)

let default_model = Option.value (Sys.getenv "TERM") ~default:"dumb"

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
    Core.Std.Printf.eprintf
      "Warning: the detected system character encoding is not UTF-8.\n\
       Lambda term work best when it is UTF-8.\n%!"
)
;;

let create ?(model=default_model) fd =
  Lazy.force check_utf8;
  if Core.Std.Unix.isatty (Fd.file_descr_exn fd) then begin
    (* Colors stuff. *)
    let colors = colors_of_term model in
    let color_map =
      match colors with
      | 16  -> LTerm_color_mappings.colors_16
      | 88  -> LTerm_color_mappings.colors_88
      | 256 -> LTerm_color_mappings.colors_256
      | _   -> assert false
    in
    let size = get_size_from_fd (Fd.file_descr_exn fd) in
    let t =
      { state = Active
      ; model
      ; colors
      ; bold_is_bright = bold_is_bright model
      ; color_map
      ; fd
      ; out_buf = Bigstring.create 16384
      ; out_pos = 0
      ; size
      ; event_parser = LTerm_unix.Event_parser.create ~escape_time:(sec 0.1) fd
      ; current_write = return (`Ok ())
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
      ; closed = Ivar.create ()
      }
    in
    terminals := t :: !terminals;
    sync_forever t;
    Ok t
  end else
    Or_error.error "reader fd is not a TTY device" fd Fd.sexp_of_t
;;

let really_close t =
  reset t;
  let d = sync t in
  t.state <- Closed;
  Event_queue.close t.events;
  LTerm_unix.Event_parser.set_active t.event_parser false;
  d >>> fun () ->
  (* We remove the terminal from the list only at the end so that Ctrl+C still does the
     right thing until then. *)
  terminals := List.filter !terminals ~f:(fun t' -> not (phys_equal t t'));
  (* Free resources now *)
  let buf = t.out_buf in
  t.out_pos <- 0;
  t.out_buf <- Bigstring.create 0;
  Bigstring.unsafe_destroy buf;
  Ivar.fill t.closed ()
;;

let close t =
  if not (State.is_closed t.state) then really_close t;
  Ivar.read t.closed
;;

let set_active t active =
  match t.state, active with
  | Closed   ,  _    -> failwiths "Called Term.set_active on closed terminal" t sexp_of_t
  | Active   , true  -> ()
  | Inactive , false -> ()
  | Active   , false ->
    t.state <- Inactive;
    terminals := List.filter !terminals ~f:(fun t' -> not (phys_equal t t'));
    Event_queue.set_active t.events false;
    LTerm_unix.Event_parser.set_active t.event_parser false;
  | Inactive , true  ->
    t.state <- Active;
    terminals := t :: !terminals;
    LTerm_unix.Event_parser.set_active t.event_parser true;
    Event_queue.set_active t.events true;
    let size = get_size_from_fd (Fd.file_descr_exn t.fd) in
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
