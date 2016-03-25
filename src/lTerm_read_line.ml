(*
 * lTerm_read_line.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Zed
open React
open Core.Std
open Async.Std

type prompt = LTerm_text.t
type history = Zed_utf8.t list

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let common_prefix_one a b =
  let rec loop ofs =
    if ofs = String.length a || ofs = String.length b then
      String.sub a ~pos:0 ~len:ofs
    else
      let ch1, ofs1 = Zed_utf8.extract_next a ofs
      and ch2, ofs2 = Zed_utf8.extract_next b ofs in
      if ch1 = ch2 && ofs1 = ofs2 then
        loop ofs1
      else
        String.sub a ~pos:0 ~len:ofs
  in
  loop 0

let common_prefix = function
  | [] -> ""
  | word :: rest -> List.fold_left rest ~init:word ~f:common_prefix_one

let lookup word words = List.filter words ~f:(fun word' -> Zed_utf8.starts_with word' word)
let lookup_assoc word words = List.filter words ~f:(fun (word', _) -> Zed_utf8.starts_with word' word)

(* +-----------------------------------------------------------------+
   | Actions                                                         |
   +-----------------------------------------------------------------+ *)

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

let doc_of_action = function
  | #LTerm_edit.action as x        -> LTerm_edit.doc_of_action x
  | `interrupt_or_delete_next_char -> "interrupt if at the beginning of an empty line, or delete the next character."
  | `complete                      -> "complete current input."
  | `complete_bar_next             -> "go to the next possible completion in the completion bar."
  | `complete_bar_prev             -> "go to the previous possible completion in the completion bar."
  | `complete_bar_first            -> "go to the beginning of the completion bar."
  | `complete_bar_last             -> "go to the end of the completion bar."
  | `complete_bar                  -> "complete current input using the completion bar."
  | `history_prev                  -> "go to the previous entry of the history."
  | `history_next                  -> "go to the next entry of the history."
  | `accept                        -> "accept the current input."
  | `clear_screen                  -> "clear the screen."
  | `prev_search                   -> "search backward in the history."
  | `next_search                   -> "search forward in the history."
  | `cancel_search                 -> "cancel search mode."
  | `break                         -> "cancel edition."

module Bindings = Zed_input.Make (LTerm_event)

let bindings = ref Bindings.empty

let bind seq actions = bindings := Bindings.add seq actions !bindings
let unbind seq = bindings := Bindings.remove seq !bindings

let () =
  let ( --> ) s cmd = bind ([%of_sexp: LTerm_event.t list] (Sexp.of_string s)) [cmd] in
  "(home)"    --> `goto_bot;
  "(end)"     --> `goto_eot;
  "(up)"      --> `history_prev;
  "(down)"    --> `history_next;
  "(tab)"     --> `complete;
  "(enter)"   --> `accept;
  "(C-b)"     --> `prev_char;
  "(C-f)"     --> `next_char;
  "(C-h)"     --> `delete_prev_char;
  "(intr)"    --> `break;
  "(C-m)"     --> `accept;
  "(C-l)"     --> `clear_screen;
  "(C-r)"     --> `prev_search;
  "(C-d)"     --> `interrupt_or_delete_next_char;
  "(M-p)"     --> `history_prev;
  "(M-n)"     --> `history_next;
  "(M-left)"  --> `complete_bar_prev;
  "(M-right)" --> `complete_bar_next;
  "(M-home)"  --> `complete_bar_first;
  "(M-end)"   --> `complete_bar_last;
  "(M-tab)"   --> `complete_bar;
  "(M-down)"  --> `complete_bar;
  "(M-enter)" --> `newline;
  "(escape)"  --> `cancel_search;
;;

(* +-----------------------------------------------------------------+
   | The read-line engine                                            |
   +-----------------------------------------------------------------+ *)

let search_string str sub =
  let rec equal_at a b =
    (b = String.length sub) || (str.[a] = sub.[b]) && equal_at (a + 1) (b + 1)
  in
  let rec loop ofs idx =
    if ofs + String.length sub > String.length str then
      None
    else
      if equal_at ofs 0 then
        Some idx
      else
        loop (Zed_utf8.next str ofs) (idx + 1)
  in
  loop 0 0

let macro : action Zed_macro.t = Zed_macro.create []

type mode =
  | Edition
  | Search
  | Set_counter
  | Add_counter

type completion_state = {
  start : int; (* Beginning of the word being completed *)
  index : int; (* Index of the selected in [words]      *)
  count : int; (* Length of [words]                     *)
  words : (Zed_utf8.t * Zed_utf8.t) list;
}

let no_completion = {
  start = 0;
  index = 0;
  words = [];
  count = 0;
}

type direction = Forward | Backward

type search_status =
  { before : Zed_utf8.t list
  ; after  : Zed_utf8.t list
  ; match_ : (Zed_utf8.t * int) option
  }

class virtual ['a] engine ?(history = []) ?(clipboard = LTerm_edit.clipboard) ?(macro = macro) () =
  let edit : unit Zed_edit.t = Zed_edit.create ~clipboard () in
  let context = Zed_edit.context edit (Zed_edit.new_cursor edit) in
  let mode, set_mode = S.create Edition in
  let user_completion_state, set_completion_state = E.create () in
  let reset_completion_state =
    E.when_
      (S.map (fun mode -> mode = Edition) mode)
      (E.select [
         E.stamp (Zed_edit.changes edit                                    ) no_completion;
         E.stamp (S.changes (Zed_cursor.position (Zed_edit.cursor context))) no_completion;
       ])
  in
  let completion_state =
    S.hold ~eq:phys_equal no_completion (E.select [reset_completion_state; user_completion_state])
  in
  let completion_words = S.map ~eq:phys_equal (fun c -> c.words) completion_state in
  let completion_index = S.map                (fun c -> c.index) completion_state in
  let history, set_history = S.create (history, []) in
  let message, set_message = S.create None in
  object(self)
    method virtual eval : 'a
    method edit = edit
    method context = context
    method show_box = true
    method mode = mode
    method history = history
    method message = message
    method clipboard = clipboard
    method macro = macro

    (* The event which occurs when completion need to be recomputed. *)
    val mutable completion_event = E.never

    (* Save for when setting the macro counter. *)
    val mutable save = (0, Zed_rope.empty)

    method set_completion ?(index=0) start words =
      let count = List.length words in
      if index < 0 || index > max 0 (count - 1) then
        invalid_arg
          "LTerm_read_line.set_completion: \
           index out of bounds compared to words.";
      set_completion_state { start; index; count; words }

    initializer
      completion_event <- (
        E.map (fun _ ->
          (* We can't execute it right now as the user might call [set_completion]
             immediatly. *)
          Deferred.unit >>> fun () -> self#completion)
          reset_completion_state
      );
      self#completion

    method input_prev =
      Zed_rope.before (Zed_edit.text edit) (Zed_edit.position context)

    method input_next =
      Zed_rope.after (Zed_edit.text edit) (Zed_edit.position context)

    method completion_words = completion_words
    method completion_index = completion_index
    method completion = self#set_completion 0 []

    method complete =
      let comp = S.value completion_state in
      let prefix_length = Zed_edit.position context - comp.start in
      match comp.words with
      | [] ->
        ()
      | [(completion, suffix)] ->
        Zed_edit.insert context (Zed_rope.of_string
                                   (Zed_utf8.after completion prefix_length));
        Zed_edit.insert context (Zed_rope.of_string suffix)
      | (completion, _suffix) :: rest ->
        let word =
          List.fold_left
            rest ~init:completion
            ~f:(fun acc (word, _) -> common_prefix_one acc word)
        in
        Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after word prefix_length))

    (* The event which search for the string in the history. *)
    val mutable search_event = E.never

    val mutable search_status = None

    initializer
      let reset_search _ =
        search_status <- None;
        self#search Backward
      in
      search_event <-
        E.map reset_search
          (E.when_ (S.map (fun mode -> mode = Search) mode)
             (Zed_edit.changes edit))

  method private search direction =
    let do_search direction =
      let set_status other_entries entries match_ =
        let before, after =
          match direction with
          | Backward -> (other_entries, entries)
          | Forward  -> (entries, other_entries)
        in
        search_status <- Some { before; after; match_ }
      in
      let input = Zed_rope.to_string (Zed_edit.text edit) in
      let rec loop other_entries entries =
        match entries with
        | [] ->
          set_status other_entries entries None;
          set_message (Some(LTerm_text.of_string "Reverse search: not found"))
        | entry :: rest ->
          match search_string entry input with
          | Some pos -> begin
              match search_status with
              | Some { match_ = Some (entry', _); _ } when entry = entry' ->
                loop (entry :: other_entries) rest
              | _ ->
                set_status other_entries rest (Some (entry, pos));
                let txt = LTerm_text.of_string entry in
                for i = pos to pos + Zed_rope.length (Zed_edit.text edit) - 1 do
                  let pt = txt.(i) in
                  pt.style <- LTerm_style.set_underline pt.style On
                done;
                set_message
                  (Some (Array.append (LTerm_text.of_string "Reverse search: ") txt))
            end
          | None ->
            loop (entry :: other_entries) rest
      in
      match search_status with
      | None ->
        let hist = fst (S.value history) in
        loop []
          (match direction with
           | Backward -> hist
           | Forward  -> List.rev hist)
      | Some { before; after; match_ } ->
        let other_entries, entries =
          match direction with
          | Backward -> (before, after)
          | Forward  -> (after, before)
        in
        let other_entries =
          match match_ with
          | None -> other_entries
          | Some (entry, _) -> entry :: other_entries
        in
        loop other_entries entries
    in
    match S.value mode with
    | Search -> do_search direction
    | Edition ->
      let text = Zed_edit.text edit in
      Zed_edit.goto context 0;
      Zed_edit.remove context (Zed_rope.length text);
      let prev, next = S.value history in
      set_history (Zed_rope.to_string text :: (List.rev_append next prev), []);
      search_status <- None;
      set_mode Search;
      do_search direction
    | _ ->
      ()

    method insert s = Zed_edit.insert context (Zed_rope.of_string s)

    method send_action action =
      if action <> `stop_macro then Zed_macro.add macro action;
      match action with
      | (`complete | `complete_bar | `accept) when S.value mode = Search -> begin
          set_mode Edition;
          set_message None;
          match search_status with
          | Some { match_ = Some (entry, _); _ } ->
            search_status <- None;
            Zed_edit.goto context 0;
            Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
            Zed_edit.insert context (Zed_rope.of_string entry)
          | Some { match_ = None; _ } | None ->
            ()
        end

      | #Zed_edit.action as action ->
        Zed_edit.get_action action context

      | `interrupt_or_delete_next_char ->
        if not (Zed_rope.is_empty (Zed_edit.text edit)) then
          Zed_edit.delete_next_char context

      | `complete when S.value mode = Edition ->
        self#complete

      | `complete_bar_next when S.value mode = Edition ->
        let comp = S.value completion_state in
        if comp.index < comp.count - 1 then
          set_completion_state { comp with index = comp.index + 1 }

      | `complete_bar_prev when S.value mode = Edition ->
        let comp = S.value completion_state in
        if comp.index > 0 then
          set_completion_state { comp with index = comp.index - 1 }

      | `complete_bar_first when S.value mode = Edition ->
        let comp = S.value completion_state in
        if comp.index > 0 then
          set_completion_state { comp with index = 0 }

      | `complete_bar_last when S.value mode = Edition ->
        let comp = S.value completion_state in
        if comp.index < comp.count - 1 then
          set_completion_state { comp with index = comp.count - 1 }

      | `complete_bar when S.value mode = Edition ->
        let comp = S.value completion_state in
        if comp.words <> [] then begin
          let prefix_length = Zed_edit.position context - comp.start in
          let completion, suffix = List.nth_exn comp.words comp.index in
          Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
          Zed_edit.insert context (Zed_rope.of_string suffix)
        end

      | `history_prev when S.value mode = Edition ->begin
          let prev, next = S.value history in
          match prev with
          | [] ->
            ()
          | line :: rest ->
            let text = Zed_edit.text edit in
            set_history (rest, Zed_rope.to_string text :: next);
            Zed_edit.goto context 0;
            Zed_edit.remove context (Zed_rope.length text);
            Zed_edit.insert context (Zed_rope.of_string line)
        end

      | `history_next when S.value mode = Edition -> begin
          let prev, next = S.value history in
          match next with
          | [] ->
            ()
          | line :: rest ->
            let text = Zed_edit.text edit in
            set_history (Zed_rope.to_string text :: prev, rest);
            Zed_edit.goto context 0;
            Zed_edit.remove context (Zed_rope.length text);
            Zed_edit.insert context (Zed_rope.of_string line)
        end

      | `prev_search -> self#search Backward
      | `next_search -> self#search Forward

      | `cancel_search ->
        if S.value mode = Search then begin
          set_mode Edition;
          set_message None
        end

      | `start_macro when S.value mode = Edition ->
        Zed_macro.set_recording macro true

      | `stop_macro ->
        Zed_macro.set_recording macro false

      | `cancel_macro ->
        Zed_macro.cancel macro

      | `play_macro ->
        Zed_macro.cancel macro;
        List.iter (Zed_macro.contents macro) ~f:self#send_action

      | `insert_macro_counter ->
        Zed_edit.insert context (Zed_rope.of_string (string_of_int (Zed_macro.get_counter macro)));
        Zed_macro.add_counter macro 1

      | `set_macro_counter when S.value mode = Edition ->
        let text = Zed_edit.text edit in
        save <- (Zed_edit.position context, text);
        Zed_edit.goto context 0;
        Zed_edit.remove context (Zed_rope.length text);
        set_mode Set_counter;
        set_message (Some (LTerm_text.of_string "Enter a value for the macro counter."))

      | `add_macro_counter when S.value mode = Edition ->
        let text = Zed_edit.text edit in
        save <- (Zed_edit.position context, text);
        Zed_edit.goto context 0;
        Zed_edit.remove context (Zed_rope.length text);
        set_mode Add_counter;
        set_message (Some (LTerm_text.of_string "Enter a value to add to the macro counter."))

      | `accept -> begin
          match S.value mode with
          | Edition | Search ->
            ()
          | Set_counter ->
            let pos, text = save in
            save <- (0, Zed_rope.empty);
            (try
               Zed_macro.set_counter macro (int_of_string (Zed_rope.to_string (Zed_edit.text edit)))
             with Failure _ ->
               ());
            Zed_edit.goto context 0;
            Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
            Zed_edit.insert context text;
            Zed_edit.goto context pos;
            set_mode Edition;
            set_message None
          | Add_counter ->
            let pos, text = save in
            save <- (0, Zed_rope.empty);
            (try
               Zed_macro.add_counter macro (int_of_string (Zed_rope.to_string (Zed_edit.text edit)))
             with Failure _ ->
               ());
            Zed_edit.goto context 0;
            Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
            Zed_edit.insert context text;
            Zed_edit.goto context pos;
            set_mode Edition;
            set_message None
        end

      | `custom f -> f ()

      | _ ->
        ()

    method stylise last =
      let txt = LTerm_text.of_rope (Zed_edit.text edit) in
      let pos = Zed_edit.position context in
      if not last && Zed_edit.get_selection edit then begin
        let mark = Zed_cursor.get_position (Zed_edit.mark edit) in
        let a = min pos mark and b = max pos mark in
        for i = a to b - 1 do
          let pt = txt.(i) in
          pt.style <- LTerm_style.set_underline pt.style On
        done;
      end;
      (txt, pos)
  end

class virtual ['a] abstract = object
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

(* +-----------------------------------------------------------------+
   | Predefined classes                                              |
   +-----------------------------------------------------------------+ *)

class read_line ?history () = object(self)
  inherit [Zed_utf8.t] engine ?history ()
  method eval = Zed_rope.to_string (Zed_edit.text self#edit)
end

class read_password () = object(self)
  inherit [Zed_utf8.t] engine () as super

  method! stylise last =
    let text, pos = super#stylise last in
    for i = 0 to Array.length text - 1 do
      text.(i).char <- Uchar.of_char '*';
    done;
    (text, pos)

  method eval = Zed_rope.to_string (Zed_edit.text self#edit)

  method! show_box = false

  method! send_action = function
    | `prev_search | `next_search -> ()
    | action -> super#send_action action
end

class ['a] read_keyword ?history () = object(self)
  inherit [('a, Zed_utf8.t) Result.t] engine ?history ()

  method keywords = []

  method eval =
    let input = Zed_rope.to_string (Zed_edit.text self#edit) in
    match List.Assoc.find self#keywords input with
    | Some x -> Ok x
    | None   -> Error input

  method! completion =
    let word = Zed_rope.to_string self#input_prev in
    let keywords =
      List.filter self#keywords ~f:(fun (keyword, _value) ->
        Zed_utf8.starts_with keyword word)
    in
    self#set_completion 0 (List.map keywords ~f:(fun (keyword, _value) -> (keyword, "")))
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let newline = Uchar.of_char '\n'
let vline = LTerm_draw.Piece.vline Light
let reverse_style = LTerm_style.make ~reverse:On ()
let default_prompt = LTerm_text.of_string "# "

let rec drop count l =
  if count <= 0 then
    l
  else match l with
    | [] -> []
    | _ :: l -> drop (count - 1) l

(* Computes the position of the cursor after printing the given styled
   string:
   - [pos] is the current cursor position
     (it may be at column [max-column + 1])
   - [text] is the text to display
   - [start] is the start of the chunk to display in [text]
   - [stop] is the end of the chunk to display in [text]
*)
let rec compute_position cols (pos:LTerm_geom.coord) (text:LTerm_text.t) start stop =
  if start = stop then
    pos
  else
    let ch = text.(start).char in
    if ch = newline then
      compute_position cols { row = pos.row + 1; col = 0 } text (start + 1) stop
    else if pos.col = cols then
      compute_position cols { row = pos.row + 1; col = 1 } text (start + 1) stop
    else
      compute_position cols { pos with col = pos.col + 1 } text (start + 1) stop

(* Return the "real" position of the cursor, i.e. on the screen. *)
let real_pos cols (pos:LTerm_geom.coord) : LTerm_geom.coord =
  if pos.col = cols then
    { row = pos.row + 1; col = 0 }
  else
    pos

let rec get_index_of_last_displayed_word column columns index words =
  match words with
    | [] ->
        index - 1
    | (word, _suffix) :: words ->
        let column = column + Zed_utf8.length word in
        if column <= columns - 1 then
          get_index_of_last_displayed_word (column + 1) columns (index + 1) words
        else
          index - 1

let draw_styled ctx row col str =
  let size = LTerm_draw.size ctx in
  let rec loop row col idx =
    if idx < Array.length str then begin
      let { LTerm_text. char; style } = str.(idx) in
      if char = newline then
        loop (row + 1) 0 (idx + 1)
      else begin
        let point = LTerm_draw.point ctx ~row ~col in
        point.char <- char;
        LTerm_draw.set_style point ~style;
        let col = col + 1 in
        if col = size.cols then
          loop (row + 1) 0 (idx + 1)
        else
          loop row col (idx + 1)
      end
    end
  in
  loop row col 0

let draw_styled_with_newlines (matrix:LTerm_draw.matrix) cols row col str =
  let rec loop row col idx =
    if idx < Array.length str then begin
      let { LTerm_text. char; style } = str.(idx) in
      if char = newline then begin
        matrix.(row).(col).char <- newline;
        loop (row + 1) 0 (idx + 1)
      end else begin
        let row, col =
          if col = cols then
            (row + 1, 0)
          else
            (row, col)
        in
        let point = matrix.(row).(col) in
        point.char <- char;
        LTerm_draw.set_style point ~style;
        loop row (col + 1) (idx + 1)
      end
    end
  in
  loop row col 0

let styled_newline : LTerm_text.t = [| { char  = newline
                                       ; style = LTerm_style.none } |]

module Res = struct
  type 'a t =
    | Ok of 'a
    | Interrupt
    | Break
    | Error of exn
  [@@deriving sexp_of]
end

class virtual ['a] term term =
  let size, set_size = S.create (LTerm.size term) in
  let event, set_prompt = E.create () in
  let prompt = S.switch (S.hold ~eq:phys_equal (S.const default_prompt) event) in
  let event_sequence, set_event_sequence = S.create [] in
  object(self)
    inherit ['a] abstract
    method size = size
    method prompt = prompt
    method set_prompt prompt = set_prompt prompt

    val mutable visible = true
    (* Whether the read-line instance is currently visible. *)

    val mutable displayed = false
    (* Whether the read-line instance is currently displayed on the
       screen. *)

    val mutable drawing_generation = 0
    (* Incremented each time a refresh is requested to know if what is shown is up to
       date *)

    val mutable drawer = Deferred.unit
    (* The current drawing operation *)

    val mutable clear_screen = false
    (* Clear the screen before the next draw *)

    val mutable cursor : LTerm_geom.coord = { row = 0; col = 0 }
    (* The position of the cursor. *)

    val mutable completion_start = S.const 0
    (* Index of the first displayed word in the completion bar. *)

    val mutable height = 0
    (* The height of the displayed material. *)

    val mutable resolver = None
    (* The current resolver for resolving input sequences. *)

    val mutable running = false

    initializer
      completion_start <- (
        S.fold
          (fun start (words, index, columns) ->
             if index < start then
               (* The cursor is before the left margin. *)
               let count = List.length words in
               let rev_index = count - index - 1 in
               count - get_index_of_last_displayed_word 1 columns rev_index
                         (drop rev_index (List.rev words)) - 1
             else if index > get_index_of_last_displayed_word 1 columns start
                               (drop start words) then
               (* The cursor is after the right margin. *)
               index
             else
               start)
          0
          (S.changes
             (S.l3
                (fun words index (size:LTerm_geom.size) -> (words, index, size.cols))
                self#completion_words
                self#completion_index
                size))
      )

    method event_sequence = event_sequence

    method completion_start = completion_start

    method private queue_draw_update =
      drawing_generation <- drawing_generation + 1;
      if Deferred.is_determined drawer then
        drawer <- Deferred.unit >>= fun () -> self#draw_update

    method private really_draw =
      let size = S.value size in
      let styled, position = self#stylise false in
      let prompt = S.value prompt in
      (* Compute the position of the cursor after displaying the prompt. *)
      let pos_after_prompt =
        compute_position size.cols { row = 0; col = 0 } prompt 0 (Array.length prompt)
      in
      (* Compute the position of the cursor after displaying the input before the
         cursor. *)
      let pos_after_before =
        compute_position size.cols pos_after_prompt styled 0 position
      in
      (* Compute the position of the cursor after displaying the input. *)
      let pos_after_styled =
        compute_position size.cols pos_after_before styled position (Array.length styled)
      in
      (* Compute the position of the cursor after displaying the newline used to end the
         input. *)
      let pos_after_newline =
        compute_position size.cols pos_after_styled styled_newline 0 1
      in
      (* The real position of the cursor on the screen. *)
      let pos_cursor = real_pos size.cols pos_after_before in
      (* Height of prompt+input. *)
      let prompt_input_height = max (pos_cursor.row + 1) pos_after_newline.row in
      let matrix =
        if self#show_box && size.cols > 2 then
          match S.value self#message with
          | Some msg ->
            (* Compute the height of the message. *)
            let message_height =
              (compute_position (size.cols - 2) { row = 0; col = 0 } msg 0
                 (Array.length msg)).row + 1
            in
            (* The total height of the displayed text. *)
            let total_height = prompt_input_height + message_height + 2 in

            (* Create the matrix for rendering. *)
            let matrix_size : LTerm_geom.size =
              { cols = size.cols + 1
              ; rows = if displayed then max total_height height else total_height }
            in
            let matrix = LTerm_draw.make_matrix matrix_size in

            (* Update the height parameter. *)
            height <- total_height;

            (* Draw the prompt and the input. *)
            draw_styled_with_newlines matrix size.cols 0 0 prompt;
            draw_styled_with_newlines matrix size.cols pos_after_prompt.row
              pos_after_prompt.col styled;
            draw_styled_with_newlines matrix size.cols pos_after_styled.row
              pos_after_styled.col styled_newline;

            let ctx =
              LTerm_draw.sub (LTerm_draw.context matrix matrix_size)
                { row1 = 0
                ; col1 = 0
                ; row2 = matrix_size.rows
                ; col2 = size.cols
                }
            in

            (* Draw a frame for the message. *)
            LTerm_draw.draw_frame ctx
              { row1 = prompt_input_height
              ; col1 = 0
              ; row2 = total_height
              ; col2 = size.cols
              } Light;
            for row = prompt_input_height to total_height - 1 do
              matrix.(row).(size.cols).char <- newline
            done;

            (* Draw the message. *)
            let ctx =
              LTerm_draw.sub ctx
                { row1 = prompt_input_height + 1
                ; col1 = 1
                ; row2 = total_height - 1
                ; col2 = size.cols - 1
                }
            in
            draw_styled ctx 0 0 msg;

            matrix

          | None ->
            let comp_start = S.value self#completion_start in
            let comp_index = S.value self#completion_index in
            let comp_words = drop comp_start (S.value self#completion_words) in

            (* The total height of the displayed text. *)
            let total_height = prompt_input_height + 3 in

            (* Create the matrix for the rendering. *)
            let matrix_size : LTerm_geom.size =
              { cols = size.cols + 1
              ; rows = if displayed then max total_height height else total_height }
            in
            let matrix = LTerm_draw.make_matrix matrix_size in

            (* Update the height parameter. *)
            height <- total_height;

            (* Draw the prompt and the input. *)
            draw_styled_with_newlines matrix size.cols 0 0 prompt;
            draw_styled_with_newlines matrix size.cols pos_after_prompt.row
              pos_after_prompt.col styled;
            draw_styled_with_newlines matrix size.cols pos_after_styled.row
              pos_after_styled.col styled_newline;

            let ctx =
              LTerm_draw.sub (LTerm_draw.context matrix matrix_size)
                { row1 = 0
                ; col1 = 0
                ; row2 = matrix_size.rows
                ; col2 = size.cols
                }
            in

            (* Draw a frame for the completion. *)
            LTerm_draw.draw_frame ctx
              { row1 = prompt_input_height
              ; col1 = 0
              ; row2 = total_height
              ; col2 = size.cols
              } Light;
            for row = prompt_input_height to total_height - 1 do
              matrix.(row).(size.cols).LTerm_draw.char <- newline
            done;

            (* Draw the completion. *)
            let ctx =
              LTerm_draw.sub ctx
                { row1 = prompt_input_height + 1
                ; col1 = 1
                ; row2 = total_height - 1
                ; col2 = size.cols - 1
                }
            in

            let rec loop idx col = function
              | [] ->
                ()
              | (word, _suffix) :: words ->
                let len = Zed_utf8.length word in
                LTerm_draw.UTF8.draw ctx ~row:0 ~col word;
                (* Apply the reverse style if this is the selected word. *)
                if idx = comp_index then
                  for col = col to min (col + len - 1) (size.cols - 2) do
                    LTerm_draw.set_style (LTerm_draw.point ctx ~row:0 ~col)
                      ~style:reverse_style
                  done;
                (* Draw a separator. *)
                LTerm_draw.draw_piece ctx ~row:0 ~col:(col + len) vline;
                let col = col + len + 1 in
                if col < size.cols - 2 then loop (idx + 1) col words
            in
            loop comp_start 0 comp_words;

            matrix

        else begin
          let total_height = prompt_input_height in
          let matrix_size : LTerm_geom.size =
            { cols = size.cols + 1
            ; rows = if displayed then max total_height height else total_height }
          in
          let matrix = LTerm_draw.make_matrix matrix_size in
          height <- total_height;
          draw_styled_with_newlines matrix size.cols 0 0 prompt;
          draw_styled_with_newlines matrix size.cols pos_after_prompt.row
            pos_after_prompt.col styled;
          matrix
        end
      in
      LTerm.hide_cursor term;
      (* Go back to the beginning of displayed text. *)
      if displayed then
        LTerm.move term ~rows:(-cursor.row) ~cols:(-cursor.col)
      else
        (* Ensure we are at the beginning of line otherwise all offset calculation will be
           wrong. *)
        LTerm.print term "\r";
      (* Display everything. *)
      LTerm.print_box_with_newlines term matrix;
      (* Update the cursor. *)
      cursor <- pos_cursor;
      (* Move the cursor to the right position. *)
      LTerm.move term ~rows:(cursor.row - Array.length matrix + 1) ~cols:cursor.col;
      LTerm.show_cursor term;

    method private draw_update =
      if running then begin
        let rendered_generation = drawing_generation in
        let size = S.value size in
        if size.rows > 0 && size.cols > 0 then begin
          if clear_screen then begin
            clear_screen <- false;
            LTerm.clear_screen term;
            LTerm.goto term { row = 0; col = 0 };
            displayed <- false;
          end;
          if visible then
            self#really_draw
          else if displayed then begin
            let matrix_size : LTerm_geom.size = { cols = size.cols + 1; rows = height } in
            let matrix = LTerm_draw.make_matrix matrix_size in
            for row = 0 to height - 1 do
              matrix.(row).(0).LTerm_draw.char <- newline
            done;
            LTerm.move term ~rows:(-cursor.row) ~cols:(-cursor.col);
            LTerm.print_box_with_newlines term matrix;
            LTerm.move term ~rows:(1 - Array.length matrix) ~cols:0;
            cursor <- { row = 0; col = 0 };
            height <- 0;
          end;
          if visible || displayed then begin
            displayed <- visible;
            let end_of_display =
              if visible then
                Some (height - cursor.row)
              else
                None
            in
            LTerm.sync ?end_of_display term
            >>= fun () ->
            if drawing_generation <> rendered_generation then
              self#draw_update
            else
              Deferred.unit
          end else
            Deferred.unit
        end else
          Deferred.unit
      end else
        Deferred.unit

    method private draw_success =
      let size = S.value size in
      if size.rows > 0 && size.cols > 0 then begin
        let styled, position = self#stylise true in
        let prompt = S.value prompt in
        let pos_after_prompt =
          compute_position size.cols { row = 0; col = 0 } prompt 0 (Array.length prompt)
        in
        let pos_after_before =
          compute_position size.cols pos_after_prompt styled 0 position
        in
        let pos_after_styled =
          compute_position size.cols pos_after_before styled position (Array.length styled)
        in
        let total_height = pos_after_styled.row + 1 in
        let matrix_size : LTerm_geom.size =
          { cols = size.cols + 1
          ; rows = if displayed then max total_height height else total_height }
        in
        let matrix = LTerm_draw.make_matrix matrix_size in
        draw_styled_with_newlines matrix size.cols 0 0 prompt;
        draw_styled_with_newlines matrix size.cols pos_after_prompt.row
          pos_after_prompt.col styled;
        draw_styled_with_newlines matrix size.cols pos_after_styled.row
          pos_after_styled.col styled_newline;
        if displayed then LTerm.move term ~rows:(-cursor.row) ~cols:(-cursor.col);
        LTerm.print_box_with_newlines term matrix;
        LTerm.move term ~rows:(total_height - Array.length matrix) ~cols:0;
        (* Print a newline instead of a movement to ensure scrolling when at the end of
           screen. *)
        LTerm.print term "\n";
        LTerm.sync term
      end else
        Deferred.unit

    method private draw_failure = self#draw_success

    method hide =
      if visible then begin
        visible <- false;
        self#queue_draw_update
      end

    method show =
      if not visible then begin
        visible <- true;
        self#queue_draw_update
      end

    val mutable local_bindings = Bindings.empty
    method bind keys actions = local_bindings <- Bindings.add keys actions local_bindings

    (* The main loop. *)
    method private loop =
      LTerm.read_event term
      >>= function
      | Resize ->
        set_size (LTerm.size term);
        self#queue_draw_update;
        self#loop
      | Resume ->
        cursor <- { row = 0; col = 0 };
        height <- 0;
        displayed <- false;
        set_size (LTerm.size term);
        if visible then self#queue_draw_update;
        self#loop
      | Text s ->
        resolver <- None;
        Zed_macro.add self#macro (`insert s);
        self#insert s;
        self#loop
      | ev ->
        let res =
          match resolver with
          | Some res -> res
          | None -> Bindings.resolver [ Pack(local_bindings, Fn.id)
                                      ; Pack(!bindings, Fn.id)
                                      ; Pack(!LTerm_edit.bindings,
                                             fun x -> (x :> action list))
                                      ]
        in
        match Bindings.resolve ev res with
        | Bindings.Accepted actions ->
          resolver <- None;
          set_event_sequence [];
          self#exec actions
        | Bindings.Continue res ->
          resolver <- Some res;
          set_event_sequence (S.value event_sequence @ [ev]);
          self#loop
        | Bindings.Rejected ->
          set_event_sequence [];
          if Option.is_none resolver then
            match ev with
            | Char (N, ch) ->
              let s = Zed_utf8.singleton (Uchar.of_char ch) in
              Zed_macro.add macro (`insert s);
              self#insert s
            | Uchar (N, ch) ->
              let s = Zed_utf8.singleton ch in
              Zed_macro.add macro (`insert s);
              self#insert s
            | _ ->
              ()
          else
            resolver <- None;
          self#loop

    method private exec = function
      | `accept :: _ when S.value self#mode = Edition ->
        Zed_macro.add self#macro `accept;
        return (Res.Ok self#eval)
      | `break :: _ ->
        return Res.Break
      | `interrupt_or_delete_next_char as action :: actions ->
        if Zed_rope.is_empty (Zed_edit.text self#edit) then
          return Res.Interrupt
        else begin
          self#send_action action;
          self#exec actions
        end
      | `clear_screen :: actions ->
        Zed_macro.add self#macro `clear_screen;
        self#queue_draw_update;
        self#exec actions
      | `play_macro :: actions ->
        Zed_macro.cancel self#macro;
        self#exec (Zed_macro.contents macro @ actions)
      | action :: actions ->
        self#send_action action;
        self#exec actions
      | [] ->
        self#loop

    method run =
      running <- true;

      (* Redraw everything when needed. *)
      let event =
        E.map
          (fun () -> if running then self#queue_draw_update)
          (E.select [
             Zed_edit.update self#edit [Zed_edit.cursor self#context];
             E.stamp (S.changes prompt) ();
             E.stamp (S.changes self#completion_words) ();
             E.stamp (S.changes self#completion_index) ();
             E.stamp (S.changes self#completion_start) ();
             E.stamp (S.changes self#message) ();
           ])
      in

      let mode = LTerm.mode term in
      LTerm.set_mode term { mode with echo = false; raw = true };

      Monitor.try_with ~name:"read-line loop"
        (fun () ->
           self#queue_draw_update;
           self#loop)
      >>= fun result ->
      running <- false;
      E.stop event;
      LTerm.set_mode term mode;
      Deferred.all_unit [drawer; LTerm.sync term]
      >>= fun () ->
      (match result with
       | Ok    _ -> self#draw_success
       | Error _ -> self#draw_failure)
      >>| fun () ->
      match result with
      | Ok    x -> x
      | Error e -> Res.Error e
  end
