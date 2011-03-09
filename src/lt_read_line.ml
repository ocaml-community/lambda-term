
(*
 * lt_read_line.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open React
open Lwt
open Lt_types
open Lt_style
open Lt_key

exception Interrupt
type prompt = Lt_style.text

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let common_prefix a b =
  let rec loop ofs =
    if ofs = String.length a || ofs = String.length b then
      String.sub a 0 ofs
    else
      let ch1, ofs1 = Zed_utf8.unsafe_extract_next a ofs
      and ch2, ofs2 = Zed_utf8.unsafe_extract_next b ofs in
      if ch1 = ch2 && ofs1 = ofs2 then
        loop ofs1
      else
        String.sub a 0 ofs
  in
  loop 0

let lookup word words =
  match List.filter (fun word' -> Zed_utf8.starts_with word' word) words with
    | [] ->
        ("", [])
    | (word :: rest) as words ->
        (List.fold_left common_prefix word rest, words)

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

type history = string list

let add_entry line history =
  if Zed_utf8.strip line = "" then
    history
  else
    if (match history with [] -> false | x :: _ -> x = line) then
      history
    else
      line :: history

let newline = UChar.of_char '\n'
let backslash = UChar.of_char '\\'
let letter_n = UChar.of_char 'n'

let escape line =
  let buf = Buffer.create (String.length line) in
  Zed_utf8.iter
    (fun ch ->
       if ch = newline then
         Buffer.add_string buf "\\\n"
       else if ch =  backslash then
         Buffer.add_string buf "\\\\"
       else
         Buffer.add_string buf (Zed_utf8.singleton ch))
    line;
  Buffer.contents buf

let unescape line =
  let buf = Buffer.create (String.length line) in
  let rec loop ofs =
    if ofs = String.length line then
      Buffer.contents buf
    else begin
      let ch, ofs = Zed_utf8.unsafe_extract_next line ofs in
      if ch = backslash then begin
        if ofs = String.length line then begin
          Buffer.add_char buf '\\';
          Buffer.contents buf
        end else begin
          let ch, ofs = Zed_utf8.unsafe_extract_next line ofs in
          if ch = backslash then
            Buffer.add_char buf '\\'
          else if ch = letter_n then
            Buffer.add_char buf '\n'
          else begin
            Buffer.add_char buf '\\';
            Buffer.add_string buf (Zed_utf8.singleton ch)
          end;
          loop ofs
        end
      end else begin
        Buffer.add_string buf (Zed_utf8.singleton ch);
        loop ofs
      end
    end
  in
  loop 0

let rec load_lines ic acc =
  Lwt_io.read_line_opt ic >>= function
    | Some l ->
        ignore (Zed_utf8.validate l);
        load_lines ic (unescape l :: acc)
    | None ->
        return acc

let load_history name =
  if Sys.file_exists name then
    Lwt_io.with_file ~mode:Lwt_io.input name (fun ic -> load_lines ic [])
  else
    return []

let rec merge h1 h2 =
  match h1, h2 with
    | l1 :: h1, l2 :: h2 when l1 = l2 ->
        l1 :: merge h1 h2
    | _ ->
        h1 @ h2

let save_history name history =
  lwt on_disk_history = load_history name in
  Lwt_io.lines_to_file name (Lwt_stream.map escape (Lwt_stream.of_list (merge (List.rev on_disk_history) (List.rev history))))

(* +-----------------------------------------------------------------+
   | Actions                                                         |
   +-----------------------------------------------------------------+ *)

type action =
  | Edit of Zed_edit.action
  | Interrupt_or_delete_next_char
  | Complete
  | Complete_bar_next
  | Complete_bar_prev
  | Complete_bar_first
  | Complete_bar_last
  | Complete_bar
  | History_prev
  | History_next
  | Accept
  | Clear_screen
  | Prev_search

let bindings = Hashtbl.create 128

let () =
  let ( --> ) key action = Hashtbl.add bindings key action in
  { control = false; meta = false; shift = false; code = Up } --> History_prev;
  { control = false; meta = false; shift = false; code = Down } --> History_next;
  { control = false; meta = false; shift = false; code = Tab } --> Complete;
  { control = false; meta = false; shift = false; code = Enter } --> Accept;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'm') } --> Accept;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'l') } --> Clear_screen;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'r') } --> Prev_search;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'd') } --> Interrupt_or_delete_next_char;
  { control = false; meta = true; shift = false; code = Left } --> Complete_bar_prev;
  { control = false; meta = true; shift = false; code = Right } --> Complete_bar_next;
  { control = false; meta = true; shift = false; code = Home } --> Complete_bar_first;
  { control = false; meta = true; shift = false; code = End } --> Complete_bar_last;
  { control = false; meta = true; shift = false; code = Tab } --> Complete_bar

let bind key =
  try
    Some(Hashtbl.find bindings key)
  with Not_found ->
    try
      Some(Edit(Hashtbl.find Lt_edit.bindings key))
    with Not_found ->
      None

(* +-----------------------------------------------------------------+
   | The read-line engine                                            |
   +-----------------------------------------------------------------+ *)

let styled_of_rope rope =
  Zed_rope.rev_fold_leaf (fun leaf acc -> String leaf :: acc) rope []

class virtual ['a] engine ?(history=[]) () =
  let edit : unit Zed_edit.t = Zed_edit.create () in
  let context = Zed_edit.context edit (Zed_edit.new_cursor edit) in
  let completion_words, set_completion_words = S.create ([] : (Zed_utf8.t * Zed_utf8.t) list) in
  let completion_index, set_completion_index = S.create 0 in
object(self)
  method virtual eval : 'a
  method edit = edit
  method context = context

  (* The history before the history cursor. *)
  val mutable history_prev = history

  (* The history after the history cursor. *)
  val mutable history_next = []

  (* The thread that compute completion. *)
  val mutable completion_thread = return ()

  (* The event that compute completion when needed. *)
  val mutable completion_event = E.never

  (* Whether a completion has been queued. *)
  val mutable completion_queued = false

  (* The index of the start of the word being completed. *)
  val mutable completion_start = 0

  initializer
    completion_event <- (
      E.map
        (fun () ->
           (* Cancel previous thread because it is now useless. *)
           cancel completion_thread;
           set_completion_index 0;
           set_completion_words [];
           if completion_queued then
             return ()
           else begin
             completion_queued <- true;
             lwt () = pause () in
             completion_queued <- false;
             completion_thread <- (
               lwt start, comp = self#complete in
               completion_start <- start;
               set_completion_words comp;
               return ()
             );
             return ()
           end)
        (E.select [
           E.stamp (Zed_edit.changes edit) ();
           E.stamp (S.changes (Zed_cursor.position (Zed_edit.cursor context))) ();
         ])
    );
    completion_thread <- (
      lwt start, comp = self#complete in
      completion_start <- start;
      set_completion_words comp;
      return ()
    )

  method input_prev =
    Zed_rope.before (Zed_edit.text edit) (Zed_edit.position context)

  method input_next =
    Zed_rope.after (Zed_edit.text edit) (Zed_edit.position context)

  method completion_words = completion_words
  method completion_index = completion_index
  method complete = return (0, [])

  method send_action action =
    match action with
      | Edit action ->
          Zed_edit.get_action action context
      | Interrupt_or_delete_next_char ->
          if Zed_rope.is_empty (Zed_edit.text edit) then
            raise Interrupt
          else
            Zed_edit.delete_next_char context
      | Complete -> begin
          let prefix_length = Zed_edit.position context - completion_start in
          match S.value completion_words with
            | [] ->
                ()
            | [(completion, suffix)] ->
                Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
                Zed_edit.insert context (Zed_rope.of_string suffix)
            | (completion, suffix) :: rest ->
                let word = List.fold_left (fun acc (word, _) -> common_prefix acc word) completion rest in
                Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after word prefix_length))
        end
      | Complete_bar_next ->
          let index = S.value completion_index in
          if index < List.length (S.value completion_words) - 1 then
            set_completion_index (index + 1)
      | Complete_bar_prev ->
          let index = S.value completion_index in
          if index > 0 then
            set_completion_index (index - 1)
      | Complete_bar_first ->
          set_completion_index 0
      | Complete_bar_last ->
          let len = List.length (S.value completion_words) in
          if len > 0 then
            set_completion_index (len - 1)
      | Complete_bar ->
          let words = S.value completion_words in
          if words <> [] then begin
            let prefix_length = Zed_edit.position context - completion_start in
            let completion, suffix = List.nth words (S.value completion_index) in
            Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
            Zed_edit.insert context (Zed_rope.of_string suffix)
          end
      | History_prev -> begin
          match history_prev with
            | [] ->
                ()
            | line :: rest ->
                let text = Zed_edit.text edit in
                history_prev <- rest;
                history_next <- Zed_rope.to_string text :: history_next;
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length text);
                Zed_edit.insert context (Zed_rope.of_string line)
        end
      | History_next -> begin
          match history_next with
            | [] ->
                ()
            | line :: rest ->
                let text = Zed_edit.text edit in
                history_prev <- Zed_rope.to_string text :: history_prev;
                history_next <- rest;
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length text);
                Zed_edit.insert context (Zed_rope.of_string line)
        end
      | Accept
      | Clear_screen
      | Prev_search ->
          ()

  method stylise =
    let before, after = Zed_rope.break (Zed_edit.text edit) (Zed_cursor.get_position (Zed_edit.cursor context)) in
    (styled_of_rope before, styled_of_rope after)
end

class virtual ['a] abstract = object
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

(* +-----------------------------------------------------------------+
   | Predefined classes                                              |
   +-----------------------------------------------------------------+ *)

class virtual read_line ?history () = object(self)
  inherit [Zed_utf8.t] engine ?history ()
  method eval = Zed_rope.to_string (Zed_edit.text self#edit)
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let default_prompt = [String "# "]

let rec drop count l =
  if count <= 0 then
    l
  else match l with
    | [] -> []
    | e :: l -> drop (count - 1) l

(* Computes the position of the cursor after printing the given styled
   string. *)
let rec compute_position size pos = function
  | [] ->
      pos
  | String str :: rest ->
      let pos =
        Zed_utf8.fold
          (fun ch pos ->
             if ch = newline then
               { line = pos.line + 1; column = 0 }
             else if pos.column = size.columns then
               { line = pos.line + 1; column = 1 }
             else
               { pos with column = pos.column + 1 })
          str pos
      in
      compute_position size pos rest
  | _ :: rest ->
      compute_position size pos rest

let make_completion_bar_middle index columns words =
  let rec aux ofs idx = function
    | [] ->
        [String(String.make (columns - ofs) ' ')]
    | (word, suffix) :: words ->
        let len = Zed_utf8.length word in
        let ofs' = ofs + len in
        if ofs' <= columns then
          if idx = index then
            Inverse :: String word :: Reset ::
              if ofs' + 1 > columns then
                []
              else
                String "│" :: aux (ofs' + 1) (idx + 1) words
          else
            String word ::
              if ofs' + 1 > columns then
                []
              else
                String "│" :: aux (ofs' + 1) (idx + 1) words
        else
          [String(Zed_utf8.sub word 0 (columns - ofs))]
  in
  aux 0 0 words

let make_bar delimiter columns words =
  let buf = Buffer.create (columns * 3) in
  let rec aux ofs = function
    | [] ->
        for i = ofs + 1 to columns do
          Buffer.add_string buf "─"
        done;
        Buffer.contents buf
    | (word, suffix) :: words ->
        let len = Zed_utf8.length word in
        let ofs' = ofs + len in
        if ofs' <= columns then begin
          for i = 1 to len do
            Buffer.add_string buf "─"
          done;
          if ofs' + 1 > columns then
            Buffer.contents buf
          else begin
            Buffer.add_string buf delimiter;
            aux (ofs' + 1) words
          end
        end else begin
          for i = ofs + 1 to columns do
            Buffer.add_string buf "─"
          done;
          Buffer.contents buf
        end
  in
  aux 0 words

let rec get_index_of_last_displayed_word column columns index words =
  match words with
    | [] ->
        index - 1
    | (word, suffix) :: words ->
        let column = column + Zed_utf8.length word in
        if column <= columns - 1 then
          get_index_of_last_displayed_word (column + 1) columns (index + 1) words
        else
          index - 1

class virtual ['a] term term =
  let size, set_size = S.create { columns = 80; lines = 25 } in
  let event, set_prompt = E.create () in
  let prompt = S.switch (S.const default_prompt) event in
object(self)
  inherit ['a] abstract
  method size = size
  method prompt = prompt
  method set_prompt = set_prompt

  val mutable visible = true
    (* Whether the read-line instance is currently visible. *)

  val mutable displayed = false
    (* Whether the read-line instance is currently displayed on the
       screen. *)

  val mutable draw_queued = false
    (* Whether a draw operation has been queued, in which case it is
       not necessary to redraw. *)

  val mutable cursor = { line = 0; column = 0 }
    (* The position of the cursor. *)

  val mutable end_of_display = { line = 0; column = 0 }
    (* The position of the end of displayed material. *)

  val mutable completion_start = S.const 0
    (* Index of the first displayed word in the completion bar. *)

  initializer
    completion_start <- (
      S.fold
        (fun start (words, index, columns) ->
           if index < start then
             (* The cursor is before the left margin. *)
             let count = List.length words in
             let rev_index = count - index - 1 in
             count - get_index_of_last_displayed_word 1 columns rev_index (drop rev_index (List.rev words)) - 1
           else if index > get_index_of_last_displayed_word 1 columns start (drop start words) then
             (* The cursor is after the right margin. *)
             index
           else
             start)
        0
        (S.changes
           (S.l3
              (fun words index size -> (words, index, size.columns))
              self#completion_words
              self#completion_index
              size))
    )

  method completion_start = completion_start

  method private erase =
    (* Move back to the beginning of printed material. *)
    lwt () = Lt_term.move term (-cursor.line) (-cursor.column) in
    (* Erase everything, line by line. *)
    let rec erase count =
      if count = 0 then
        Lt_term.clear_line term
      else
        lwt () = Lt_term.clear_line term in
        lwt () = Lt_term.move term 1 0 in
        erase (count - 1)
    in
    lwt () = erase end_of_display.line in
    (* Move back again to the beginning. *)
    Lt_term.move term (-end_of_display.line) 0

  method draw_update =
    if draw_queued then
      return ()
    else begin
      (* Wait a bit in order not to draw too often. *)
      draw_queued <- true;
      lwt () = pause () in
      draw_queued <- false;

      if visible then begin
        let { columns } = S.value size in
        let start = S.value self#completion_start in
        let index = S.value self#completion_index in
        let words = drop start (S.value self#completion_words) in
        let before, after = self#stylise in
        let before = List.concat [S.value prompt; [Reset]; before] in
        let after = List.concat [
          after; [Reset];
          (* The completion bar. *)
          [String "\n┌"; String(make_bar "┬" (columns - 2) words); String "┐\n│"];
          make_completion_bar_middle (index - start) (columns - 2) words;
          [String "│\n└"; String(make_bar "┴" (columns - 2) words); String "┘\n"]
        ] in
        let total = before @ after in
        let size = S.value size in
        lwt () =
          if displayed then
            self#erase
          else begin
            displayed <- true;
            return ()
          end
        in
        cursor <- compute_position size { column = 0; line = 0 } before;
        end_of_display <- compute_position size cursor after;
        lwt () = Lt_term.fprints term total in
        lwt () = Lt_term.move term (cursor.line - end_of_display.line) (cursor.column - end_of_display.column) in
        Lt_term.flush term
      end else
        return ()
    end

  method draw_simple =
    let before, after = self#stylise in
    let before = List.concat [S.value prompt; [Reset]; before] in
    let total = List.concat [before; after; [Reset]] in
    Lt_term.fprintls term total

  method draw_accept =
    self#draw_simple

  method hide =
    if visible then begin
      visible <- false;
      self#erase
    end else
      return ()

  method show =
    if not visible then begin
      visible <- true;
      displayed <- false;
      self#draw_update
    end else
      return ()

  method run =
    (* Get the initial size of the terminal. *)
    lwt initial_size = Lt_term.get_size term in
    set_size initial_size;

    (* Redraw everything when needed. *)
    let event =
      Lwt_event.map_p
        (fun () -> self#draw_update)
        (E.select [
           E.stamp (S.changes size) ();
           E.stamp (Zed_edit.changes self#edit) ();
           E.stamp (S.changes (Zed_cursor.position (Zed_edit.cursor self#context))) ();
           E.stamp (S.changes prompt) ();
           E.stamp (S.changes self#completion_words) ();
           E.stamp (S.changes self#completion_index) ();
           E.stamp (S.changes self#completion_start) ();
         ])
    in

    (* The main loop. *)
    let rec loop () =
      Lt_term.read_event term >>= function
        | Lt_event.Resize size ->
            set_size size;
            loop ()
        | Lt_event.Key key -> begin
            match bind key with
              | Some Accept ->
                  return self#eval
              | Some Clear_screen ->
                  lwt () = Lt_term.clear_screen term in
                  lwt () = Lt_term.goto term { line = 0; column = 0 } in
                  displayed <- false;
                  lwt () = self#draw_update in
                  loop ()
              | Some action ->
                  self#send_action action;
                  loop ()
              | None ->
                  match key with
                    | { control = false; meta = false; shift = false; code = Char ch } ->
                        Zed_edit.insert self#context (Zed_rope.singleton ch);
                        loop ()
                    | _ ->
                        loop ()
          end
        | _ ->
            loop ()
    in

    lwt result =
      try_lwt
        lwt () = self#draw_update in
        loop ()
      with exn ->
        E.stop event;
        lwt () = if visible then self#erase else return () in
        lwt () = self#draw_simple in
        raise_lwt exn
    in
    E.stop event;
    lwt () = if visible then self#erase else return () in
    lwt () = self#draw_accept in
    return result
end
