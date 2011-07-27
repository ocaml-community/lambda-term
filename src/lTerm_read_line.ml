(*
 * lTerm_read_line.ml
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open Lwt
open LTerm_geom
open LTerm_style
open LTerm_text
open LTerm_key

exception Interrupt
type prompt = LTerm_text.t

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let common_prefix_one a b =
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

let common_prefix = function
  | [] -> ""
  | word :: rest -> List.fold_left common_prefix_one word rest

let lookup word words = List.filter (fun word' -> Zed_utf8.starts_with word' word) words
let lookup_assoc word words = List.filter (fun (word', x) -> Zed_utf8.starts_with word' word) words

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
  | Cancel_search

let bindings = Hashtbl.create 128

let () =
  let ( --> ) key action = Hashtbl.add bindings key action in
  { control = false; meta = false; shift = false; code = Home } --> Edit Zed_edit.Goto_bot;
  { control = false; meta = false; shift = false; code = End } --> Edit Zed_edit.Goto_eot;
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
  { control = false; meta = true; shift = false; code = Tab } --> Complete_bar;
  { control = false; meta = true; shift = false; code = Enter } --> Edit Zed_edit.Newline;
  { control = false; meta = false; shift = false; code = Escape } --> Cancel_search

let bind key =
  try
    Some(Hashtbl.find bindings key)
  with Not_found ->
    try
      Some(Edit(Hashtbl.find LTerm_edit.bindings key))
    with Not_found ->
      None

(* +-----------------------------------------------------------------+
   | The read-line engine                                            |
   +-----------------------------------------------------------------+ *)

let search_string str sub =
  let rec equal_at a b =
    (b = String.length sub) || (String.unsafe_get str a = String.unsafe_get sub b) && equal_at (a + 1) (b + 1)
  in
  let rec loop ofs idx =
    if ofs + String.length sub > String.length str then
      None
    else
      if equal_at ofs 0 then
        Some idx
      else
        loop (Zed_utf8.unsafe_next str ofs) (idx + 1)
  in
  loop 0 0

class virtual ['a] engine ?(history=[]) () =
  let edit : unit Zed_edit.t = Zed_edit.create () in
  let context = Zed_edit.context edit (Zed_edit.new_cursor edit) in
  let completion_words, set_completion_words = S.create ([] : (Zed_utf8.t * Zed_utf8.t) list) in
  let completion_index, set_completion_index = S.create 0 in
  let search_mode, set_search_mode = S.create false in
  let history, set_history = S.create (history, []) in
  let message, set_message = S.create None in
object(self)
  method virtual eval : 'a
  method edit = edit
  method context = context
  method show_box = true
  method search_mode = search_mode
  method history = history
  method message = message

  (* Whether a completion has been queued. *)
  val mutable completion_queued = false

  (* The event which occurs when completion need to be recomputed. *)
  val mutable completion_event = E.never

  (* The index of the start of the word being completed. *)
  val mutable completion_start = 0

  method set_completion start words =
    completion_start <- start;
    set_completion_words words

  initializer
    completion_event <- (
      E.map
        (fun () ->
           self#set_completion 0 [];
           self#completion)
        (E.when_
           (S.map not search_mode)
           (E.select [
              E.stamp (Zed_edit.changes edit) ();
              E.stamp (S.changes (Zed_cursor.position (Zed_edit.cursor context))) ();
            ]))
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
    let prefix_length = Zed_edit.position context - completion_start in
    match S.value completion_words with
      | [] ->
          ()
      | [(completion, suffix)] ->
          Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
          Zed_edit.insert context (Zed_rope.of_string suffix)
      | (completion, suffix) :: rest ->
          let word = List.fold_left (fun acc (word, _) -> common_prefix_one acc word) completion rest in
          Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after word prefix_length))

  (* The event which search for the string in the history. *)
  val mutable search_event = E.never

  (* The result of the search. If the search was successful it
     contains the matched history entry, the position of the substring
     in this entry and the rest of the history. *)
  val mutable search_result = None

  initializer
    search_event <- E.map (fun _ -> search_result <- None; self#search) (E.when_ search_mode (Zed_edit.changes edit))

  method private search =
    let input = Zed_rope.to_string (Zed_edit.text edit) in
    let rec loop = function
      | [] ->
          search_result <- None;
          set_message (Some(LTerm_text.of_string "Reverse search: not found"))
      | entry :: rest ->
          match search_string entry input with
            | Some pos -> begin
                match search_result with
                  | Some(entry', _, _) when entry = entry' ->
                      loop rest
                  | _ ->
                      search_result <- Some(entry, pos, rest);
                      let txt = LTerm_text.of_string entry in
                      for i = pos to pos + Zed_rope.length (Zed_edit.text edit) - 1 do
                        let ch, style = txt.(i) in
                        txt.(i) <- (ch, { style with underline = Some true })
                      done;
                      set_message (Some(Array.append (LTerm_text.of_string "Reverse search: ") txt))
              end
            | None ->
                loop rest
    in
    match search_result with
      | Some(entry, pos, rest) -> loop rest
      | None -> loop (fst (S.value history))

  method insert ch =
    Zed_edit.insert context (Zed_rope.singleton ch)

  method send_action action =
    match action with
      | (Complete | Complete_bar) when S.value search_mode -> begin
          set_search_mode false;
          set_message None;
          match search_result with
            | Some(entry, pos, rest) ->
                search_result <- None;
                Zed_edit.goto context 0;
                Zed_edit.remove context (Zed_rope.length (Zed_edit.text edit));
                Zed_edit.insert context (Zed_rope.of_string entry)
            | None ->
                ()
        end

      | Edit action ->
          Zed_edit.get_action action context

      | Interrupt_or_delete_next_char ->
          if Zed_rope.is_empty (Zed_edit.text edit) then
            raise Interrupt
          else
            Zed_edit.delete_next_char context

      | Complete when not (S.value search_mode) ->
          self#complete

      | Complete_bar_next when not (S.value search_mode) ->
          let index = S.value completion_index in
          if index < List.length (S.value completion_words) - 1 then
            set_completion_index (index + 1)

      | Complete_bar_prev when not (S.value search_mode) ->
          let index = S.value completion_index in
          if index > 0 then
            set_completion_index (index - 1)

      | Complete_bar_first when not (S.value search_mode) ->
          set_completion_index 0

      | Complete_bar_last when not (S.value search_mode) ->
          let len = List.length (S.value completion_words) in
          if len > 0 then
            set_completion_index (len - 1)

      | Complete_bar when not (S.value search_mode) ->
          let words = S.value completion_words in
          if words <> [] then begin
            let prefix_length = Zed_edit.position context - completion_start in
            let completion, suffix = List.nth words (S.value completion_index) in
            Zed_edit.insert context (Zed_rope.of_string (Zed_utf8.after completion prefix_length));
            Zed_edit.insert context (Zed_rope.of_string suffix)
          end

      | History_prev when not (S.value search_mode) ->begin
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

      | History_next when not (S.value search_mode) -> begin
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

      | Prev_search ->
          if S.value search_mode then
            self#search
          else begin
            let text = Zed_edit.text edit in
            Zed_edit.goto context 0;
            Zed_edit.remove context (Zed_rope.length text);
            let prev, next = S.value history in
            set_history (Zed_rope.to_string text :: (List.rev_append next prev), []);
            search_result <- None;
            set_search_mode true;
            self#search
          end

      | Cancel_search ->
          if S.value search_mode then begin
            set_search_mode false;
            set_message None
          end

      | _ ->
          ()

  method stylise =
    let txt = LTerm_text.of_rope (Zed_edit.text edit) in
    let pos = Zed_edit.position context in
    if Zed_edit.get_selection edit then begin
      let mark = Zed_cursor.get_position (Zed_edit.mark edit) in
      let a = min pos mark and b = max pos mark in
      for i = a to b - 1 do
        let ch, style = txt.(i) in
        txt.(i) <- (ch, { style with underline = Some true })
      done;
    end;
    (txt, pos)
end

class virtual ['a] abstract = object
  method virtual eval : 'a
  method virtual send_action : action -> unit
  method virtual insert : UChar.t -> unit
  method virtual edit : unit Zed_edit.t
  method virtual context : unit Zed_edit.context
  method virtual stylise : LTerm_text.t * int
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

(* +-----------------------------------------------------------------+
   | Predefined classes                                              |
   +-----------------------------------------------------------------+ *)

class read_line ?history () = object(self)
  inherit [Zed_utf8.t] engine ?history ()
  method eval = Zed_rope.to_string (Zed_edit.text self#edit)
end

class read_password () = object(self)
  inherit [Zed_utf8.t] engine () as super

  method stylise =
    let text, pos = super#stylise in
    for i = 0 to Array.length text - 1 do
      let ch, style = text.(i) in
      text.(i) <- (UChar.of_char '*', style)
    done;
    (text, pos)

  method eval = Zed_rope.to_string (Zed_edit.text self#edit)

  method show_box = false

  method send_action = function
    | Prev_search -> ()
    | action -> super#send_action action
end

type 'a read_keyword_result =
  | Rk_value of 'a
  | Rk_error of Zed_utf8.t

class ['a] read_keyword ?history () = object(self)
  inherit ['a read_keyword_result] engine ?history ()

  method keywords = []

  method eval =
    let input = Zed_rope.to_string (Zed_edit.text self#edit) in
    try Rk_value(List.assoc input self#keywords) with Not_found -> Rk_error input

  method completion =
    let word = Zed_rope.to_string self#input_prev in
    let keywords = List.filter (fun (keyword, value) -> Zed_utf8.starts_with keyword word) self#keywords in
    self#set_completion 0 (List.map (fun (keyword, value) -> (keyword, "")) keywords)
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let vline = LTerm_draw.({ top = Light; bottom = Light; left = Blank; right = Blank })
let reverse_style = { LTerm_style.none with LTerm_style.reverse = Some true }
let default_prompt = LTerm_text.of_string "# "

let rec drop count l =
  if count <= 0 then
    l
  else match l with
    | [] -> []
    | e :: l -> drop (count - 1) l

(* Computes the position of the cursor after printing the given styled
   string. *)
let rec compute_position cols pos text start stop =
  if start = stop then
    pos
  else
    let ch, style = text.(start) in
    if ch = newline then
      compute_position cols { row = pos.row + 1; col = 0 } text (start + 1) stop
    else if pos.col + 1 = cols then
      compute_position cols { row = pos.row + 1; col = 0 } text (start + 1) stop
    else
      compute_position cols { pos with col = pos.col + 1 } text (start + 1) stop

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

let draw_styled ctx row col str =
  let size = LTerm_draw.size ctx in
  let rec loop row col idx =
    if idx < Array.length str then begin
      let ch, style = Array.unsafe_get str idx in
      if ch = newline then
        loop (row + 1) 0 (idx + 1)
      else begin
        let point = LTerm_draw.point ctx row col in
        point.LTerm_draw.char <- ch;
        LTerm_draw.set_style point style;
        let col = col + 1 in
        if col = size.cols then
          loop (row + 1) 0 (idx + 1)
        else
          loop row col (idx + 1)
      end
    end
  in
  loop row col 0

class virtual ['a] term term =
  let size, set_size = S.create { cols = 80; rows = 25 } in
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

  val mutable cursor = { row = 0; col = 0 }
    (* The position of the cursor. *)

  val mutable completion_start = S.const 0
    (* Index of the first displayed word in the completion bar. *)

  val mutable height = 0
    (* The height of the displayed material. *)

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
              (fun words index size -> (words, index, size.cols))
              self#completion_words
              self#completion_index
              size))
    )

  method completion_start = completion_start

  val draw_mutex = Lwt_mutex.create ()

  method private queue_draw_update =
    if draw_queued then
      return ()
    else begin
      (* Wait a bit in order not to draw too often. *)
      draw_queued <- true;
      lwt () = pause () in
      draw_queued <- false;
      Lwt_mutex.with_lock draw_mutex (fun () -> self#draw_update)
    end

  method draw_update =
    let size = S.value size in
    if visible && size.cols > 0 then begin
      let styled, position = self#stylise in
      let prompt = S.value prompt in
      (* Compute the position of the cursor after displaying the
         prompt. *)
      let pos_after_prompt = compute_position size.cols { row = 0; col = 0 } prompt 0 (Array.length prompt) in
      (* Compute the position of the cursor after displaying the
         input before the cursor. *)
      let pos_after_before = compute_position size.cols pos_after_prompt styled 0 position in
      (* Compute the position of the cursor after displaying the
         input. *)
      let pos_after_styled = compute_position size.cols pos_after_before styled position (Array.length styled) in
      let matrix =
        if self#show_box && size.cols > 2 then
          (* Compute the position of the cursor after displaying the
             newline to separate the input and the box. *)
          let pos_after_newline = compute_position size.cols pos_after_styled [|(newline, LTerm_style.none)|] 0 1 in
          match S.value self#message with
            | Some msg ->
                (* Compute the height of the message. *)
                let message_height = (compute_position (size.cols - 2) { row = 0; col = 0 } msg 0 (Array.length msg)).row + 1 in
                (* The total height of the displayed text. *)
                let total_height = pos_after_newline.row + message_height + 2 in

                (* Create the matrix for the rendering. *)
                let matrix_size = { size with rows = if displayed then max total_height height else total_height } in
                let matrix = LTerm_draw.make_matrix matrix_size in
                let ctx = LTerm_draw.context matrix matrix_size in

                (* Update the height parameter. *)
                height <- total_height;

                (* Draw the prompt and the input. *)
                draw_styled ctx 0 0 prompt;
                draw_styled ctx pos_after_prompt.row pos_after_prompt.col styled;

                (* Draw a frame for the message. *)
                LTerm_draw.draw_frame ctx {
                  row1 = pos_after_newline.row;
                  col1 = 0;
                  row2 = total_height;
                  col2 = size.cols;
                } LTerm_draw.Light;

                (* Draw the message. *)
                let ctx = LTerm_draw.sub ctx {
                  row1 = pos_after_newline.row + 1;
                  col1 = 1;
                  row2 = total_height - 1;
                  col2 = size.cols - 1;
                } in
                draw_styled ctx 0 0 msg;

                matrix

            | None ->
                let comp_start = S.value self#completion_start in
                let comp_index = S.value self#completion_index in
                let comp_words = drop comp_start (S.value self#completion_words) in

                (* The total height of the displayed text. *)
                let total_height = pos_after_newline.row + 3 in

                (* Create the matrix for the rendering. *)
                let matrix_size = { size with rows = if displayed then max total_height height else total_height } in
                let matrix = LTerm_draw.make_matrix matrix_size in
                let ctx = LTerm_draw.context matrix matrix_size in

                (* Update the height parameter. *)
                height <- total_height;

                (* Draw the prompt and the input. *)
                draw_styled ctx 0 0 prompt;
                draw_styled ctx pos_after_prompt.row pos_after_prompt.col styled;

                (* Draw a frame for the completion. *)
                LTerm_draw.draw_frame ctx {
                  row1 = pos_after_newline.row;
                  col1 = 0;
                  row2 = total_height;
                  col2 = size.cols;
                } LTerm_draw.Light;

                (* Draw the completion. *)
                let ctx = LTerm_draw.sub ctx {
                  row1 = pos_after_newline.row + 1;
                  col1 = 1;
                  row2 = total_height - 1;
                  col2 = size.cols - 1;
                } in

                let rec loop idx col = function
                  | [] ->
                      ()
                  | (word, suffix) :: words ->
                      let len = Zed_utf8.length word in
                      LTerm_draw.draw_string ctx 0 col word;
                      (* Apply the reverse style if this is the selected word. *)
                      if idx = comp_index then
                        for col = col to min (col + len - 1) (size.cols - 2) do
                          LTerm_draw.set_style (LTerm_draw.point ctx 0 col) reverse_style
                        done;
                      (* Draw a separator. *)
                      LTerm_draw.draw_piece ctx 0 (col + len) vline;
                      let col = col + len + 1 in
                      if col < size.cols - 2 then loop (idx + 1) col words
                in
                loop comp_start 0 comp_words;

                matrix

        else begin
          let total_height = pos_after_styled.row + 1 in
          let matrix_size = { size with rows = if displayed then max total_height height else total_height } in
          let matrix = LTerm_draw.make_matrix matrix_size in
          let ctx = LTerm_draw.context matrix matrix_size in
          height <- total_height;
          draw_styled ctx 0 0 prompt;
          draw_styled ctx pos_after_prompt.row pos_after_prompt.col styled;
          matrix
        end
      in
      lwt () =
        if displayed then
          (* Go back to the beginning of displayed text. *)
          LTerm.move term (-cursor.row) (-cursor.col)
        else
          return ()
      in
      (* Display everything. *)
      lwt () = LTerm.print_box term matrix in
      (* Update the cursor. *)
      cursor <- pos_after_before;
      (* Move the cursor to the right position. *)
      lwt () =
        if LTerm.windows term then
          (* On windows the cursor is not moved. *)
          LTerm.move term cursor.row cursor.col
        else
          (* On Unix the cursor stay at the end of line. We put it
             back to the beginning of the line immediatly because all
             terminals do not respond the same way when the cursor is
             at the end of line. *)
          lwt () = LTerm.fprint term "\r" in
          LTerm.move term (cursor.row - Array.length matrix + 1) cursor.col
      in
      lwt () = LTerm.flush term in
      displayed <- true;
      return ()
    end else
      return ()

  method draw_success =
    let size = S.value size in
    let styled, position = self#stylise in
    let prompt = S.value prompt in
    let pos_after_prompt = compute_position size.cols { row = 0; col = 0 } prompt 0 (Array.length prompt) in
    let pos_after_before = compute_position size.cols pos_after_prompt styled 0 position in
    let pos_after_styled = compute_position size.cols pos_after_before styled position (Array.length styled) in
    let total_height = pos_after_styled.row + 1 in
    let matrix_size = { size with rows = if displayed then max total_height height else total_height } in
    let matrix = LTerm_draw.make_matrix matrix_size in
    let ctx = LTerm_draw.context matrix matrix_size in
    draw_styled ctx 0 0 prompt;
    draw_styled ctx pos_after_prompt.row pos_after_prompt.col styled;
    lwt () = if displayed then LTerm.move term (-cursor.row) (-cursor.col) else return () in
    lwt () = LTerm.print_box term matrix in
    if LTerm.windows term then
      LTerm.move term total_height 0
    else
      lwt () = LTerm.fprint term "\r" in
      LTerm.move term (total_height - Array.length matrix + 1) 0

  method draw_failure =
    self#draw_success

  method hide =
    if visible then begin
      visible <- false;
      Lwt_mutex.with_lock draw_mutex (fun () ->
                                        if displayed then
                                          let matrix_size = { S.value size with rows = height } in
                                          let matrix = LTerm_draw.make_matrix matrix_size in
                                          lwt () = LTerm.move term (-cursor.row) (-cursor.col) in
                                          lwt () = LTerm.print_box term matrix in
                                          lwt () =
                                            if LTerm.windows term then
                                              return ()
                                            else
                                              lwt () = LTerm.fprint term "\r" in
                                              LTerm.move term (1 - Array.length matrix) 0
                                          in
                                          cursor <- { row = 0; col = 0 };
                                          height <- 0;
                                          displayed <- false;
                                          return ()
                                        else
                                          return ())
    end else
      return ()

  method show =
    if not visible then begin
      visible <- true;
      self#queue_draw_update
    end else
      return ()

  method run =
    (* Get the initial size of the terminal. *)
    lwt initial_size = LTerm.get_size term in
    set_size initial_size;

    let running = ref true in

    (* Redraw everything when needed. *)
    let event =
      E.map_p
        (fun () -> if !running then self#queue_draw_update else return ())
        (E.select [
           E.stamp (S.changes size) ();
           Zed_edit.update self#edit [Zed_edit.cursor self#context];
           E.stamp (S.changes prompt) ();
           E.stamp (S.changes self#completion_words) ();
           E.stamp (S.changes self#completion_index) ();
           E.stamp (S.changes self#completion_start) ();
           E.stamp (S.changes self#message) ();
         ])
    in

    (* The main loop. *)
    let rec loop () =
      LTerm.read_event term >>= function
        | LTerm_event.Resize size ->
            set_size size;
            loop ()
        | LTerm_event.Key key -> begin
            match bind key with
              | Some Accept ->
                  return self#eval
              | Some Clear_screen ->
                  lwt () = LTerm.clear_screen term in
                  lwt () = LTerm.goto term { row = 0; col = 0 } in
                  displayed <- false;
                  lwt () = self#queue_draw_update in
                  loop ()
              | Some action ->
                  self#send_action action;
                  loop ()
              | None ->
                  match key with
                    | { control = false; meta = false; shift = false; code = Char ch } ->
                        self#insert ch;
                        loop ()
                    | _ ->
                        loop ()
          end
        | _ ->
            loop ()
    in

    lwt mode =
      match LTerm.is_a_tty term with
        | true ->
            lwt mode = LTerm.enter_raw_mode term in
            return (Some mode)
        | false ->
            return None
    in

    lwt result =
      try_lwt
        (* Go to the beginning of line otherwise all offset
           calculation will be false. *)
        lwt () = LTerm.fprint term "\r" in
        lwt () = self#queue_draw_update in
        loop ()
      with exn ->
        running := false;
        E.stop event;
        lwt () = Lwt_mutex.with_lock draw_mutex (fun () -> self#draw_failure) in
        raise_lwt exn
      finally
        match mode with
          | Some mode ->
              LTerm.leave_raw_mode term mode
          | None ->
              return ()
    in
    running := false;
    E.stop event;
    lwt () = Lwt_mutex.with_lock draw_mutex (fun () -> self#draw_success) in
    return result
end
