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

type string_set = Set.Make(String).t
type completion = unit Zed_edit.context -> string_set Lwt.t
type completion_mode = [ `Classic | `Real_time | `None ]

module String_set = Set.Make(String)

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
  let words = String_set.filter (fun word' -> Zed_utf8.starts_with word' word) words in
  if String_set.is_empty words then
    ("", String_set.empty)
  else
    (String_set.fold common_prefix words (String_set.choose words), words)

let complete ?(suffix=" ") ctx word words =
  let prefix, words = lookup word words in
  if String_set.is_empty words then
    words
  else begin
    (* Insert the end of the longest prefix. *)
    Zed_edit.insert ctx (Zed_rope.of_string (String.sub prefix (String.length word) (String.length prefix - String.length word)));
    (* Insert the suffix if there is only one completion. *)
    if String_set.cardinal words = 1 then Zed_edit.insert ctx (Zed_rope.of_string suffix);
    words
  end

let print_words term words =
  match List.filter ((<>) "") words with
    | [] ->
        return ()
    | words ->
        let max_width = List.fold_left (fun x word -> max x (Zed_utf8.length word)) 0 words + 1 in
        let count = List.length words in
        lwt { columns = screen_width } = Lt_term.get_size term in
        let columns = max 1 (screen_width / max_width) in
        let lines =
          if count < columns then
            1
          else
            let l = count / columns in
            if columns mod count = 0 then l else l + 1
        in
        let column_width = screen_width / columns in
        let m = Array.make_matrix lines columns "" in
        let rec fill_display line column = function
          | [] ->
              ()
          | word :: words ->
              m.(line).(column) <- word;
              let line = line + 1 in
              if line < lines then
                fill_display line column words
              else
                fill_display 0 (column + 1) words
        in
        fill_display 0 0 words;
        for_lwt line = 0 to lines - 1 do
          lwt () =
            for_lwt column = 0 to columns - 1 do
              let word = m.(line).(column) in
              lwt () = Lt_term.fprint term word in
              let len = Zed_utf8.length word in
              if len < column_width then
                Lt_term.fprint term (String.make (column_width - len) ' ')
              else
                return ()
            done
          in
          Lt_term.fprint term "\n"
        done

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
  | History_first
  | History_last
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

class virtual ['a] engine ?(history: string list=[]) () =
  let edit : unit Zed_edit.t = Zed_edit.create () in
  let context = Zed_edit.context edit (Zed_edit.new_cursor edit) in
object(self)
  method virtual eval : 'a
  method edit = edit
  method context = context

  (* The completion thread. *)
  val mutable completion = return ()

  method send_action action =
    (* Cancel completion if the user requested another action. *)
    cancel completion;
    match action with
      | Edit action ->
          Zed_edit.get_action action context
      | Interrupt_or_delete_next_char ->
          if Zed_rope.is_empty (Zed_edit.text edit) then
            raise Interrupt
          else
            Zed_edit.delete_next_char context
      | Complete
      | Complete_bar_next
      | Complete_bar_prev
      | Complete_bar_first
      | Complete_bar_last
      | Complete_bar
      | History_prev
      | History_next
      | History_first
      | History_last
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
end

(* +-----------------------------------------------------------------+
   | Predefined classes                                              |
   +-----------------------------------------------------------------+ *)

class read_line ?history () = object(self)
  inherit [Zed_utf8.t] engine ?history ()
  method eval = Zed_rope.to_string (Zed_edit.text self#edit)
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let default_prompt = [String "# "]

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

class virtual ['a] term term =
  let size, set_size = S.create { columns = 80; lines = 25 } in
object(self)
  inherit ['a] abstract
  method size = size

  val mutable prompt = S.const default_prompt
    (* The signal holding the prompt. *)

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
    (* THe position of the end of displayed material. *)

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

  method draw =
    if draw_queued then
      return ()
    else begin
      (* Wait a bit in order not to draw too often. *)
      draw_queued <- true;
      lwt () = pause () in
      draw_queued <- false;

      if visible then begin
        let before, after = self#stylise in
        let before = List.concat [S.value prompt; [Reset]; before] in
        let total = List.concat [before; after; [Reset]] in
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

  method last_draw =
    lwt () = if visible then self#erase else return () in
    let before, after = self#stylise in
    let before = List.concat [S.value prompt; [Reset]; before] in
    let total = List.concat [before; after; [Reset]] in
    Lt_term.fprintls term total

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
      self#draw
    end else
      return ()

  method run =
    (* Get the initial size of the terminal. *)
    lwt initial_size = Lt_term.get_size term in
    set_size initial_size;

    (* Redraw everything when needed. *)
    let id =
      Lwt_event.notify_s
        (fun () -> self#draw)
        (E.select [
           E.stamp (S.changes size) ();
           E.stamp (Zed_edit.changes self#edit) ();
           E.stamp (S.changes (Zed_cursor.position (Zed_edit.cursor self#context))) ();      E.stamp (S.changes prompt) ();
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
                  lwt () = self#last_draw in
                  return self#eval
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

    try_lwt
      lwt () = self#draw in
      loop ()
    finally
      Lwt_event.disable id;
      return ()
end

(* +-----------------------------------------------------------------+
   | High-level functions                                            |
   +-----------------------------------------------------------------+ *)

let read_line ?(term=Lt_term.stdout) ?history ?complete ?clipboard ?mode ?(prompt=default_prompt) () =
  let prompt_signal = S.const prompt in
  let rl = object
    inherit read_line ?history ()
    inherit [Zed_utf8.t] term term
    initializer
      prompt <- prompt_signal
  end in
  rl#run
