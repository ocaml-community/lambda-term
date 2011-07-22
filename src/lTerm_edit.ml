(*
 * lTerm_edit.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Zed_edit
open LTerm_key
open LTerm_geom
open Lwt_react

(* +-----------------------------------------------------------------+
   | Bindings                                                        |
   +-----------------------------------------------------------------+ *)

let bindings = Hashtbl.create 128

let () =
  let ( --> ) key action = Hashtbl.add bindings key action in
  { control = false; meta = false; shift = false; code = Left } --> Prev_char;
  { control = false; meta = false; shift = false; code = Right } --> Next_char;
  { control = false; meta = false; shift = false; code = Up } --> Prev_line;
  { control = false; meta = false; shift = false; code = Down } --> Next_line;
  { control = false; meta = false; shift = false; code = Home } --> Goto_bol;
  { control = false; meta = false; shift = false; code = End } --> Goto_eol;
  { control = false; meta = false; shift = false; code = Insert } --> Switch_erase_mode;
  { control = false; meta = false; shift = false; code = Delete } --> Delete_next_char;
  { control = false; meta = false; shift = false; code = Enter } --> Newline;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char ' ') } --> Set_mark;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'a') } --> Goto_bol;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'e') } --> Goto_eol;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'd') } --> Delete_next_char;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'k') } --> Kill_next_line;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'u') } --> Kill_prev_line;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'n') } --> Prev_char;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'p') } --> Next_char;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'w') } --> Kill;
  { control = true; meta = false; shift = false; code = Char(UChar.of_char 'y') } --> Yank;
  { control = false; meta = false; shift = false; code = Backspace } --> Delete_prev_char;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'w') } --> Copy;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'c') } --> Capitalize_word;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'l') } --> Lowercase_word;
  { control = false; meta = true; shift = false; code = Char(UChar.of_char 'u') } --> Uppercase_word;
  { control = false; meta = true; shift = false; code = Right } --> Next_word;
  { control = false; meta = true; shift = false; code = Left } --> Prev_word;
  { control = true; meta = false; shift = false; code = Right } --> Next_word;
  { control = true; meta = false; shift = false; code = Left } --> Prev_word

(* +-----------------------------------------------------------------+
   | Widgets                                                         |
   +-----------------------------------------------------------------+ *)

let clipboard =
  let x = ref Zed_rope.empty in
  { Zed_edit.clipboard_get = (fun () -> !x);
    Zed_edit.clipboard_set = (fun r -> x := r) }

let regexp_word =
  let set = UCharInfo.load_property_set `Alphabetic in
  let set = List.fold_left (fun set ch -> USet.add (UChar.of_char ch) set) set ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
  Zed_re.compile (`Repn(`Set set, 1, None))

let dummy_engine = Zed_edit.create ()
let dummy_cursor = Zed_edit.new_cursor dummy_engine
let dummy_context = Zed_edit.context dummy_engine dummy_cursor
let newline = UChar.of_char '\n'

class edit ?(clipboard = clipboard) () =
  let locale, set_locale = S.create None in
object(self)
  inherit LTerm_widget.t "edit"

  method can_focus = true

  val mutable engine = dummy_engine
  method engine = engine

  val mutable cursor = dummy_cursor
  method cursor = cursor

  val mutable context = dummy_context
  method context = context

  method text = Zed_rope.to_string (Zed_edit.text engine)

  val mutable style = LTerm_style.none
  val mutable marked_style = LTerm_style.none
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    marked_style <- LTerm_resources.get_style (rc ^ ".marked") resources

  method editable pos len = true
  method move pos delta = pos + delta
  method match_word text pos = match_by_regexp regexp_word text pos
  method locale = S.value locale
  method set_locale locale = set_locale locale

  val mutable event = E.never

  initializer
    engine <- (
      Zed_edit.create
        ~editable:(fun pos len -> self#editable pos len)
        ~move:(fun pos delta -> self#move pos delta)
        ~match_word:(fun text pos -> self#match_word text pos)
        ~clipboard
        ~locale
        ()
    );
    cursor <- Zed_edit.new_cursor engine;
    context <- Zed_edit.context engine cursor;
    Zed_edit.set_data engine (self :> edit);
    event <- (
      E.map (fun _ -> self#queue_draw)
        (E.select [
           E.stamp (Zed_edit.changes engine) ();
           E.stamp (S.changes (Zed_edit.selection engine)) ();
           E.stamp (S.changes (Zed_cursor.position cursor)) ();
           E.stamp (S.changes (Zed_cursor.position (Zed_edit.mark engine))) ();
         ])
    );
    self#on_event
      (function
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Char ch } ->
             Zed_edit.insert context (Zed_rope.singleton ch);
             true
         | LTerm_event.Key key -> begin
             match try Some (Hashtbl.find bindings key) with Not_found -> None with
               | Some action ->
                   Zed_edit.get_action action context;
                   true
               | None ->
                   false
           end
         | _ ->
             false)

  val mutable shift = 0
  val mutable start = 0

  method draw ctx focused =
    let open LTerm_draw in

    let size = LTerm_draw.size ctx in

    (*** Check that the cursor is displayed ***)

    let line_set = Zed_edit.lines engine in
    let cursor_offset = Zed_cursor.get_position cursor in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column = cursor_offset - Zed_lines.line_start line_set cursor_line in

    (* Horizontal check *)
    if cursor_column < shift || cursor_column >= shift + size.cols then
      shift <- max 0 (cursor_column - size.cols / 2);

    (* Vertical check *)
    let start_line = Zed_lines.line_index line_set start in
    let start_line =
      if cursor_line < start_line || cursor_line >= start_line + size.rows then begin
        let start_line = max 0 (cursor_line - size.rows / 2) in
        start <- Zed_lines.line_start line_set start_line;
        start_line
      end else
        start_line
    in

    (*** Drawing ***)

    (* Initialises points with the text style and spaces. *)
    fill ctx (UChar.of_char ' ');
    fill_style ctx style;

    (*** Text drawing ***)

    let rec draw_line row col zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then begin
          let row = row + 1 in
          if row < size.rows then begin_line row zip
        end else begin
          if col > size.cols then begin
            let row = row + 1 in
            if row < size.rows then skip_eol row zip
          end else begin
            draw_char ctx row col char;
            draw_line row (col + 1) zip
          end
        end

    and skip_eol row zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then
          begin_line row zip
        else
          skip_eol row zip

    and skip_bol row zip remaining =
      if remaining = 0 then
        draw_line row 0 zip
      else if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then begin
          let row = row + 1 in
          if row < size.rows then begin_line row zip
        end else
          skip_bol row zip (remaining - 1)

    and begin_line row zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi row
      else if shift <> 0 then begin
        skip_bol row zip shift
      end else
        draw_line row 0 zip

    and draw_eoi row =
      ()
    in

    let text = Zed_edit.text engine in

    (* Compute the end of the displayed text. *)
(*    let end_offset =
      if start_line + size.rows > Zed_lines.count line_set then
        Zed_rope.length text
      else
        Zed_lines.line_start line_set (start_line + size.rows)
    in
*)
    begin_line 0 (Zed_rope.Zip.make_f text start);

    (* Colorize the current line. *)
    (*      let line = m.(tl_line + cursor_line - start_line) in
            let style_current_line = Peps_var.get ~scope:area.edit Peps_edit.style_current_line in
            for column = 1 to area.columns do
            set_style line.(tl_column + column) style_current_line
            done;
    *)

    (* Colorize the selection if needed *)
    if Zed_edit.get_selection engine then begin
      let sel_offset = Zed_cursor.get_position (Zed_edit.mark engine) in
      let sel_line = Zed_lines.line_index line_set sel_offset in
      let sel_column = sel_offset - Zed_lines.line_start line_set sel_line in
      let line_a, column_a, line_b, column_b =
        if sel_offset < cursor_offset then
          (sel_line, sel_column, cursor_line, cursor_column)
        else
          (cursor_line, cursor_column, sel_line, sel_column)
      in
      let line_a, column_a =
        if line_a < start_line then
          (start_line, 0)
        else
          (line_a, column_a)
      in
      let line_b, column_b =
        if line_b >= start_line + size.rows then
          (start_line + size.rows - 1, size.cols - 1)
        else
          (line_b, column_b)
      in
      if line_a < start_line + size.rows && line_b >= start_line then begin
        let line_a = line_a - start_line and line_b = line_b - start_line in
        let column_a = column_a and column_b = column_b in
        if line_a = line_b then
          for column = column_a to column_b do
            set_style (point ctx line_a column) marked_style
          done
        else begin
          for column = column_a to size.cols - 1 do
            set_style (point ctx line_a column) marked_style
          done;
          for line = line_a + 1 to line_b - 1 do
            for column = 0 to size.cols - 1 do
              set_style (point ctx line column) marked_style
            done
          done;
          for column = 0 to column_b do
            set_style (point ctx line_b column) marked_style
          done
        end
      end
    end

  method cursor_position =
    let line_set = Zed_edit.lines engine in
    let cursor_offset = Zed_cursor.get_position cursor in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column = cursor_offset - Zed_lines.line_start line_set cursor_line in
    let start_line = Zed_lines.line_index line_set start in
    Some { row = cursor_line - start_line; col = cursor_column - shift }
end
