(*
 * lTerm_widget.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open LTerm_geom
open LTerm_draw
open LTerm_key
open LTerm_style
open LTerm_text
open LTerm_widget_callbacks

let return, (>>=) = Lwt.return, Lwt.(>>=)

(* +-----------------------------------------------------------------+
   | The widget class                                                |
   +-----------------------------------------------------------------+ *)

class t = LTerm_widget_base_impl.t

(* +-----------------------------------------------------------------+
   | Labels                                                          |
   +-----------------------------------------------------------------+ *)

let newline = UChar.of_char '\n'

let text_size str =
  let rec loop ofs rows cols max_cols =
    if ofs = String.length str then
      { rows; cols = max cols max_cols }
    else
      let chr, ofs = Zed_utf8.unsafe_extract_next str ofs in
      if chr = newline then
        if ofs = String.length str then
          { rows; cols = max cols max_cols }
        else
          loop ofs (rows + 1) 0 (max cols max_cols)
      else
        loop ofs rows (cols + 1) max_cols
  in
  loop 0 1 0 0

class label initial_text = object(self)
  inherit t "label"
  val mutable text = initial_text

  val mutable size_request = text_size initial_text
  method size_request = size_request

  val mutable style = LTerm_style.none
  method update_resources =
    style <- LTerm_resources.get_style self#resource_class self#resources

  method text = text
  method set_text t =
    text <- t;
    size_request <- text_size t;
    self#queue_draw

  method draw ctx focused =
    let { rows } = LTerm_draw.size ctx in
    let row = (rows - size_request.rows) / 2 in
    LTerm_draw.fill_style ctx style;
    LTerm_draw.draw_string_aligned ctx row H_align_center text
end

(* +-----------------------------------------------------------------+
   | Boxes                                                           |
   +-----------------------------------------------------------------+ *)

exception Out_of_range = LTerm_containers_impl.Out_of_range
class type box = LTerm_containers_impl.box
class hbox = LTerm_containers_impl.hbox
class vbox = LTerm_containers_impl.vbox
class frame = LTerm_containers_impl.frame
class modal_frame = LTerm_containers_impl.modal_frame

(* +-----------------------------------------------------------------+
   | Spacing for layout control (aka glue)                           |
   +-----------------------------------------------------------------+ *)

class spacing ?(rows=0) ?(cols=0) () = object
  inherit t "glue"
  val size_request = { rows; cols }
  method size_request = size_request
end

(* +-----------------------------------------------------------------+
   | Lines                                                           |
   +-----------------------------------------------------------------+ *)

class hline = object(self)
  inherit t "hline"

  val size_request = { rows = 1; cols = 0 }
  method size_request = size_request

  val mutable style = LTerm_style.none
  val mutable connection = LTerm_draw.Light
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    connection <- LTerm_resources.get_connection (rc ^ ".connection") resources

  method draw ctx focused =
    let { rows } = LTerm_draw.size ctx in
    LTerm_draw.fill_style ctx style;
    draw_hline ctx (rows / 2) 0 (LTerm_draw.size ctx).cols connection
end

class vline = object(self)
  inherit t "vline"

  val size_request = { rows = 0; cols = 1 }
  method size_request = size_request

  val mutable style = LTerm_style.none
  val mutable connection = LTerm_draw.Light
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    connection <- LTerm_resources.get_connection (rc ^ ".connection") resources

  method draw ctx focused =
    let { cols } = LTerm_draw.size ctx in
    LTerm_draw.fill_style ctx style;
    draw_vline ctx 0 (cols / 2) (LTerm_draw.size ctx).rows connection
end

(* +-----------------------------------------------------------------+
   | Buttons                                                         |
   +-----------------------------------------------------------------+ *)

class button = LTerm_buttons_impl.button
class checkbutton = LTerm_buttons_impl.checkbutton
class type ['a] radio = ['a] LTerm_buttons_impl.radio
class ['a] radiogroup = ['a] LTerm_buttons_impl.radiogroup
class ['a] radiobutton = ['a] LTerm_buttons_impl.radiobutton

(* +-----------------------------------------------------------------+
   | Scrollbars                                                      |
   +-----------------------------------------------------------------+ *)

class type adjustment = LTerm_scroll_impl.adjustment

class type scrollable_adjustment = LTerm_scroll_impl.scrollable_adjustment

class default_scrollable_adjustment = LTerm_scroll_impl.default_scrollable_adjustment

class vscrollbar_for_adjustment = LTerm_scroll_impl.vscrollbar_for_adjustment

class hscrollbar_for_adjustment = LTerm_scroll_impl.hscrollbar_for_adjustment

class vscrollbar = LTerm_scroll_impl.vscrollbar

class hscrollbar = LTerm_scroll_impl.hscrollbar

class type scrollable_document = object
  method set_document_size : LTerm_geom.size -> unit
  method set_page_size : LTerm_geom.size -> unit
  method vscroll : scrollable_adjustment
  method hscroll : scrollable_adjustment
end

class default_scrollable_document = object(self)
 
  val mutable document_size = { rows=0; cols=0 }
  val mutable page_size = { rows=0; cols=0 }

  val vscroll = new default_scrollable_adjustment
  val hscroll = new default_scrollable_adjustment

  method private update = 
    let range = max 0 (document_size.rows-page_size.rows+1) in
    vscroll#set_range range;
    vscroll#set_mouse_mode `auto;
    vscroll#set_scroll_bar_mode (`dynamic page_size.rows);
    let range = max 0 (document_size.cols-page_size.cols+1) in
    hscroll#set_range range;
    hscroll#set_mouse_mode `auto;
    hscroll#set_scroll_bar_mode (`dynamic page_size.cols)

  method set_document_size size = 
    document_size <- size;
    self#update

  method set_page_size size = 
    page_size <- size;
    self#update

  method vscroll = vscroll
  method hscroll = hscroll

end

class vscrollbar_for_document ?width (doc : #scrollable_document) = object(self)
  inherit vscrollbar_for_adjustment ?width doc#vscroll as super
end

class hscrollbar_for_document ?height (doc : #scrollable_document) = object(self)
  inherit hscrollbar_for_adjustment ?height doc#hscroll as super
end

class vslider rng = object(self)
  inherit vscrollbar ~rc:"slider" ~width:1 ()
  initializer
    self#set_mouse_mode `middle;
    self#set_scroll_bar_mode (`fixed 1);
    self#set_range (max 0 rng)
  method size_request = { rows=rng; cols=1 }
end

class hslider rng = object(self)
  inherit hscrollbar ~rc:"slider" ~height:1 ()
  initializer
    self#set_mouse_mode `middle;
    self#set_scroll_bar_mode (`fixed 1);
    self#set_range (max 0 rng)
  method size_request = { rows=1; cols=rng }
end

(* +-----------------------------------------------------------------+
   | Running in a terminal                                           |
   +-----------------------------------------------------------------+ *)

let run = LTerm_running_impl.run
let run_modal = LTerm_running_impl.run_modal
let prepare_simple_run = LTerm_running_impl.prepare_simple_run
