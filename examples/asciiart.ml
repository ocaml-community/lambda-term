(*
 * asciiart.ml
 * -----------
 * Copyright : (c) 2016, Andy Ray <andy.ray@ujamjar.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* ascii chars of increasing intensity *)
let palette = " .*:o&8#@"

(* grayscale conversion coefficients *)
let coefs = [| 0.229; 0.587; 0.114 |]

(* load image *)
let load file = 
  let img = OImages.load file [] in
  match OImages.tag img with
  | OImages.Index8 img 
  | OImages.Index16 img -> img#to_rgb24
  | OImages.Rgb24 img -> img
  | _ -> failwith "not supported"

(* images will be scaled down by averaging pixels in blocks of this size *)
let avg_cols = ref 5
let avg_rows = ref 10

let filename = ref "test.png"

let () = Arg.(parse [
  "-cols", Set_int avg_cols, "num cols to average";
  "-rows", Set_int avg_rows, "num rows to average";
] (fun s -> filename := s) "asciiart [options] filename")

(* scale image and convert to indices into palette *)
let indices img = 
  let rows = img#height in
  let cols = img#width in
  let avg = float_of_int (!avg_rows * !avg_cols) in
  let luma r g b = 
    ((float_of_int r *. coefs.(0)) +.
     (float_of_int g *. coefs.(1)) +.
     (float_of_int b *. coefs.(2)))
  in
  Array.init (rows / !avg_rows) (fun row ->
    Array.init (cols / !avg_cols) (fun col ->
      let sum = ref 0. in
      for row=row * !avg_rows to ((row+1) * !avg_rows)-1 do
        for col=col * !avg_cols to ((col+1) * !avg_cols)-1 do
          let pel = img#get col row in
          sum := !sum +. (luma pel.Images.r pel.Images.g pel.Images.b)
        done
      done;
      let sum = !sum /. (256. *. avg) in
      max 0 @@ min 8 (int_of_float (sum *. 9.))))

open Lwt
open LTerm_widget
open LTerm_geom
open CamomileLibrary

(* scrollable asciiart widget *)
class asciiart img = object(self)
  inherit t "asciiart" as super

  method can_focus = true

  (* scrollable interfaces *)
  val vscroll = new scrollable
  val hscroll = new scrollable
  method vscroll = vscroll
  method hscroll = hscroll

  method document_size = { 
    rows = img#height / !avg_rows;
    cols = img#width / !avg_cols;
  }

  initializer
    vscroll#set_document_size self#document_size.rows;
    hscroll#set_document_size self#document_size.cols

  method set_allocation r = 
    super#set_allocation r;
    let size = size_of_rect r in
    vscroll#set_page_size size.rows;
    hscroll#set_page_size size.cols

  val style = 
    LTerm_style.({ none with foreground=Some white; 
                             background=Some black }) 

  (* buffer the image - reconvert when the scale changes *)
  val mutable stored_img : (int * int * (int array array)) option = None
  method img = 
    match stored_img with
    | Some(r, c, i) when r = !avg_rows && c = !avg_cols -> i
    | _ -> stored_img <- Some(!avg_rows, !avg_cols, indices img); self#img

  method draw ctx focused = 
    let { rows; cols } = LTerm_draw.size ctx in
    let img = self#img in
    for row=0 to rows-1 do
      for col=0 to cols-1 do
        LTerm_draw.draw_char ~style ctx row col @@ 
          UChar.of_char palette.[ 
            try img.(row + vscroll#offset).(col + hscroll#offset) with _ -> 0 
          ]
      done
    done

  (* delta from center of screen *)
  method private mouse_delta_event ev = 
    let open LTerm_mouse in
    match ev with
    | LTerm_event.Mouse m when m.button=Button1 && m.control=true ->
      let alloc = self#allocation in
      let size = size_of_rect alloc in
      vscroll#set_offset 
        (vscroll#offset + m.LTerm_mouse.row - alloc.row1 - size.rows/2);
      hscroll#set_offset 
        (hscroll#offset + m.LTerm_mouse.col - alloc.col1 - size.cols/2);
      true
    | _ -> false

  (* adjust scale, which changes the document size *)
  method private scale_event = function
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 'w' ->
      avg_rows := max 1 (!avg_rows - 1);
      vscroll#set_document_size self#document_size.rows;
      self#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 's' ->
      avg_rows := !avg_rows + 1; 
      vscroll#set_document_size self#document_size.rows;
      self#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 'a' ->
      avg_cols := max 1 (!avg_cols - 1); 
      hscroll#set_document_size self#document_size.cols;
      self#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 'd' ->
      avg_cols := !avg_cols + 1; 
      hscroll#set_document_size self#document_size.cols;
      self#queue_draw; true
    | _ -> false

  (* page up/down *)
  method page_event = function
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Next_page} ->
      vscroll#set_offset @@ vscroll#page_next; self#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Prev_page} ->
      vscroll#set_offset @@ vscroll#page_prev; self#queue_draw; true
    | _ -> false

  initializer 
    self#on_event (fun ev -> self#scale_event ev || 
                             self#page_event ev || 
                             self#mouse_delta_event ev)

end

(* place vertical and horizontal scroll bars around the picture *)
let with_scrollbar ?down widget = 
  let vbox = new vbox in
  let hbox = new hbox in
  (* make scroll bars roughly the same size *)
  let vscroll = new vscrollbar ~width:3 widget#vscroll in
  let hscroll = new hscrollbar ~height:2 widget#hscroll in
  let spacing = new spacing ~rows:2 ~cols:3 () in
  hbox#add widget;
  hbox#add ~expand:false (new vline);
  hbox#add ~expand:false vscroll;
  vbox#add hbox;
  vbox#add ~expand:false (new hline);
  let hbox = new hbox in
  hbox#add hscroll;
  hbox#add ~expand:false (new vline);
  hbox#add ~expand:false spacing;
  vbox#add ~expand:false hbox;
  (* moving focus *)
  widget#set_focus { widget#focus with right = Some(vscroll :> t); 
                                       down = Some(hscroll :> t) };
  vscroll#set_focus { vscroll#focus with down = Some(hscroll :> t) };
  hscroll#set_focus { hscroll#focus with up = Some(vscroll :> t); down };
  (* events *)
  widget#on_event (fun ev -> vscroll#mouse_event ev && hscroll#mouse_event ev);
  vscroll#on_event widget#page_event;
  vbox

let main () = 
  let img = load !filename in

  let waiter, wakener = wait () in
  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let vbox = with_scrollbar ~down:(exit :> t) (new asciiart img) in
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false exit;

  let top = new frame in
  top#set vbox;

  top#on_event (function (* quit with escape key *)
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Escape} -> 
      wakeup wakener (); false 
    | _ -> false);

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term top waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

