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

  method can_focus = false

  (* scroll interface *)
  method document_size = { 
    rows = img#height / !avg_rows;
    cols = img#width / !avg_cols;
  }

  method page_size = size_of_rect self#allocation

  val mutable voffset = 0
  val mutable hoffset = 0
  method set_voffset o = voffset <- o
  method set_hoffset o = hoffset <- o

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
            try img.(row + voffset).(col + hoffset) with _ -> 0 
          ]
      done
    done

end

(* place vertical and horizontal scroll bars around the picture *)
let with_scrollbar ?down widget = 
  let vbox = new vbox in
  let hbox = new hbox in
  (* make scroll bars roughly the same size *)
  let vscroll = new vscrollbar_for_document ~width:3 widget in
  let hscroll = new hscrollbar_for_document ~height:2 widget in
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
  widget#set_focus { widget#focus with right = Some(vscroll :> t); 
                                       down = Some(hscroll :> t) };
  vscroll#set_focus { vscroll#focus with down = Some(hscroll :> t) };
  hscroll#set_focus { hscroll#focus with up = Some(vscroll :> t); down } ;
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
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 'w' ->
      avg_rows := max 1 (!avg_rows - 1); top#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 's' ->
      avg_rows := !avg_rows + 1; top#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 'a' ->
      avg_cols := max 1 (!avg_cols - 1); top#queue_draw; true
    | LTerm_event.Key{LTerm_key.code=LTerm_key.Char c} when c = UChar.of_char 'd' ->
      avg_cols := !avg_cols + 1; top#queue_draw; true
    | _ -> false);

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term top waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

