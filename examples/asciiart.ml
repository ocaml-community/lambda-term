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
class asciiart img vscroll hscroll = object(self)
  inherit t "asciiart" as super

  (*initializer
    vscroll#set_range (Array.length img);
    hscroll#set_range (Array.length img.(0))*)

  method can_focus = false

  method set_allocation rect =
    let size = size_of_rect rect in
    vscroll#set_range (max 1 (Array.length img - size.rows));
    hscroll#set_range (max 1 (Array.length img.(0) - size.cols));
    super#set_allocation rect

  val style = 
    LTerm_style.({ none with foreground=Some white; 
                             background=Some black }) 

  method draw ctx focused = 
    let { rows; cols } = LTerm_draw.size ctx in
    for row=0 to rows-1 do
      for col=0 to cols-1 do
        LTerm_draw.draw_char ~style ctx row col @@ 
          UChar.of_char palette.[ 
            try img.(row + vscroll#offset).(col + hscroll#offset) with _ -> 0 
          ]
      done
    done

end

let with_scrollbar ?down widget = 
  let vbox = new vbox in
  let hbox = new hbox in
  let vscroll = (* make scroll bars roughly the same size *)
    new vscrollbar ~size_request:{rows=0;cols=3} ~default_scroll_bar_size:5 () 
  in
  let hscroll = 
    new hscrollbar ~size_request:{rows=2;cols=0} ~default_scroll_bar_size:10 () 
  in
  let spacing = new spacing ~rows:2 ~cols:3 () in
  let widget = widget vscroll hscroll in
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
  let img = indices (load !filename) in

  let waiter, wakener = wait () in
  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let vbox = with_scrollbar ~down:(exit :> t) (new asciiart img) in
  vbox#add ~expand:false (new hline);
  vbox#add ~expand:false exit;

  let top = new frame in
  top#set vbox;

  top#on_event (function (* quit with escape key *)
    LTerm_event.Key{LTerm_key.code=LTerm_key.Escape} -> 
      wakeup wakener (); false | _ -> false);

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term top waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

