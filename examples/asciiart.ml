(*let palette = "@#8&o:*. "*)
let palette = " .*:o&8#@"
let coefs = [| 0.229; 0.587; 0.114 |]

let load file = 
  let img = OImages.load file [] in
  match OImages.tag img with
  | OImages.Index8 img 
  | OImages.Index16 img -> img#to_rgb24
  | OImages.Rgb24 img -> img
  | _ -> failwith "not supported"

let avg_cols = ref 5
let avg_rows = ref 10
let filename = ref "test.png"

let () = Arg.(parse [
  "-cols", Set_int avg_cols, "num cols to average";
  "-rows", Set_int avg_rows, "num rows to average";
] (fun s -> filename := s) "asciiart [options] filename")

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

class asciiart img = object(self)
  inherit t "asciiart"

  method can_focus = false

  method full_size = { rows = Array.length img - 1; cols=Array.length img.(0) - 1 }

  val mutable offset = { row = 0; col = 0 }
  method offset = offset
  method set_offset o = offset <- o; self#queue_draw

  val style = 
    LTerm_style.({ none with foreground=Some white; 
                             background=Some black }) 

  method draw ctx focused = 
    let { rows; cols } = LTerm_draw.size ctx in
    for row=0 to rows-1 do
      for col=0 to cols-1 do
        LTerm_draw.draw_char ~style ctx row col @@ 
          UChar.of_char palette.[ try img.(row+offset.row).(col+offset.col) with _ -> 0 ]
      done
    done

end

let main () = 
  let img = indices (load !filename) in

  let waiter, wakener = wait () in
  let exit = new button "exit" in
  exit#on_click (wakeup wakener);

  let vbox = new vbox in
  let hbox = new hbox in
  let art = new asciiart img in
  let vscroll = new vscrollbar (art :> scrollable) in
  let hscroll = new hscrollbar (art :> scrollable) in

  art#set_focus { art#focus with right = Some(vscroll); down = Some(hscroll :> t) };
  vscroll#set_focus { vscroll#focus with down = Some(hscroll :> t) };
  hscroll#set_focus { hscroll#focus with up = Some(vscroll); down = Some(exit :> t) } ;


  hbox#add art;
  hbox#add ~expand:false vscroll;
  vbox#add hbox;
  vbox#add ~expand:false hscroll;
  vbox#add ~expand:false exit;

  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

