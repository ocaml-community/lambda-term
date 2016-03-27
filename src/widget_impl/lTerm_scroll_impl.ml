open CamomileLibrary
open LTerm_geom

class t = LTerm_widget_base_impl.t

module Proj = struct

  let create range1 range2 = 
    assert (range1 >= range2);
    let range1' = float_of_int range1 in
    let range2' = float_of_int (range2-1) in
    Array.init range2 
      (fun i ->
        let i' = float_of_int i in
        max 0 (min range1 
          (int_of_float @@ ((range1' *. i' /. range2') +. 0.5))))
  
  let scale_up t offset = 
    t.(max 0 (min offset (Array.length t - 1)))

  let scale_down a value = 
    let (<:) x y = scale_up a x < y in
    let (>:) x y = scale_up a x > y in
    let (-:) x y = scale_up a x - y in
    (* binary search nearest *)
    let rec search low high =
      if high = low then low 
      else if high = low + 1 then
        if abs (low -: value) < abs (high -: value) then low 
        else high
      else 
        let mid = (low + high) / 2 in
        if mid >: value then search low mid
        else if mid <: value then search mid high
        else mid 
    in
    let len = Array.length a in
    if value < 0 then 0
    else if (len-1) <: value then len-1
    else search 0 (len - 1)

  let make () = 
    let range1', range2' = ref 0, ref 0 in
    let proj = ref [||] in
    let memo_create range1 range2 = 
      if !range1' = range1 && !range2' = range2 then !proj
      else begin
        proj := create range1 range2;
        range1' := range1;
        range2' := range2;
        !proj
      end
    in
    let scale_to range1 range2 = 
      if range1 >= range2 then scale_up (memo_create range1 range2)
      else scale_down (memo_create range2 range1)
    in
    let scale_from range1 range2 = 
      if range1 >= range2 then scale_down (memo_create range1 range2)
      else scale_up (memo_create range2 range1)
    in
    scale_to, scale_from

end

let hbar = 0x2550
let vbar = 0x2551
let lbar = 0x2560
let rbar = 0x2563
let tbar = 0x2566
let bbar = 0x2569
let xbar = 0x256c

class type scrollable = object

  method full_size : size
    (* full size of scrollable window *)

  method offset : coord
    (* offset within window *)

  method set_offset : coord -> unit
    (* set offset within window *)

end

class virtual scrollbar ~scroll_bar_size ~(scrollable:#scrollable) =
  let window_of_scroll, scroll_of_window = Proj.make () in

  object(self)
    inherit t "scrollbar"

    method can_focus = true

    method virtual private dim_of_coord : coord -> int
    method virtual private dim_of_size : size -> int
    method virtual private dim_of_rect : rect -> int
    method virtual private mouse_offset : LTerm_mouse.t -> rect -> int
    method virtual private update_dim : coord -> int -> coord
    method virtual private key_scroll_incr : LTerm_key.code
    method virtual private key_scroll_decr : LTerm_key.code

    val mutable focused_style = LTerm_style.none
    val mutable unfocused_style = LTerm_style.none
    method update_resources =
      let rc = self#resource_class and resources = self#resources in
      focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
      unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources

    method private window_size = self#dim_of_size scrollable#full_size 
    method private scroll_steps = self#dim_of_rect self#allocation - scroll_bar_size + 1 

    val mutable scroll_bar_offset = 0

    method private window_of_scroll offset = 
      scroll_bar_offset <- max 0 (min (self#scroll_steps - 1) offset);
      window_of_scroll self#window_size self#scroll_steps offset

    method private scroll_of_window offset = 
      let o = scroll_of_window self#window_size self#scroll_steps offset in
      scroll_bar_offset <- max 0 (min (self#scroll_steps - 1) o);
      o

    initializer self#on_event @@ fun ev ->
      let open LTerm_mouse in
      let open LTerm_key in

      let alloc = self#allocation in
      let prev () = self#window_of_scroll (scroll_bar_offset - 1) in
      let next () = self#window_of_scroll (scroll_bar_offset + 1) in
      let to_ofs x = self#update_dim scrollable#offset x in

      match ev with
      | LTerm_event.Mouse m when m.button=Button1 && in_rect alloc (coord m) ->
        let scroll = self#mouse_offset m alloc in
        scrollable#set_offset @@ to_ofs @@ self#window_of_scroll scroll;
        self#queue_draw;
        true

      | LTerm_event.Key { control = false; meta = false; shift = true; code } 
        when code=self#key_scroll_decr ->
        scrollable#set_offset @@ to_ofs (prev ());
        self#queue_draw;
        true

      | LTerm_event.Key { control = false; meta = false; shift = true; code } 
        when code=self#key_scroll_incr ->
        scrollable#set_offset @@ to_ofs (next ());
        self#queue_draw;
        true

      | _ -> false

  end

class vscrollbar
  ?(scroll_bar_size=5) 
  ?(scroll_window_size=0) 
  ?(scroll_bar_thickness=2) 
  ~scrollable = object(self)
  inherit scrollbar ~scroll_bar_size ~scrollable 

  method size_request = { rows=scroll_window_size; cols=scroll_bar_thickness }

  method private dim_of_coord = row
  method private dim_of_size = rows
  method private dim_of_rect r = r.row2 - r.row1
  method private mouse_offset m alloc = m.LTerm_mouse.row - alloc.row1 
  method private update_dim c row = { c with LTerm_geom.row }
  method private key_scroll_incr = LTerm_key.Down
  method private key_scroll_decr = LTerm_key.Up

  (* ...should try and refactor this into the base class... *)
  method draw ctx focused = 
    let focus = (self :> t) = focused in
    let { cols; _ } = LTerm_draw.size ctx in

    let style = if focus then focused_style else unfocused_style in
    LTerm_draw.fill_style ctx style;

    let offset = self#scroll_of_window @@ self#dim_of_coord scrollable#offset in

    for col=0 to cols-1 do
      let r1 = offset in
      let r2 = offset + scroll_bar_size-1 in
      for row=r1 to r2 do
        if col=0 || col=cols-1 then
          if row=r1 && row=r2 then
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int xbar
          else if row=r1 then
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int tbar
          else if row=r2 then
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int bbar
          else
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int vbar
        else if row=r1 || row=r2 then
          LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int hbar
      done
    done

end

class hscrollbar
  ?(scroll_bar_size=5) 
  ?(scroll_window_size=0) 
  ?(scroll_bar_thickness=2) 
  ~scrollable = object(self)
  inherit scrollbar ~scroll_bar_size ~scrollable 

  method size_request = { cols=scroll_window_size; rows=scroll_bar_thickness }

  method private dim_of_coord = col
  method private dim_of_size = cols
  method private dim_of_rect r = r.col2 - r.col1
  method private mouse_offset m alloc = m.LTerm_mouse.col - alloc.col1 
  method private update_dim c col = { c with LTerm_geom.col }
  method private key_scroll_incr = LTerm_key.Right
  method private key_scroll_decr = LTerm_key.Left

  method draw ctx focused = 
    let focus = (self :> t) = focused in
    let { rows; _ } = LTerm_draw.size ctx in

    let style = if focus then focused_style else unfocused_style in
    LTerm_draw.fill_style ctx style;

    let offset = self#scroll_of_window @@ self#dim_of_coord scrollable#offset in

    let c1 = offset in
    let c2 = offset + scroll_bar_size-1 in
    for col=c1 to c2 do
      for row=0 to rows-1 do
        if row=0 || row=rows-1 then
          if col=c1 && col=c2 then
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int xbar
          else if col=c1 then
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int lbar
          else if col=c2 then
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int rbar
          else
            LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int hbar
        else if col=c1 || col=c2 then
          LTerm_draw.draw_char ~style ctx row col @@ UChar.of_int vbar
      done
    done

end


