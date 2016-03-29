open CamomileLibrary
open LTerm_geom

class t = LTerm_widget_base_impl.t

let hbar = 0x2550
let vbar = 0x2551
let lbar = 0x2560
let rbar = 0x2563
let tbar = 0x2566
let bbar = 0x2569
let xbar = 0x256c

class type adjustment = object
  method range : int
  method set_range : int -> unit
  method offset : int
  method set_offset : int -> unit
  method incr : unit
  method decr : unit
  method scroll_bar_size : int
end

class virtual scrollbar = 
  let map_range range1 range2 offset1 = 
    let map_range range1 range2 offset1 = 
      max 0. (min range2 (range2 *. offset1 /. range1)) 
    in
    let rnd x = int_of_float (x +. 0.5) in
    rnd @@ map_range 
      (float_of_int range1)
      (float_of_int range2)
      (float_of_int offset1)
  in
  object(self)
    inherit t "scrollbar"

    method can_focus = true

    val mutable focused_style = LTerm_style.none
    val mutable unfocused_style = LTerm_style.none
    method update_resources =
      let rc = self#resource_class and resources = self#resources in
      focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
      unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources

    val mutable range = 0
    val mutable offset = 0
    method range = range
    method set_range r = range <- max 0 r
    method offset = offset
    method set_offset o = offset <- max 0 (min (range-1) o); self#queue_draw
    method scroll_bar_size = 5

    method virtual private mouse_offset : LTerm_mouse.t -> rect -> int
    method virtual private key_scroll_incr : LTerm_key.code
    method virtual private key_scroll_decr : LTerm_key.code
    method virtual private scroll_window_size : int

    method private scroll_bar_steps = 
      self#scroll_window_size - self#scroll_bar_size + 1

    val mutable scroll_bar_offset = 0
    method private set_scroll_bar_offset o = 
      scroll_bar_offset <- max 0 (min (self#scroll_bar_steps-1) o)

    method private window_of_scroll offset = 
      self#set_scroll_bar_offset offset;
      map_range (self#scroll_bar_steps-1) (range-1) scroll_bar_offset

    method private scroll_of_window offset = 
      let offset = map_range (range-1) (self#scroll_bar_steps-1) offset in
      self#set_scroll_bar_offset offset;
      scroll_bar_offset

    method incr = 
      if range >= self#scroll_bar_steps then
        self#set_offset @@ self#window_of_scroll (scroll_bar_offset+1) 
      else
        self#set_offset (self#offset+1);

    method decr = 
      if range >= self#scroll_bar_steps then
        self#set_offset @@ self#window_of_scroll (scroll_bar_offset-1) 
      else
        self#set_offset (self#offset-1);

  initializer self#on_event @@ fun ev ->
    let open LTerm_mouse in
    let open LTerm_key in

    let alloc = self#allocation in

    match ev with
    | LTerm_event.Mouse m when m.button=Button1 && in_rect alloc (coord m) ->
      let scroll = self#mouse_offset m alloc in
      self#set_offset @@ self#window_of_scroll scroll;
      true

    | LTerm_event.Key { control = false; meta = false; shift = true; code } 
      when code=self#key_scroll_decr ->
      self#decr;
      true

    | LTerm_event.Key { control = false; meta = false; shift = true; code } 
      when code=self#key_scroll_incr ->
      self#incr;
      true

    | _ -> false

  end

class vscrollbar ?size_request () = object(self)
  inherit scrollbar

  method size_request = 
    match size_request with None -> { rows=0; cols=2 } | Some(x) -> x

  method private mouse_offset m alloc = m.LTerm_mouse.row - alloc.row1 
  method private key_scroll_incr = LTerm_key.Down
  method private key_scroll_decr = LTerm_key.Up
  method private scroll_window_size = self#allocation.row2 - self#allocation.row1

  method draw ctx focused = 
    let focus = (self :> t) = focused in
    let { cols; _ } = LTerm_draw.size ctx in

    let style = if focus then focused_style else unfocused_style in
    LTerm_draw.fill_style ctx style;

    let offset = self#scroll_of_window @@ self#offset in

    for col=0 to cols-1 do
      let r1 = offset in
      let r2 = offset + self#scroll_bar_size-1 in
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

class hscrollbar ?size_request () = object(self)
  inherit scrollbar
  
  method size_request = 
    match size_request with None -> { rows=2; cols=0 } | Some(x) -> x

  method private mouse_offset m alloc = m.LTerm_mouse.col - alloc.col1 
  method private key_scroll_incr = LTerm_key.Right
  method private key_scroll_decr = LTerm_key.Left
  method private scroll_window_size = self#allocation.col2 - self#allocation.col1

  method draw ctx focused = 
    let focus = (self :> t) = focused in
    let { rows; _ } = LTerm_draw.size ctx in

    let style = if focus then focused_style else unfocused_style in
    LTerm_draw.fill_style ctx style;

    let offset = self#scroll_of_window @@ self#offset in

    let c1 = offset in
    let c2 = offset + self#scroll_bar_size - 1 in
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

