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
end

class virtual scrollbar ~default_scroll_bar_size = 
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
    method scroll_bar_size = default_scroll_bar_size

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

    (* methods for setting offset from mouse click.
      
       ratio: scale whole scroll bar area into the number of steps.  The scroll
              bar will not necessarily end up where clicked.  Also, the extremities
              need to be clicked exactly.
      
       middle: place the middle of the scroll bar at the cursor.  Large scroll bars
               will reduce the clickable area by their size.  Nice otherwise.
      
       left/right: place the edges of the scroll bar at the cursor.  Test only.
                   Similar problem to middle. *)

    method private mouse_scale_ratio scroll = 
      let steps, size = self#scroll_bar_steps, self#scroll_bar_size in
      map_range (self#scroll_window_size - 1) (steps - 1) scroll 

    method private mouse_scale_middle scroll = 
      let size = self#scroll_bar_size in
      scroll - (size/2)

    method private mouse_scale_left scroll = scroll

    method private mouse_scale_right scroll = 
      let size = self#scroll_bar_size in
      scroll - size + 1

    val mutable mouse_mode : [`ratio|`middle|`left|`right] = `ratio
    method set_mouse_mode m = mouse_mode <- m
    method private mouse_scale scroll = 
      match mouse_mode with
      | `ratio -> self#mouse_scale_ratio scroll
      | `middle -> self#mouse_scale_middle scroll
      | `left -> self#mouse_scale_left scroll
      | `right -> self#mouse_scale_right scroll

    (* event handling *)
    initializer self#on_event @@ fun ev ->
      let open LTerm_mouse in
      let open LTerm_key in

      let alloc = self#allocation in

      match ev with
      | LTerm_event.Mouse m when m.button=Button1 && in_rect alloc (coord m) ->
        let scroll = self#mouse_offset m alloc in
        self#set_offset @@ self#window_of_scroll @@ self#mouse_scale scroll;
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

    method debug_offset = scroll_bar_offset
    method debug_size = self#scroll_bar_size
    method debug_steps = self#scroll_bar_steps

  end

class vscrollbar ?size_request ?(default_scroll_bar_size=5) () = object(self)
  inherit scrollbar ~default_scroll_bar_size

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

    let open LTerm_draw in
    if cols = 1 then
      for r=offset to offset+self#scroll_bar_size-1 do
        draw_char ctx r 0 ~style (CamomileLibrary.UChar.of_int vbar)
      done
    else if self#scroll_bar_size = 1 then
      for c=0 to cols-1 do
        draw_char ctx offset c ~style (CamomileLibrary.UChar.of_int hbar)
      done
    else
      draw_frame ctx
        { row1 = offset; col1 = 0;
          row2 = offset + self#scroll_bar_size; col2 = cols }
        ~style Light

end

class hscrollbar ?size_request ?(default_scroll_bar_size=5) () = object(self)
  inherit scrollbar ~default_scroll_bar_size
  
  method size_request = 
    match size_request with None -> { rows=2; cols=0 } | Some(x) -> x

  method private mouse_offset m alloc = m.LTerm_mouse.col - alloc.col1 
  method private key_scroll_incr = LTerm_key.Right
  method private key_scroll_decr = LTerm_key.Left
  method private scroll_window_size = 
    let alloc = self#allocation in
    alloc.col2 - alloc.col1

  method draw ctx focused = 
    let focus = (self :> t) = focused in
    let { rows; _ } = LTerm_draw.size ctx in

    let style = if focus then focused_style else unfocused_style in
    LTerm_draw.fill_style ctx style;

    let offset = self#scroll_of_window @@ self#offset in

    let open LTerm_draw in
    if rows = 1 then
      for c=offset to offset+self#scroll_bar_size-1 do
        draw_char ctx 0 c ~style (CamomileLibrary.UChar.of_int hbar) (* 0x25a2? *)
      done
    else if self#scroll_bar_size = 1 then
      for r=0 to rows-1 do
        draw_char ctx r offset ~style (CamomileLibrary.UChar.of_int vbar)
      done
    else
      draw_frame ctx
        { row1 = 0; col1 = offset;
          row2 = rows; col2 = offset + self#scroll_bar_size }
        ~style Light

end

