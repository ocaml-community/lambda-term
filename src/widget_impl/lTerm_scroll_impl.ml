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
  method on_offset_change : ?switch:LTerm_widget_callbacks.switch -> 
    (int -> unit) -> unit
  method incr : unit
  method decr : unit
end

class virtual scrollbar = 
  let map_range range1 range2 offset1 = 
    if range1 = 0 then 0 
    else
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

    (* style *)
    val mutable focused_style = LTerm_style.none
    val mutable unfocused_style = LTerm_style.none
    method update_resources =
      let rc = self#resource_class and resources = self#resources in
      focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
      unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources

    (* callback *)
    val offset_change_callbacks = Lwt_sequence.create ()
    method on_offset_change ?switch (f : int -> unit) = 
      LTerm_widget_callbacks.register switch offset_change_callbacks f

    (* adjustable *)
    val mutable range = 0
    val mutable offset = 0
    method range = range
    method set_range r = 
      range <- max 0 r;
      self#set_offset offset (* ensure offset is clipped to the new range *)
    method offset = offset
    method set_offset o = 
      let o' = max 0 (min (range-1) o) in
      if offset <> o' then begin
        offset <- o';  
        LTerm_widget_callbacks.exec_callbacks offset_change_callbacks o'
      end;
      self#queue_draw
    
    (* configuration of the scroll bar *)

    val mutable scroll_bar_mode : [ `fixed of int | `dynamic of int ] = `fixed 5
    method set_scroll_bar_mode m = scroll_bar_mode <- m
    
    method private scroll_bar_size_fixed size = 
      let wsize = self#scroll_window_size in
      if wsize <= size then max 1 (wsize-1)
      else max 1 size

    method private scroll_bar_size_dynamic view_size = 
      if view_size <= 0 then
        max 1 (self#scroll_window_size / max 1 range)
      else
        let range = float_of_int range in
        let scroll_size = float_of_int @@ self#scroll_window_size in
        let view_size = float_of_int view_size in
        let doc_size = view_size +. range in
        int_of_float @@ scroll_size *. view_size /. doc_size

    val mutable min_scroll_bar_size : int option = None
    method private min_scroll_bar_size = 
      match min_scroll_bar_size with None -> 1 | Some(x) -> x
    method set_min_scroll_bar_size min = min_scroll_bar_size <- Some(min)
     
    val mutable max_scroll_bar_size : int option = None
    method private max_scroll_bar_size = 
      match max_scroll_bar_size with None -> self#scroll_window_size | Some(x) -> x
    method set_max_scroll_bar_size max = max_scroll_bar_size <- Some(max)

    method scroll_bar_size = 
      max self#min_scroll_bar_size @@ min self#max_scroll_bar_size @@
      match scroll_bar_mode with
      | `fixed size -> self#scroll_bar_size_fixed size
      | `dynamic size -> self#scroll_bar_size_dynamic size

    (* virtual methods needed to abstract over vert/horz scrollbars *)

    method virtual private mouse_offset : LTerm_mouse.t -> rect -> int
    method virtual private key_scroll_incr : LTerm_key.code
    method virtual private key_scroll_decr : LTerm_key.code
    method virtual private scroll_window_size : int

    (* coordinate system conversions *)

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

    (* mouse click control *)

    (* scale whole scroll bar area into the number of steps.  The scroll
       bar will not necessarily end up where clicked.  Add a small dead_zone
       at far left and right *)
    method private mouse_scale_ratio scroll = 
      let steps, size = self#scroll_bar_steps, self#scroll_bar_size in
      let wsize = self#scroll_window_size in
      let dead_zone = wsize / 5 in (* ~10% at each end *)
      map_range (wsize - dead_zone - 1) (steps - 1) (scroll - dead_zone/2)

    (* place the middle of the scroll bar at the cursor.  Large scroll bars
       will reduce the clickable area by their size. *)
    method private mouse_scale_middle scroll = 
      let size = self#scroll_bar_size in
      scroll - (size/2)

    method private mouse_scale_auto scroll = 
      if self#scroll_bar_size > self#scroll_window_size/2 then 
        self#mouse_scale_ratio scroll
      else 
        self#mouse_scale_middle scroll

    val mutable mouse_mode : [ `middle | `ratio | `auto ] = `middle
    method set_mouse_mode m = mouse_mode <- m

    method scroll_of_mouse scroll = 
      match mouse_mode with
      | `middle -> self#mouse_scale_middle scroll
      | `ratio -> self#mouse_scale_ratio scroll
      | `auto -> self#mouse_scale_auto scroll

    (* event handling *)

    initializer self#on_event @@ fun ev ->
      let open LTerm_mouse in
      let open LTerm_key in

      let alloc = self#allocation in

      match ev with
      | LTerm_event.Mouse m when m.button=Button1 && in_rect alloc (coord m) ->
        let scroll = self#mouse_offset m alloc in
        self#set_offset @@ self#window_of_scroll @@ self#scroll_of_mouse scroll;
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

class vscrollbar ?(width=2) () = object(self)
  inherit scrollbar 

  method size_request = { rows=0; cols=width }

  method private mouse_offset m alloc = m.LTerm_mouse.row - alloc.row1 
  method private key_scroll_incr = LTerm_key.Down
  method private key_scroll_decr = LTerm_key.Up
  method private scroll_window_size =
    let alloc = self#allocation in
    alloc.row2 - alloc.row1

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

class hscrollbar ?(height=2) () = object(self)
  inherit scrollbar 
  
  method size_request = { rows=height; cols=0 }

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

