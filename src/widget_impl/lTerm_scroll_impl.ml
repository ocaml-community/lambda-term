open CamomileLibrary
open LTerm_geom

class t = LTerm_widget_base_impl.t

let hbar = 0x2550
let vbar = 0x2551

class type adjustment = object
  method range : int
  method set_range : int -> unit

  method offset : int
  method set_offset : int -> unit
  method on_offset_change : ?switch:LTerm_widget_callbacks.switch -> 
    (int -> unit) -> unit
end

class type scrollable_adjustment = object
  inherit adjustment

  (* public interface *)

  method incr : unit
  method decr : unit

  method set_scroll_bar_mode : [ `fixed of int | `dynamic of int ] -> unit
  method set_mouse_mode : [ `middle | `ratio | `auto ] -> unit
  method set_min_scroll_bar_size : int -> unit
  method set_max_scroll_bar_size : int -> unit

  (* private scrollbar interface *)

  method set_scroll_window_size : int -> unit
  method set_scroll_bar_offset : int -> unit
  method scroll_window_size : int
  method scroll_bar_size : int
  method scroll_bar_steps : int
  method scroll_of_window : int -> int
  method window_of_scroll : int -> int
  method scroll_of_mouse : int -> int
end

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

class default_scrollable_adjustment = object(self)

  val offset_change_callbacks = Lwt_sequence.create ()
  method on_offset_change ?switch (f : int -> unit) = 
    LTerm_widget_callbacks.register switch offset_change_callbacks f

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
    end

  val mutable scroll_window_size = 0
  method scroll_window_size = scroll_window_size
  method set_scroll_window_size s = scroll_window_size <- s

  val mutable scroll_bar_mode : [ `fixed of int | `dynamic of int ] = `fixed 5
  method set_scroll_bar_mode m = scroll_bar_mode <- m
  
  method private scroll_bar_size_fixed size = 
    let wsize = self#scroll_window_size in
    if wsize <= size then max 1 (wsize-1)
    else max 1 size

  method private scroll_bar_size_dynamic view_size = 
    if range <= 1 then
      self#scroll_window_size
    else if view_size <= 0 then
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

  method scroll_bar_steps = 
    self#scroll_window_size - self#scroll_bar_size + 1

  val mutable scroll_bar_offset = 0
  method set_scroll_bar_offset o = 
    scroll_bar_offset <- max 0 (min (self#scroll_bar_steps-1) o)

  method window_of_scroll offset = 
    self#set_scroll_bar_offset offset;
    map_range (self#scroll_bar_steps-1) (range-1) scroll_bar_offset

  method scroll_of_window offset = 
    let offset = map_range (range-1) (self#scroll_bar_steps-1) offset in
    self#set_scroll_bar_offset offset;
    scroll_bar_offset

  method incr = 
    if range >= self#scroll_bar_steps then
      self#set_offset @@ self#window_of_scroll (scroll_bar_offset+1) 
    else
      self#set_offset (offset+1);

  method decr = 
    if range >= self#scroll_bar_steps then
      self#set_offset @@ self#window_of_scroll (scroll_bar_offset-1) 
    else
      self#set_offset (offset-1);

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

end

class virtual scrollbar rc (adj : #scrollable_adjustment) = object(self)
  inherit t rc

  method can_focus = true

  (* style *)
  val mutable focused_style = LTerm_style.none
  val mutable unfocused_style = LTerm_style.none
  val mutable bar_style : [ `filled | `outline ] = `outline
  val mutable show_track = false
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
    unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources;
    bar_style <- 
      (match LTerm_resources.get (rc ^ ".barstyle") resources with
      | "filled" -> `filled
      | "outline" | "" -> `outline
      | str -> Printf.ksprintf failwith "invalid scrollbar style");
    show_track <- 
      (match LTerm_resources.get_bool (rc ^ ".track") resources with
      | Some(x) -> x
      | None -> false)

  (* virtual methods needed to abstract over vert/horz scrollbars *)

  method virtual private mouse_offset : LTerm_mouse.t -> rect -> int
  method virtual private key_scroll_incr : LTerm_key.code
  method virtual private key_scroll_decr : LTerm_key.code

  (* event handling *)

  initializer self#on_event @@ fun ev ->
    let open LTerm_mouse in
    let open LTerm_key in

    let alloc = self#allocation in

    match ev with
    | LTerm_event.Mouse m when m.button=Button1 && in_rect alloc (coord m) ->
      let scroll = self#mouse_offset m alloc in
      adj#set_offset @@ adj#window_of_scroll @@ adj#scroll_of_mouse scroll;
      true

    | LTerm_event.Key { control = false; meta = false; shift = true; code } 
      when code=self#key_scroll_decr ->
      adj#decr;
      true

    | LTerm_event.Key { control = false; meta = false; shift = true; code } 
      when code=self#key_scroll_incr ->
      adj#incr;
      true

    | _ -> false

  (* drawing *)
  method private draw_bar ctx style rect =
    let open LTerm_draw in
    let { cols; rows } = size_of_rect rect in
    if cols=1 || rows=1 || bar_style=`filled then
      let x = 
        CamomileLibrary.UChar.of_int @@
          if bar_style=`filled then 0x2588
          else if cols=1 then vbar
          else hbar
      in
      for c=rect.col1 to rect.col2-1 do
        for r=rect.row1 to rect.row2-1 do
          draw_char ctx r c ~style x
        done
      done
    else
      draw_frame ctx rect ~style Light

end

class vscrollbar_for_adjustment  ?(rc="scrollbar") ?(width=2) adj = object(self)
  inherit scrollbar rc adj as super

  method size_request = { rows=0; cols=width }

  method private mouse_offset m alloc = m.LTerm_mouse.row - alloc.row1 
  method private key_scroll_incr = LTerm_key.Down
  method private key_scroll_decr = LTerm_key.Up

  method set_allocation r = 
    super#set_allocation r;
    adj#set_scroll_window_size (r.row2 - r.row1)

  method draw ctx focused = 
    let open LTerm_draw in
    let focus = (self :> t) = focused in
    let { cols; _ } = size ctx in

    let style = if focus then focused_style else unfocused_style in
    fill_style ctx style;

    let offset = adj#scroll_of_window @@ adj#offset in

    let rect =  
      { row1 = offset; col1 = 0;
        row2 = offset + adj#scroll_bar_size; col2 = cols }
    in

    (if show_track then draw_vline ctx 0 (cols/2) adj#scroll_window_size ~style Light);
    self#draw_bar ctx style rect

end

class hscrollbar_for_adjustment  ?(rc="scrollbar") ?(height=2) adj = object(self)
  inherit scrollbar rc adj as super
  
  method size_request = { rows=height; cols=0 }

  method private mouse_offset m alloc = m.LTerm_mouse.col - alloc.col1 
  method private key_scroll_incr = LTerm_key.Right
  method private key_scroll_decr = LTerm_key.Left

  method set_allocation r = 
    super#set_allocation r;
    adj#set_scroll_window_size (r.col2 - r.col1)


  method draw ctx focused = 
    let open LTerm_draw in
    let focus = (self :> t) = focused in
    let { rows; _ } = size ctx in

    let style = if focus then focused_style else unfocused_style in
    fill_style ctx style;

    let offset = adj#scroll_of_window @@ adj#offset in

    let rect = 
      { row1 = 0; col1 = offset;
        row2 = rows; col2 = offset + adj#scroll_bar_size }
    in

    (if show_track then draw_hline ctx (rows/2) 0 adj#scroll_window_size ~style Light);
    self#draw_bar ctx style rect

end

class vscrollbar ?(rc="scrollbar") ?(width=2) () = 
  let adj = new default_scrollable_adjustment in
  object(self)
    inherit vscrollbar_for_adjustment ~rc ~width adj 

    method range  = adj#range
    method set_range = adj#set_range
    method offset = adj#offset
    method set_offset = adj#set_offset
    method on_offset_change = adj#on_offset_change
    method incr = adj#incr
    method decr = adj#decr
    method set_scroll_bar_mode = adj#set_scroll_bar_mode
    method set_mouse_mode = adj#set_mouse_mode
    method set_min_scroll_bar_size = adj#set_min_scroll_bar_size
    method set_max_scroll_bar_size = adj#set_max_scroll_bar_size
    method set_scroll_window_size = adj#set_scroll_window_size
    method set_scroll_bar_offset = adj#set_scroll_bar_offset
    method scroll_window_size = adj#scroll_window_size
    method scroll_bar_size = adj#scroll_bar_size
    method scroll_bar_steps = adj#scroll_bar_steps
    method scroll_of_window = adj#scroll_of_window
    method window_of_scroll = adj#window_of_scroll
    method scroll_of_mouse = adj#scroll_of_mouse
  end

class hscrollbar ?(rc="scrollbar") ?(height=2) () = 
  let adj = new default_scrollable_adjustment in
  object(self)
    inherit hscrollbar_for_adjustment ~rc ~height adj 

    method range  = adj#range
    method set_range = adj#set_range
    method offset = adj#offset
    method set_offset = adj#set_offset
    method on_offset_change = adj#on_offset_change
    method incr = adj#incr
    method decr = adj#decr
    method set_scroll_bar_mode = adj#set_scroll_bar_mode
    method set_mouse_mode = adj#set_mouse_mode
    method set_min_scroll_bar_size = adj#set_min_scroll_bar_size
    method set_max_scroll_bar_size = adj#set_max_scroll_bar_size
    method set_scroll_window_size = adj#set_scroll_window_size
    method set_scroll_bar_offset = adj#set_scroll_bar_offset
    method scroll_window_size = adj#scroll_window_size
    method scroll_bar_size = adj#scroll_bar_size
    method scroll_bar_steps = adj#scroll_bar_steps
    method scroll_of_window = adj#scroll_of_window
    method window_of_scroll = adj#window_of_scroll
    method scroll_of_mouse = adj#scroll_of_mouse
  end

