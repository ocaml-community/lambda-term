open CamomileLibraryDyn.Camomile
open LTerm_geom
open LTerm_key
open LTerm_widget_callbacks

let section = Lwt_log.Section.make "lambda-term(buttons_impl)"

class button initial_label = object(self)
  inherit LTerm_widget_base_impl.t "button" as super

  method can_focus = true

  val click_callbacks = Lwt_sequence.create ()

  method on_click ?switch f =
    register switch click_callbacks f

  val mutable size_request = { rows = 1; cols = 4 + Zed_utf8.length initial_label }
  method size_request = size_request

  val mutable label = initial_label

  method label = label

  method set_label text =
    label <- text;
    size_request <- { rows = 1; cols = 4 + Zed_utf8.length text };
    self#queue_draw

  initializer
    self#on_event
      (function
         | LTerm_event.Key { control = false; meta = false; shift = false; code = Enter } ->
             exec_callbacks click_callbacks ();
             true
         | _ ->
             false)

  val mutable focused_style = LTerm_style.none
  val mutable unfocused_style = LTerm_style.none
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
    unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources

  method draw ctx focused =
    let { rows; cols } = LTerm_draw.size ctx in
    let len = Zed_utf8.length label in
    if focused = (self :> LTerm_widget_base_impl.t) then begin
      LTerm_draw.fill_style ctx focused_style;
      LTerm_draw.draw_string ctx (rows / 2) ((cols - len - 4) / 2) (Printf.sprintf "< %s >" label)
    end else begin
      LTerm_draw.fill_style ctx unfocused_style;
      LTerm_draw.draw_string ctx (rows / 2) ((cols - len - 4) / 2) (Printf.sprintf "< %s >" label)
    end
end

class checkbutton initial_label initial_state = object(self)
  inherit button initial_label

  val mutable state = initial_state

  initializer
    self#on_event
    (function
      | LTerm_event.Key { control = false; meta = false; shift = false; code }
        when (code = Enter || code = Char(UChar.of_char ' ')) ->
          state <- not state;
          (* checkbutton changes the state when clicked, so has to be redrawn *)
          self#queue_draw;
          exec_callbacks click_callbacks ();
          true
      | _ ->
          false);
    self#set_resource_class "checkbutton"

  method state = state

  method draw ctx focused =
    let { rows; cols } = LTerm_draw.size ctx in
    let style = if focused = (self :> LTerm_widget_base_impl.t) then focused_style else unfocused_style in
    let checked = if state then "[x]" else "[ ]" in
    begin
      LTerm_draw.fill_style ctx style;
      LTerm_draw.draw_string ctx (rows / 2) 0 (checked ^ label);
    end

end


class type ['a] radio = object
  method on : unit
  method off : unit
  method id : 'a
end

class ['a] radiogroup  = object(self)

  val state_change_callbacks : ('a option -> unit) Lwt_sequence.t = Lwt_sequence.create ()

  method on_state_change ?switch (f : 'a option -> unit) =
    register switch state_change_callbacks f

  val mutable state = None
  val mutable buttons = []

  method state = state

  method register_button (button : 'a radio) =
    if buttons = [] then button#on else ();
    buttons <- button :: buttons;
    ()

  method switch_to some_id =
    let switch_button button =
      if button#id != some_id then button#off else () in
    List.iter switch_button buttons;
    state <- Some some_id;
    exec_callbacks state_change_callbacks state

end

class ['a] radiobutton (group : 'a radiogroup) initial_label (id : 'a) = object(self)
  inherit button initial_label

  val mutable state = false

  initializer
    self#on_event
    (function
      | LTerm_event.Key { control = false; meta = false; shift = false; code }
        when (code = Enter || code = Char(UChar.of_char ' ')) ->
          if state
          (* no need to do anything if the button is on already *)
          then ()
          (* otherwise switch state, redraw itself and inform the radio group
           * about the change *)
          else
            begin
              state <- true;
              self#queue_draw;
              group#switch_to id;
            end;
          (* event is consumed in any case *)
          true
      | _ -> false);
    self#set_resource_class "radiobutton";
    group#register_button (self :> 'a radio)

  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
    unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources
  method draw ctx focused =
    let { rows; cols } = LTerm_draw.size ctx in
    let style = if focused = (self :> LTerm_widget_base_impl.t) then focused_style else unfocused_style in
    let checked = if state then "(o) " else "( ) " in
    LTerm_draw.fill_style ctx style;
    LTerm_draw.draw_string ctx (rows / 2) 0 (checked ^ self#label);

  method on =
    state <- true;
    self#queue_draw;
    group#switch_to id;

  method off =
    state <- false;
    self#queue_draw

  method id = id

end

