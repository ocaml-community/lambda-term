open CamomileLibraryDyn.Camomile
open LTerm_geom
open LTerm_key

let section = Lwt_log.Section.make "lambda-term(radiobuttons)"

class type ['a] radio = object
  method on : unit
  method off : unit
  method id : 'a
end

type switch = { mutable switch_state : (int option -> unit) list option }

let register switch_opt seq f =
  match switch_opt with
    | None ->
        ignore (Lwt_sequence.add_l f seq)
    | Some switch ->
        match switch.switch_state with
          | Some l ->
              let node = Lwt_sequence.add_l f seq in
              switch.switch_state <- Some ((fun _ -> Lwt_sequence.remove node) :: l)
          | None ->
              ()

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
    Lwt_sequence.iter_l
    (fun f ->
      try
        f state
      with exn ->
        ignore (Lwt_log.error ~section ~exn "callback failed with"))
    state_change_callbacks

end

class ['a] radiobutton (group : 'a radiogroup) initial_label (id : 'a) = object(self)
  inherit LTerm_widget.button initial_label

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

  val mutable focused_style = LTerm_style.none
  val mutable unfocused_style = LTerm_style.none
  method update_resources =
    let rc = self#resource_class and resources = self#resources in
    focused_style <- LTerm_resources.get_style (rc ^ ".focused") resources;
    unfocused_style <- LTerm_resources.get_style (rc ^ ".unfocused") resources
  method draw ctx focused =
    let { rows; cols } = LTerm_draw.size ctx in
    let style = if focused = (self :> LTerm_widget.t) then focused_style else unfocused_style in
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

