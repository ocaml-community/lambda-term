type switch

class type ['a] radio = object
  method on : unit
  method off : unit
  method id : 'a
end

class ['a] radiogroup : object
  method on_state_change : ?switch : switch -> ('a option -> unit) -> unit
  method state : 'a option
  method register_button : 'a radio -> unit
  method switch_to : 'a -> unit
end

class ['a] radiobutton : 'a radiogroup -> string -> 'a -> object
  inherit LTerm_widget.t
  method on : unit
  method off : unit
  method label : string
  method on_click : ?switch:LTerm_widget.switch -> (unit -> unit) -> unit
  method set_label : string -> unit
  method id : 'a
end
