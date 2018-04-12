[@@@ocaml.warning "-3"]
include module type of Lwt_sequence with type 'a t = 'a Lwt_sequence.t
