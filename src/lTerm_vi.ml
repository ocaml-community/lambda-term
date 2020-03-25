(*
 * lTerm_vi.ml
 * ------------
 * Copyright : (c) 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

module Concurrent = struct
  module Thread= struct
      include Lwt
      let run= Lwt_unix.run [@@ocaml.warning "-3"]
      let sleep= Lwt_unix.sleep
    end
  module MsgBox= struct
      include Lwt_mvar
      let get= Lwt_mvar.take
      let create= Lwt_mvar.create_empty
    end
end

include Mew_vi.Core.Make (Concurrent)

