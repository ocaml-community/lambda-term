(*
 * print_sequences.ml
 * ------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Script to print sequences returned by the terminal. You can execute
   it like this:

   # ocaml print_sequences.ml
*)

#load "unix.cma";;

let () =
  let decode_mouse =
    match Array.to_list Sys.argv with
    | _ :: "mouse" :: _ ->
      true
    | _ ->
      false
  in
  (* Setup terminal attributes. *)
  let attr = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH {
    attr with
      Unix.c_brkint = false;
      Unix.c_inpck = false;
      Unix.c_istrip = false;
      Unix.c_ixon = false;
      Unix.c_csize = 8;
      Unix.c_parenb = false;
      Unix.c_echo = false;
      Unix.c_icanon = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
      Unix.c_isig = false;
  };
  print_string "\027[?1003h\027[?1004h";
  flush stdout;
  (* Read and print key sequences. *)
  print_endline "press 'q' to quit";
  let buf = Bytes.create 128 in
  let rec loop () =
    let n = Unix.read Unix.stdin buf 0 (String.length buf) in
    let s = String.sub buf 0 n in
    if decode_mouse && n = 6 && s.[0] = '\027' && s.[1] = '[' && s.[2] = 'M' then
      let b = Char.code s.[3] - 32 in
      let x = Char.code s.[4] - 32 in
      let y = Char.code s.[5] - 32 in
      let buf = Bytes.create 8 in
      for i = 0 to 7 do
        buf.[i] <- if b land (1 lsl (7-i)) <> 0 then '1' else '0'
      done;
      Printf.printf "mouse: %s %dx%d\n%!" buf x y
    else
      print_endline (String.escaped s);
    if s <> "q" then loop ()
  in
  let result =
    try
      loop ();
      `OK
    with exn ->
      `Exn exn
  in
  print_string "\027[?1003l\027[?1004l";
  flush stdout;
  (* Reset terminal attributes. *)
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr;
  match result with
    | `OK -> ()
    | `Exn exn -> raise exn

