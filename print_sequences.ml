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
  print_string "\027[?1003h;\027[?1004h;\027[?1005h";
  flush stdout;
  (* Read and print key sequences. *)
  print_endline "press 'q' to quit";
  let buf = Bytes.create 128 in
  let rec loop () =
    let n = Unix.read Unix.stdin buf 0 (String.length buf) in
    let s = String.sub buf 0 n in
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
  print_string "\027[?1003l;\027[?1005l;\027[?1004l";
  flush stdout;
  (* Reset terminal attributes. *)
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr;
  match result with
    | `OK -> ()
    | `Exn exn -> raise exn

