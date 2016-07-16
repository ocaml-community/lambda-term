(*
 * history_stress_test.ml
 * ----------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open StdLabels
open Lambda_term

(* Start n processes, adding entries [k, k + n, k + n * 2, ... k + n *
   count] to their history. Between each addition, they save their
   history to the same file.

   At the end the parent check that the history contains all numbers
   from [0] to [n - 1 + n * count]. *)

let rec check nproc count n l =
  match l with
  | [] ->
    n = nproc * count
  | p :: l ->
    n = p && check nproc count (n + 1) l

let start_procs name fn nproc count =
  let rec start n =
    if n = nproc then
      []
    else
      let pid =
        Unix.create_process
          name
          [| name; fn; string_of_int nproc
           ; string_of_int count; string_of_int n
          |]
          Unix.stdin Unix.stdout Unix.stderr
      in
      pid :: start (n + 1)
  in
  List.iter (start 0) ~f:(fun pid ->
    let _, status = Unix.waitpid [] pid in
    assert (status = WEXITED 0))

let () =
  match Sys.argv with
  | [|name; fn; s1; s2|] ->
    if Sys.file_exists fn then Sys.remove fn;
    let nproc = int_of_string s1
    and count = int_of_string s2 in
    start_procs name fn nproc count;
    let history = History.create [] in
    History.load history fn;

    if check nproc count 0
        (List.sort ~cmp:compare
           (List.map ~f:int_of_string (History.contents history))) then begin
      prerr_endline "success";
      exit 0
    end else begin
      prerr_endline "failure";
      exit 1
    end
  | [|_name; fn; s1; s2; s3|] ->
    let nproc = int_of_string s1
    and count = int_of_string s2
    and start = int_of_string s3 in
    let history = History.create [] in
    let rec loop i =
      if i < count then begin
        History.add history (string_of_int (start + i * nproc));
        assert (History.length history = i + 1 && History.old_count history = i);
        History.save history fn;
        loop (i + 1)
      end
    in loop 0
  | _ ->
    Printf.eprintf "usage: %s <file> <nproc> <count>\n" Sys.argv.(0);
    exit 2
