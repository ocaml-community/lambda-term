(*
 * history_stress_test.ml
 * ----------------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* Start n processes, adding entries [k, k + n, k + n * 2, ... k + n *
   count] to their history. Between each addition, they save their
   history to the same file.

   At the end the parent check that the history contains all numbers
   from [0] to [n - 1 + n * count]. *)

open Lwt

let rec check nproc count n l =
  match l with
    | [] ->
        n = nproc * (count + 1)
    | p :: l ->
        n = p && check nproc count (n + 1) l

let rec start_procs name fn nproc count k =
  if k = nproc then
    []
  else
    let t =
      lwt status = Lwt_process.exec (name, [|name; fn; string_of_int nproc; string_of_int count; string_of_int k|]) in
      assert (status = Unix.WEXITED 0);
      return ()
    in
    t :: start_procs name fn nproc count (k + 1)

let () =
  match Sys.argv with
    | [|name; s1; s2|] ->
        Lwt_main.run (
          let nproc = int_of_string s1
          and count = int_of_string s2 in
          let fn = Filename.temp_file "lambda-term" "history-test" in
          lwt () = join (start_procs name fn nproc count 0) in
          let history = LTerm_history.create [] in
          lwt () = LTerm_history.load history fn in
          Sys.remove fn;
          if check nproc count 0 (List.sort compare (List.map int_of_string (LTerm_history.contents history))) then begin
            prerr_endline "success";
            exit 0
          end else begin
            prerr_endline "failure";
            exit 1
          end
        )
    | [|name; fn; s1; s2; s3|] ->
        Lwt_main.run (
          let nproc = int_of_string s1
          and count = int_of_string s2
          and start = int_of_string s3 in
          let history = LTerm_history.create [] in
          for_lwt i = 0 to count do
            LTerm_history.add history (string_of_int (start + i * nproc));
            assert (LTerm_history.length history = i + 1 && LTerm_history.old_count history = i);
            LTerm_history.save history fn
          done
        )
    | _ ->
        Printf.eprintf "usage: %s <nproc> <count>\n" Sys.argv.(0);
        exit 2
