(*
 * lTerm_history.ml
 * ----------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

(* A node contains an entry of the history. *)
type node =
  { mutable data : Zed_utf8.t
  ; mutable size : int
  ; mutable prev : node
  }

type t =
  { (* Points to the first entry (the most recent). Its [prev] is a fake
       node used as marker, is after the oldest entry. *)
    mutable entries     : node
  ; mutable full_size   : int
  ; mutable length      : int
  ; mutable max_size    : int
  ; mutable max_entries : int
  ; mutable old_count   : int
  ; (* When set, the cache is equal to the list of entries, from the
       most recent to the oldest. *)
    mutable cache       : Zed_utf8.t list option
  }

let entry_size str =
  let size = ref 0 in
  for i = 0 to String.length str - 1 do
    match String.unsafe_get str i with
    | '\n' | '\\' ->
      size := !size + 2
    | _ ->
      size := !size + 1
  done;
  !size + 1

(* Check that [size1 + size2 < limit], handling overflow. *)
let size_ok size1 size2 limit =
  let sum = size1 + size2 in
  sum >= 0 && sum <= limit

let create ?(max_size=max_int) ?(max_entries=max_int) init =
  if max_size < 0 then
    invalid_arg "LTerm_history.create: negative maximum size";
  if max_entries < 0 then
    invalid_arg "LTerm_history.create: negative maximum number of entries";
  let rec aux size count node entries =
    match entries with
    | [] ->
      (size, count, node)
    | entry :: entries ->
      let entry_size = entry_size entry in
      if size_ok size entry_size max_size && count + 1 < max_entries then begin
        let next = { data = ""; prev = node; size = 0 } in
        node.data <- entry;
        node.size <- entry_size;
        aux (size + entry_size) (count + 1) next entries
      end else
        (size, count, node)
  in
  let rec node = { data = ""; size = 0; prev = node } in
  let size, count, marker = aux 0 0 node init in
  node.prev <- marker;
  { entries     = node
  ; full_size   = size
  ; length      = count
  ; max_size    = max_size
  ; max_entries = max_entries
  ; old_count   = count
  ; cache       = None
  }

let is_space ch  = Uucp.White.is_white_space (Uchar.to_int ch)
let is_empty str = Zed_utf8.for_all is_space str

let is_dup history entry =
  history.length > 0 && history.entries.data = entry

(* Remove the oldest entry of history, precondition: the history
   contains at least one entry. *)
let drop_oldest history =
  let last = history.entries.prev.prev in
  (* Make [last] become the end of entries marker. *)
  history.entries.prev <- last;
  (* Update counters. *)
  history.length <- history.length - 1;
  history.full_size <- history.full_size - last.size;
  if history.old_count > 0 then history.old_count <- history.old_count - 1;
  (* Clear the marker so its contents can be garbage collected. *)
  last.data <- "";
  last.size <- 0

let add_aux history data size =
  if size <= history.max_size then begin
    (* Check length. *)
    if history.length = history.max_entries then begin
      history.cache <- None;
      (* We know that [max_entries > 0], so the precondition is
         verified. *)
      drop_oldest history
    end;
    (* Check size. *)
    if not (size_ok history.full_size size history.max_size) then begin
      history.cache <- None;
      (* We know that size <= max_size, so we are here only if there
         is at least one other entry in the history, so the
         precondition is verified. *)
      drop_oldest history;
      while not (size_ok history.full_size size history.max_size) do
        (* Same here. *)
        drop_oldest history
      done
    end;
    (* Add the entry. *)
    let node = { data = data; size = size; prev = history.entries.prev } in
    history.entries.prev <- node;
    history.entries <- node;
    history.length <- history.length + 1;
    history.full_size <- history.full_size + size;
    match history.cache with
    | None ->
      ()
    | Some l ->
      history.cache <- Some (data :: l)
  end

let add history ?(skip_empty=true) ?(skip_dup=true) entry =
  if history.max_entries > 0                  &&
     history.max_size    > 0                  &&
     not (skip_empty && is_empty entry      ) &&
     not (skip_dup   && is_dup history entry) then
    add_aux history entry (entry_size entry)

let rec list_of_nodes marker acc node =
  if node == marker then
    acc
  else
    list_of_nodes marker (node.data :: acc) node.prev

let contents history =
  match history.cache with
  | Some l ->
    l
  | None ->
    let marker = history.entries.prev in
    let l = list_of_nodes marker [] marker.prev in
    history.cache <- Some l;
    l

let size history = history.full_size
let length history = history.length
let old_count history = history.old_count
let max_size history = history.max_size
let max_entries history = history.max_entries

let set_old_count history n =
  if n < 0 then
    invalid_arg "LTerm_history.set_old_count: negative old count";
  if n > history.length then
    invalid_arg "LTerm_history.set_old_count: old count greater than the length of the history";
  history.old_count <- n

let set_max_size history size =
  if size < 0 then
    invalid_arg "LTerm_history.set_max_size: negative maximum size";
  if size < history.full_size then begin
    history.cache <- None;
    (* 0 <= size < full_size so there is at least one element. *)
    drop_oldest history;
    while size < history.full_size do
      (* Same here. *)
      drop_oldest history
    done
  end;
  history.max_size <- size

let set_max_entries history n =
  if n < 0 then
    invalid_arg "LTerm_history.set_max_entries: negative maximum number of entries";
  if n < history.length then begin
    history.cache <- None;
    (* 0 <= n < length so there is at least one element. *)
    drop_oldest history;
    while n < history.length do
      (* Same here. *)
      drop_oldest history
    done
  end;
  history.max_entries <- n

let escape entry =
  let len = String.length entry in
  let buf = Buffer.create len in
  let rec loop ofs =
    if ofs = len then
      Buffer.contents buf
    else
      match String.unsafe_get entry ofs with
      | '\n' ->
        Buffer.add_string buf "\\n";
        loop (ofs + 1)
      | '\\' ->
        Buffer.add_string buf "\\\\";
        loop (ofs + 1)
      | ch when Char.code ch <= 127 ->
        Buffer.add_char buf ch;
        loop (ofs + 1)
      | _ ->
        let ofs' = Zed_utf8.unsafe_next entry ofs in
        Buffer.add_substring buf entry ofs (ofs' - ofs);
        loop ofs'
  in
  loop 0

let unescape line =
  let len = String.length line in
  let buf = Buffer.create len in
  let rec loop ofs size =
    if ofs = len then
      (Buffer.contents buf, size + 1)
    else
      match String.unsafe_get line ofs with
      | '\\' ->
        if ofs = len then begin
          Buffer.add_char buf '\\';
          (Buffer.contents buf, size + 3)
        end else begin
          match String.unsafe_get line (ofs + 1) with
          | 'n' ->
            Buffer.add_char buf '\n';
            loop (ofs + 2) (size + 2)
          | '\\' ->
            Buffer.add_char buf '\\';
            loop (ofs + 2) (size + 2)
          | _ ->
            Buffer.add_char buf '\\';
            loop (ofs + 1) (size + 2)
        end
      | ch when Char.code ch <= 127 ->
        Buffer.add_char buf ch;
        loop (ofs + 1) (size + 1)
      | _ ->
        let ofs' = Zed_utf8.next line ofs in
        Buffer.add_substring buf line ofs (ofs' - ofs);
        loop ofs' (size + ofs' - ofs)
  in
  loop 0 0

let default_log msg =
  Printf.eprintf "%s: %s\n%!"
    (Filename.basename Sys.executable_name)
    msg

let rec safe_lockf ~log fn fd cmd ofs =
  try
    Unix.lockf fd cmd ofs;
    true
  with
  | Unix.Unix_error (EINTR, _, _) ->
    safe_lockf ~log fn fd cmd ofs
  | Unix.Unix_error (error, _, _) ->
    Printf.ksprintf log "failed to lock file '%s': %s" fn (Unix.error_message error);
    false

let open_history ~log fn =
  match Unix.openfile fn [O_RDWR] 0 with
  | fd ->
    Some (fd, safe_lockf ~log fn fd F_LOCK 0)
  | exception Unix.Unix_error (ENOENT, _, _) ->
    None
  | exception Unix.Unix_error (EACCES, _, _) ->
    Printf.ksprintf log "cannot open file '%s' in read and write mode: %s" fn (Unix.error_message Unix.EACCES);
    (* If the file cannot be openned in read & write mode, open it in
       read only mode but do not lock it. *)
    match Unix.openfile fn [O_RDONLY] 0 with
    | fd -> Some (fd, false)
    | exception Unix.Unix_error (ENOENT, _, _) -> None

let protectx x ~finally ~f =
  match f x with
  | y           -> finally (); y
  | exception e -> finally (); raise e

let load history ?(log=default_log) ?(skip_empty=true) ?(skip_dup=true) fn =
  (* In case we do not load anything. *)
  history.old_count <- history.length;
  if history.max_entries = 0 || history.max_size = 0 then
    (* Do not bother loading the file for nothing... *)
    ()
  else begin
    (* File opening. *)
    match open_history ~log fn with
    | None -> ()
    | Some (fd, locked) ->
      (* File loading. *)
      let ic = Unix.in_channel_of_descr fd in
      protectx () ~finally:(fun () ->
          (* Cleanup. *)
          if locked then ignore (safe_lockf ~log fn fd F_ULOCK 0 : bool);
          close_in ic)
        ~f:(fun () ->
            let rec aux line_number =
              match input_line ic with
              | exception End_of_file -> ()
              | line ->
                match unescape line with
                | exception Zed_utf8.Invalid (msg, _) ->
                  Printf.ksprintf log "file %S, line %d: %s" fn line_number msg;
                  aux (line_number + 1)
                | entry, size ->
                  if not (skip_empty && is_empty entry) && not (skip_dup && is_dup history entry) then begin
                    add_aux history entry size;
                    history.old_count <- history.length
                  end;
                  aux (line_number + 1)
            in
            aux 1)
  end

let rec skip_nodes node count =
  if count = 0 then
    node
  else
    skip_nodes node.prev (count - 1)

let rec copy history marker node skip_empty skip_dup =
  if node != marker then begin
    let line = escape node.data in
    if not (skip_empty && is_empty line) && not (skip_dup && is_dup history line) then
      add_aux history line node.size;
    copy history marker node.prev skip_empty skip_dup
  end

let rec dump_entries oc marker node =
  if node == marker then
    ()
  else begin
    output_string oc node.data;
    output_char oc '\n';
    dump_entries oc marker node.prev
  end

let save history ?(log=default_log) ?max_size ?max_entries ?(skip_empty=true)
    ?(skip_dup=true) ?(append=true) ?(perm=0o666) fn =
  let max_size =
    match max_size with
    | Some m -> m
    | None -> history.max_size
  and max_entries =
    match max_entries with
    | Some m -> m
    | None -> history.max_entries
  in
  let history_save = create ~max_size ~max_entries [] in
  if history_save.max_size = 0 || history_save.max_entries = 0 || (not append && history.old_count = history.length) then
    (* Just empty the history. *)
    Unix.close (Unix.openfile fn [O_CREAT; O_TRUNC] perm)
  else if append && history.old_count = history.length then
    (* Do not touch the file. *)
    ()
  else begin
    let fd = Unix.openfile fn [O_CREAT; O_RDWR] perm in
    (* Lock the entire file. *)
    let locked = safe_lockf ~log fn fd F_LOCK 0 in
    protectx () ~finally:(fun () ->
        if locked then ignore (safe_lockf ~log fn fd F_ULOCK 0 : bool);
        Unix.close fd)
      ~f:(fun () ->
          let count =
            if append then begin
              (* Load existing entries into [history_save].

                 We return the number of entries read. This may be greater
                 than the number of entries stored in [history_save]:
                 - because of limits
                 - because the history files contains duplicated lines
                   and/or empty lines and [skip_dup] and/or [skip_empty]
                   have been specified. *)
              let ic = Unix.in_channel_of_descr fd in
              let rec aux count =
                match input_line ic with
                | exception End_of_file ->
                  history_save.old_count <- history_save.length;
                  count
                | line ->
                  (* Do not bother unescaping. Tests remain the same
                     on the unescaped version. *)
                  if not (skip_empty && is_empty line) && not (skip_dup && is_dup history_save line) then
                    add_aux history_save line (String.length line + 1);
                  aux (count + 1)
              in
              aux 0
            end else
              0
          in
          let marker = history.entries.prev in
          (* Copy new entries into the saving history. *)
          copy history_save marker (skip_nodes marker.prev history.old_count) skip_empty skip_dup;
          let to_skip =
            if append && history_save.old_count = count then
              (* We are in append mode and no old entries were removed: do
                 not modify the file and append new entries at the end of
                 the file. *)
              count
            else begin
              (* Otherwise truncate the file and save everything. *)
              ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
              Unix.ftruncate fd 0;
              0
            end
          in
          (* Save entries to the temporary file. *)
          let oc = Unix.out_channel_of_descr fd in
          let marker = history_save.entries.prev in
          dump_entries oc marker (skip_nodes marker.prev to_skip);
          flush oc;
          (* Done! *)
          history.old_count <- history.length)
  end
