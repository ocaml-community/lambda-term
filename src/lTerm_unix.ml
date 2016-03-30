(*
 * lTerm_unix.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open StdLabels
open Bigarray

external get_sigwinch : unit -> int option = "lt_unix_get_sigwinch"
external get_system_encoding : unit -> string = "lt_unix_get_system_encoding"

let sigwinch = get_sigwinch ()

(* Obtained by running the folliwing makefile in the "localedata"
   directory of the glibc:

   {[
     include SUPPORTED
     all:
             @echo $(SUPPORTED-LOCALES) | sed 's/ /\n/g' | awk -F/ '$$1 ~ /[.]/ { next; }; { print "  | \""$$1"\" -> \""$$2"\"" }'
   ]}
*)
let encoding_of_lang = function
  | "aa_DJ" -> "ISO-8859-1"
  | "aa_ER" -> "UTF-8"
  | "aa_ER@saaho" -> "UTF-8"
  | "aa_ET" -> "UTF-8"
  | "af_ZA" -> "ISO-8859-1"
  | "am_ET" -> "UTF-8"
  | "an_ES" -> "ISO-8859-15"
  | "ar_AE" -> "ISO-8859-6"
  | "ar_BH" -> "ISO-8859-6"
  | "ar_DZ" -> "ISO-8859-6"
  | "ar_EG" -> "ISO-8859-6"
  | "ar_IN" -> "UTF-8"
  | "ar_IQ" -> "ISO-8859-6"
  | "ar_JO" -> "ISO-8859-6"
  | "ar_KW" -> "ISO-8859-6"
  | "ar_LB" -> "ISO-8859-6"
  | "ar_LY" -> "ISO-8859-6"
  | "ar_MA" -> "ISO-8859-6"
  | "ar_OM" -> "ISO-8859-6"
  | "ar_QA" -> "ISO-8859-6"
  | "ar_SA" -> "ISO-8859-6"
  | "ar_SD" -> "ISO-8859-6"
  | "ar_SY" -> "ISO-8859-6"
  | "ar_TN" -> "ISO-8859-6"
  | "ar_YE" -> "ISO-8859-6"
  | "az_AZ" -> "UTF-8"
  | "as_IN" -> "UTF-8"
  | "ast_ES" -> "ISO-8859-15"
  | "be_BY" -> "CP1251"
  | "be_BY@latin" -> "UTF-8"
  | "bem_ZM" -> "UTF-8"
  | "ber_DZ" -> "UTF-8"
  | "ber_MA" -> "UTF-8"
  | "bg_BG" -> "CP1251"
  | "bn_BD" -> "UTF-8"
  | "bn_IN" -> "UTF-8"
  | "bo_CN" -> "UTF-8"
  | "bo_IN" -> "UTF-8"
  | "br_FR" -> "ISO-8859-1"
  | "br_FR@euro" -> "ISO-8859-15"
  | "bs_BA" -> "ISO-8859-2"
  | "byn_ER" -> "UTF-8"
  | "ca_AD" -> "ISO-8859-15"
  | "ca_ES" -> "ISO-8859-1"
  | "ca_ES@euro" -> "ISO-8859-15"
  | "ca_FR" -> "ISO-8859-15"
  | "ca_IT" -> "ISO-8859-15"
  | "crh_UA" -> "UTF-8"
  | "cs_CZ" -> "ISO-8859-2"
  | "csb_PL" -> "UTF-8"
  | "cv_RU" -> "UTF-8"
  | "cy_GB" -> "ISO-8859-14"
  | "da_DK" -> "ISO-8859-1"
  | "de_AT" -> "ISO-8859-1"
  | "de_AT@euro" -> "ISO-8859-15"
  | "de_BE" -> "ISO-8859-1"
  | "de_BE@euro" -> "ISO-8859-15"
  | "de_CH" -> "ISO-8859-1"
  | "de_DE" -> "ISO-8859-1"
  | "de_DE@euro" -> "ISO-8859-15"
  | "de_LU" -> "ISO-8859-1"
  | "de_LU@euro" -> "ISO-8859-15"
  | "dv_MV" -> "UTF-8"
  | "dz_BT" -> "UTF-8"
  | "el_GR" -> "ISO-8859-7"
  | "el_CY" -> "ISO-8859-7"
  | "en_AG" -> "UTF-8"
  | "en_AU" -> "ISO-8859-1"
  | "en_BW" -> "ISO-8859-1"
  | "en_CA" -> "ISO-8859-1"
  | "en_DK" -> "ISO-8859-1"
  | "en_GB" -> "ISO-8859-1"
  | "en_HK" -> "ISO-8859-1"
  | "en_IE" -> "ISO-8859-1"
  | "en_IE@euro" -> "ISO-8859-15"
  | "en_IN" -> "UTF-8"
  | "en_NG" -> "UTF-8"
  | "en_NZ" -> "ISO-8859-1"
  | "en_PH" -> "ISO-8859-1"
  | "en_SG" -> "ISO-8859-1"
  | "en_US" -> "ISO-8859-1"
  | "en_ZA" -> "ISO-8859-1"
  | "en_ZM" -> "UTF-8"
  | "en_ZW" -> "ISO-8859-1"
  | "es_AR" -> "ISO-8859-1"
  | "es_BO" -> "ISO-8859-1"
  | "es_CL" -> "ISO-8859-1"
  | "es_CO" -> "ISO-8859-1"
  | "es_CR" -> "ISO-8859-1"
  | "es_DO" -> "ISO-8859-1"
  | "es_EC" -> "ISO-8859-1"
  | "es_ES" -> "ISO-8859-1"
  | "es_ES@euro" -> "ISO-8859-15"
  | "es_GT" -> "ISO-8859-1"
  | "es_HN" -> "ISO-8859-1"
  | "es_MX" -> "ISO-8859-1"
  | "es_NI" -> "ISO-8859-1"
  | "es_PA" -> "ISO-8859-1"
  | "es_PE" -> "ISO-8859-1"
  | "es_PR" -> "ISO-8859-1"
  | "es_PY" -> "ISO-8859-1"
  | "es_SV" -> "ISO-8859-1"
  | "es_US" -> "ISO-8859-1"
  | "es_UY" -> "ISO-8859-1"
  | "es_VE" -> "ISO-8859-1"
  | "et_EE" -> "ISO-8859-1"
  | "eu_ES" -> "ISO-8859-1"
  | "eu_ES@euro" -> "ISO-8859-15"
  | "fa_IR" -> "UTF-8"
  | "ff_SN" -> "UTF-8"
  | "fi_FI" -> "ISO-8859-1"
  | "fi_FI@euro" -> "ISO-8859-15"
  | "fil_PH" -> "UTF-8"
  | "fo_FO" -> "ISO-8859-1"
  | "fr_BE" -> "ISO-8859-1"
  | "fr_BE@euro" -> "ISO-8859-15"
  | "fr_CA" -> "ISO-8859-1"
  | "fr_CH" -> "ISO-8859-1"
  | "fr_FR" -> "ISO-8859-1"
  | "fr_FR@euro" -> "ISO-8859-15"
  | "fr_LU" -> "ISO-8859-1"
  | "fr_LU@euro" -> "ISO-8859-15"
  | "fur_IT" -> "UTF-8"
  | "fy_NL" -> "UTF-8"
  | "fy_DE" -> "UTF-8"
  | "ga_IE" -> "ISO-8859-1"
  | "ga_IE@euro" -> "ISO-8859-15"
  | "gd_GB" -> "ISO-8859-15"
  | "gez_ER" -> "UTF-8"
  | "gez_ER@abegede" -> "UTF-8"
  | "gez_ET" -> "UTF-8"
  | "gez_ET@abegede" -> "UTF-8"
  | "gl_ES" -> "ISO-8859-1"
  | "gl_ES@euro" -> "ISO-8859-15"
  | "gu_IN" -> "UTF-8"
  | "gv_GB" -> "ISO-8859-1"
  | "ha_NG" -> "UTF-8"
  | "he_IL" -> "ISO-8859-8"
  | "hi_IN" -> "UTF-8"
  | "hne_IN" -> "UTF-8"
  | "hr_HR" -> "ISO-8859-2"
  | "hsb_DE" -> "ISO-8859-2"
  | "ht_HT" -> "UTF-8"
  | "hu_HU" -> "ISO-8859-2"
  | "hy_AM" -> "UTF-8"
  | "id_ID" -> "ISO-8859-1"
  | "ig_NG" -> "UTF-8"
  | "ik_CA" -> "UTF-8"
  | "is_IS" -> "ISO-8859-1"
  | "it_CH" -> "ISO-8859-1"
  | "it_IT" -> "ISO-8859-1"
  | "it_IT@euro" -> "ISO-8859-15"
  | "iu_CA" -> "UTF-8"
  | "iw_IL" -> "ISO-8859-8"
  | "ka_GE" -> "GEORGIAN-PS"
  | "kk_KZ" -> "PT154"
  | "kl_GL" -> "ISO-8859-1"
  | "km_KH" -> "UTF-8"
  | "kn_IN" -> "UTF-8"
  | "kok_IN" -> "UTF-8"
  | "ks_IN" -> "UTF-8"
  | "ks_IN@devanagari" -> "UTF-8"
  | "ku_TR" -> "ISO-8859-9"
  | "kw_GB" -> "ISO-8859-1"
  | "ky_KG" -> "UTF-8"
  | "lb_LU" -> "UTF-8"
  | "lg_UG" -> "ISO-8859-10"
  | "li_BE" -> "UTF-8"
  | "li_NL" -> "UTF-8"
  | "lij_IT" -> "UTF-8"
  | "lo_LA" -> "UTF-8"
  | "lt_LT" -> "ISO-8859-13"
  | "lv_LV" -> "ISO-8859-13"
  | "mai_IN" -> "UTF-8"
  | "mg_MG" -> "ISO-8859-15"
  | "mhr_RU" -> "UTF-8"
  | "mi_NZ" -> "ISO-8859-13"
  | "mk_MK" -> "ISO-8859-5"
  | "ml_IN" -> "UTF-8"
  | "mn_MN" -> "UTF-8"
  | "mr_IN" -> "UTF-8"
  | "ms_MY" -> "ISO-8859-1"
  | "mt_MT" -> "ISO-8859-3"
  | "my_MM" -> "UTF-8"
  | "nan_TW@latin" -> "UTF-8"
  | "nb_NO" -> "ISO-8859-1"
  | "nds_DE" -> "UTF-8"
  | "nds_NL" -> "UTF-8"
  | "ne_NP" -> "UTF-8"
  | "nl_AW" -> "UTF-8"
  | "nl_BE" -> "ISO-8859-1"
  | "nl_BE@euro" -> "ISO-8859-15"
  | "nl_NL" -> "ISO-8859-1"
  | "nl_NL@euro" -> "ISO-8859-15"
  | "nn_NO" -> "ISO-8859-1"
  | "nr_ZA" -> "UTF-8"
  | "nso_ZA" -> "UTF-8"
  | "oc_FR" -> "ISO-8859-1"
  | "om_ET" -> "UTF-8"
  | "om_KE" -> "ISO-8859-1"
  | "or_IN" -> "UTF-8"
  | "os_RU" -> "UTF-8"
  | "pa_IN" -> "UTF-8"
  | "pa_PK" -> "UTF-8"
  | "pap_AN" -> "UTF-8"
  | "pl_PL" -> "ISO-8859-2"
  | "ps_AF" -> "UTF-8"
  | "pt_BR" -> "ISO-8859-1"
  | "pt_PT" -> "ISO-8859-1"
  | "pt_PT@euro" -> "ISO-8859-15"
  | "ro_RO" -> "ISO-8859-2"
  | "ru_RU" -> "ISO-8859-5"
  | "ru_UA" -> "KOI8-U"
  | "rw_RW" -> "UTF-8"
  | "sa_IN" -> "UTF-8"
  | "sc_IT" -> "UTF-8"
  | "sd_IN" -> "UTF-8"
  | "sd_IN@devanagari" -> "UTF-8"
  | "se_NO" -> "UTF-8"
  | "shs_CA" -> "UTF-8"
  | "si_LK" -> "UTF-8"
  | "sid_ET" -> "UTF-8"
  | "sk_SK" -> "ISO-8859-2"
  | "sl_SI" -> "ISO-8859-2"
  | "so_DJ" -> "ISO-8859-1"
  | "so_ET" -> "UTF-8"
  | "so_KE" -> "ISO-8859-1"
  | "so_SO" -> "ISO-8859-1"
  | "sq_AL" -> "ISO-8859-1"
  | "sq_MK" -> "UTF-8"
  | "sr_ME" -> "UTF-8"
  | "sr_RS" -> "UTF-8"
  | "sr_RS@latin" -> "UTF-8"
  | "ss_ZA" -> "UTF-8"
  | "st_ZA" -> "ISO-8859-1"
  | "sv_FI" -> "ISO-8859-1"
  | "sv_FI@euro" -> "ISO-8859-15"
  | "sv_SE" -> "ISO-8859-1"
  | "sw_KE" -> "UTF-8"
  | "sw_TZ" -> "UTF-8"
  | "ta_IN" -> "UTF-8"
  | "te_IN" -> "UTF-8"
  | "tg_TJ" -> "KOI8-T"
  | "th_TH" -> "TIS-620"
  | "ti_ER" -> "UTF-8"
  | "ti_ET" -> "UTF-8"
  | "tig_ER" -> "UTF-8"
  | "tk_TM" -> "UTF-8"
  | "tl_PH" -> "ISO-8859-1"
  | "tn_ZA" -> "UTF-8"
  | "tr_CY" -> "ISO-8859-9"
  | "tr_TR" -> "ISO-8859-9"
  | "ts_ZA" -> "UTF-8"
  | "tt_RU" -> "UTF-8"
  | "tt_RU@iqtelif" -> "UTF-8"
  | "ug_CN" -> "UTF-8"
  | "uk_UA" -> "KOI8-U"
  | "ur_PK" -> "UTF-8"
  | "uz_UZ" -> "ISO-8859-1"
  | "uz_UZ@cyrillic" -> "UTF-8"
  | "ve_ZA" -> "UTF-8"
  | "vi_VN" -> "UTF-8"
  | "wa_BE" -> "ISO-8859-1"
  | "wa_BE@euro" -> "ISO-8859-15"
  | "wae_CH" -> "UTF-8"
  | "wo_SN" -> "UTF-8"
  | "xh_ZA" -> "ISO-8859-1"
  | "yi_US" -> "CP1255"
  | "yo_NG" -> "UTF-8"
  | "yue_HK" -> "UTF-8"
  | "zh_CN" -> "GB2312"
  | "zh_HK" -> "BIG5-HKSCS"
  | "zh_SG" -> "GB2312"
  | "zh_TW" -> "BIG5"
  | "zu_ZA" -> "ISO-8859-1"
  | _ -> "ASCII"

let system_encoding =
  match get_system_encoding () with
  | "" -> begin
      match Sys.getenv "LANG" with
      | exception _ ->
        "ASCII"
      | lang ->
        match String.index lang '.' with
        | exception _ ->
          encoding_of_lang lang
        | idx ->
          String.sub lang ~pos:(idx + 1) ~len:(String.length lang - idx - 1)
    end
  | enc ->
    enc
;;

module Event_parser = struct
  module Bigstring = LTerm_bigstring

  type t =
    { fd                     : Unix.file_descr
    ; mutable escape_time    : float
    ; mutable active         : bool
    ; buffer                 : LTerm_bigstring.t
    ; mutable ofs            : int
    ; mutable max            : int
    ; mutable button_pressed : int (* mouse event for button up doesn't include which one
                                      it is, so we have to remember it *)
    }

  let create fd ~escape_time =
    { fd
    ; escape_time
    ; active         = true
    ; buffer         = Bigstring.create 4096
    ; ofs            = 0
    ; max            = 0
    ; button_pressed = -1
    }
  ;;

  let escape_time t = t.escape_time
  let set_escape_time t span = t.escape_time <- span

  let discard t =
    t.ofs <- 0;
    t.max <- 0;
  ;;

  let shift_remaining t =
    let len = t.max - t.ofs in
    if 0 < t.ofs && len > 0 then
      Bigstring.blit ~src:t.buffer ~dst:t.buffer ~src_pos:t.ofs ~dst_pos:0 ~len;
    t.ofs <- 0;
    t.max <- len;
  ;;

  (* Wait before reading so that we avoid eating data when the terminal is disabled *)
  let rec wait_for_fd t =
    try
      let res = Unix.select [t.fd] [] [] (-1.0) in
      if t.active then
        `Ready
      else
        `Disabled
    with Unix.Unix_error (EINTR, _, _) ->
      if t.active then
        wait_for_fd t
      else
        `Disabled

  let rec wait_for_fd_with_timeout t ~timeout ~timeout_at =
    try
      let res = Unix.select [t.fd] [] [] timeout in
      if t.active then
        match res with
        | [], [], [] -> `Timeout
        | _ -> `Ready
      else
        `Disabled
    with Unix.Unix_error (EINTR, _, _) ->
      if t.active then
        let now = Unix.gettimeofday () in
        let timeout = timeout_at -. now in
        if now <= 0. then
          `Timeout
        else
          wait_for_fd_with_timeout t ~timeout ~timeout_at
      else
        `Disabled
  ;;

  let rec read_handle_eintr t =
    match Bigstring.read t.fd ~buf:t.buffer ~pos:t.max ~len:(Bigstring.length t.buffer - t.max) with
    | n ->
      t.max <- t.max + n;
      if t.active then `Read n else `Disabled
    | exception (Unix.Unix_error (EINTR, _, _)) ->
      if t.active then
        read_handle_eintr t
      else
        `Disabled
  ;;

  let refill t =
    shift_remaining t;
    match wait_for_fd t with
    | `Ready -> read_handle_eintr t
    | `Disabled -> `Disabled
  ;;

  let refill_with_timeout t ~timeout =
    shift_remaining t;
    match wait_for_fd_with_timeout t ~timeout
            ~timeout_at:(Unix.gettimeofday () +. timeout)
    with
    | `Disabled | `Timeout as x -> x
    | `Ready -> read_handle_eintr t
  ;;

  exception Need_more
  let need_more =
    let exn = Need_more in
    fun () -> raise_notrace exn
  ;;

  exception Discard_event
  let discard_event =
    let exn = Discard_event in
    fun () -> raise_notrace exn
  ;;

  (* +---------------------------------------------------------------+
     | Parsing of encoded characters                                 |
     +---------------------------------------------------------------+ *)

  let parse_uchar t first_byte ~can_refill ~start =
    let len =
      match first_byte with
      | '\xc0' .. '\xdf' ->
        2
      | '\xe0' .. '\xef' ->
        3
      | '\xf0' .. '\xf7' ->
        4
      | _ ->
        1
    in
    if len > t.max - start then begin
      if can_refill then need_more () else begin
        t.ofs <- start + 1;
        Uchar.of_char first_byte
      end
    end else begin
      let ch =
        match first_byte with
        | '\xc0' .. '\xdf' ->
          Uchar.of_int ((   (Char.code first_byte           land 0x1f) lsl 6)
                           lor (Char.code t.buffer.{start + 1} land 0x3f))
        | '\xe0' .. '\xef' ->
          Uchar.of_int ((    (Char.code first_byte           land 0x0f) lsl 12)
                           lor ((Char.code t.buffer.{start + 1} land 0x3f) lsl 6)
                           lor  (Char.code t.buffer.{start + 2} land 0x3f))
        | '\xf0' .. '\xf7' ->
          Uchar.of_int ((    (Char.code first_byte           land 0x07) lsl 18)
                           lor ((Char.code t.buffer.{start + 1} land 0x3f) lsl 12)
                           lor ((Char.code t.buffer.{start + 2} land 0x3f) lsl 6)
                           lor  (Char.code t.buffer.{start + 3} land 0x3f))
        | _ ->
          Uchar.of_char first_byte
      in
      t.ofs <- start + len;
      ch
    end
  ;;

  (* +---------------------------------------------------------------+
     | Input of escape sequence                                      |
     +---------------------------------------------------------------+ *)

  (* This doesn't advance [t.ofs] the caller is responsible for doing it by the length of
     the returned string. *)
  let peek_escape t ~can_refill ~start =
    let rec loop ~start i =
      if i < t.max then
        match t.buffer.{i} with
        | '0' .. '9' | ';' | '[' ->
          loop ~start (i + 1)
        | '\x00' .. '\x1f' | '\x80' .. '\xff' ->
          (* Control characters and non-ascii characters are not part of escape
             sequences. *)
          ""
        | _ ->
          Bigstring.sub_string t.buffer ~pos:start ~len:(i - start + 1)
      else if can_refill then
        need_more ()
      else
        (* If the rest is not immediatly available, conclude that this is not an escape
           sequence but just the escape key: *)
        ""
    in
    if start < t.max then
      match t.buffer.{start} with
      | '[' | 'O' ->
        loop ~start (start + 1)
      | _ ->
        ""
    else if can_refill then
      need_more ()
    else
      ""
  ;;

  (* +---------------------------------------------------------------+
     | Escape sequences mapping                                      |
     +---------------------------------------------------------------+ *)

  let controls : LTerm_event.t array = [|
    Char(C, ' ');
    Char(C, 'a');
    Char(C, 'b');
    Char(C, 'c');
    Char(C, 'd');
    Char(C, 'e');
    Char(C, 'f');
    Char(C, 'g');
    Char(C, 'h');
    Key (N, Tab);
    Char(C, 'j');
    Char(C, 'k');
    Char(C, 'l');
    Key (N, Enter);
    Char(C, 'n');
    Char(C, 'o');
    Char(C, 'p');
    Char(C, 'q');
    Char(C, 'r');
    Char(C, 's');
    Char(C, 't');
    Char(C, 'u');
    Char(C, 'v');
    Char(C, 'w');
    Char(C, 'x');
    Char(C, 'y');
    Char(C, 'z');
    Key (N, Escape);
    Char(C, '\\');
    Char(C, ']');
    Char(C, '^');
    Char(C, '_');
  |]

  let sequences : (string * LTerm_event.t) array = [|
    "[1~", Key (N, Home);
    "[2~", Key (N, Insert);
    "[3~", Key (N, Delete);
    "[4~", Key (N, End);
    "[5~", Key (N, Prev);
    "[6~", Key (N, Next);
    "[7~", Key (N, Home);
    "[8~", Key (N, End);
    "[11~", Key (N, F1);
    "[12~", Key (N, F2);
    "[13~", Key (N, F3);
    "[14~", Key (N, F4);
    "[15~", Key (N, F5);
    "[17~", Key (N, F6);
    "[18~", Key (N, F7);
    "[19~", Key (N, F8);
    "[20~", Key (N, F9);
    "[21~", Key (N, F10);
    "[23~", Key (N, F11);
    "[24~", Key (N, F12);

    "[1^", Key (C, Home);
    "[2^", Key (C, Insert);
    "[3^", Key (C, Delete);
    "[4^", Key (C, End);
    "[5^", Key (C, Prev);
    "[6^", Key (C, Next);
    "[7^", Key (C, Home);
    "[8^", Key (C, End);
    "[11^", Key (C, F1);
    "[12^", Key (C, F2);
    "[13^", Key (C, F3);
    "[14^", Key (C, F4);
    "[15^", Key (C, F5);
    "[17^", Key (C, F6);
    "[18^", Key (C, F7);
    "[19^", Key (C, F8);
    "[20^", Key (C, F9);
    "[21^", Key (C, F10);
    "[23^", Key (C, F11);
    "[24^", Key (C, F12);

    "[1$", Key (S, Home);
    "[2$", Key (S, Insert);
    "[3$", Key (S, Delete);
    "[4$", Key (S, End);
    "[5$", Key (S, Prev);
    "[6$", Key (S, Next);
    "[7$", Key (S, Home);
    "[8$", Key (S, End);

    "[1@", Key (C_S, Home);
    "[2@", Key (C_S, Insert);
    "[3@", Key (C_S, Delete);
    "[4@", Key (C_S, End);
    "[5@", Key (C_S, Prev);
    "[6@", Key (C_S, Next);
    "[7@", Key (C_S, Home);
    "[8@", Key (C_S, End);

    "[25~", Key (S, F3);
    "[26~", Key (S, F4);
    "[28~", Key (S, F5);
    "[29~", Key (S, F6);
    "[31~", Key (S, F7);
    "[32~", Key (S, F8);
    "[33~", Key (S, F9);
    "[34~", Key (S, F10);
    "[23$", Key (S, F11);
    "[24$", Key (S, F12);

    "[25^", Key (C_S, F3);
    "[26^", Key (C_S, F4);
    "[28^", Key (C_S, F5);
    "[29^", Key (C_S, F6);
    "[31^", Key (C_S, F7);
    "[32^", Key (C_S, F8);
    "[33^", Key (C_S, F9);
    "[34^", Key (C_S, F10);
    "[23@", Key (C_S, F11);
    "[24@", Key (C_S, F12);

    "[Z", Key (S, Tab);

    "[A", Key (N, Up);
    "[B", Key (N, Down);
    "[C", Key (N, Right);
    "[D", Key (N, Left);

    "[a", Key (S, Up);
    "[b", Key (S, Down);
    "[c", Key (S, Right);
    "[d", Key (S, Left);

    "A", Key (N, Up);
    "B", Key (N, Down);
    "C", Key (N, Right);
    "D", Key (N, Left);

    (* Putty *)
    "OA", Key (C, Up);
    "OB", Key (C, Down);
    "OC", Key (C, Right);
    "OD", Key (C, Left);

    "Oa", Key (C, Up);
    "Ob", Key (C, Down);
    "Oc", Key (C, Right);
    "Od", Key (C, Left);

    "OP", Key (N, F1);
    "OQ", Key (N, F2);
    "OR", Key (N, F3);
    "OS", Key (N, F4);

    "O2P", Key (S, F1);
    "O2Q", Key (S, F2);
    "O2R", Key (S, F3);
    "O2S", Key (S, F4);

    "O3P", Key (M, F1);
    "O3Q", Key (M, F2);
    "O3R", Key (M, F3);
    "O3S", Key (M, F4);

    "O4P", Key (M_S, F1);
    "O4Q", Key (M_S, F2);
    "O4R", Key (M_S, F3);
    "O4S", Key (M_S, F4);

    "O5P", Key (C, F1);
    "O5Q", Key (C, F2);
    "O5R", Key (C, F3);
    "O5S", Key (C, F4);

    "O6P", Key (C_S, F1);
    "O6Q", Key (C_S, F2);
    "O6R", Key (C_S, F3);
    "O6S", Key (C_S, F4);

    "O7P", Key (C_M, F1);
    "O7Q", Key (C_M, F2);
    "O7R", Key (C_M, F3);
    "O7S", Key (C_M, F4);

    "O8P", Key (C_M_S, F1);
    "O8Q", Key (C_M_S, F2);
    "O8R", Key (C_M_S, F3);
    "O8S", Key (C_M_S, F4);

    "[[A", Key (N, F1);
    "[[B", Key (N, F2);
    "[[C", Key (N, F3);
    "[[D", Key (N, F4);
    "[[E", Key (N, F5);

    "[H", Key (N, Home);
    "[F", Key (N, End);

    "OH", Key (N, Home);
    "OF", Key (N, End);

    "H", Key (N, Home);
    "F", Key (N, End);

    "[1;2A", Key (S, Up);
    "[1;2B", Key (S, Down);
    "[1;2C", Key (S, Right);
    "[1;2D", Key (S, Left);

    "[1;3A", Key (M, Up);
    "[1;3B", Key (M, Down);
    "[1;3C", Key (M, Right);
    "[1;3D", Key (M, Left);

    "[1;4A", Key (M_S, Up);
    "[1;4B", Key (M_S, Down);
    "[1;4C", Key (M_S, Right);
    "[1;4D", Key (M_S, Left);

    "[1;5A", Key (C, Up);
    "[1;5B", Key (C, Down);
    "[1;5C", Key (C, Right);
    "[1;5D", Key (C, Left);

    "[1;6A", Key (C_S, Up);
    "[1;6B", Key (C_S, Down);
    "[1;6C", Key (C_S, Right);
    "[1;6D", Key (C_S, Left);

    "[1;7A", Key (C_M, Up);
    "[1;7B", Key (C_M, Down);
    "[1;7C", Key (C_M, Right);
    "[1;7D", Key (C_M, Left);

    "[1;8A", Key (C_M_S, Up);
    "[1;8B", Key (C_M_S, Down);
    "[1;8C", Key (C_M_S, Right);
    "[1;8D", Key (C_M_S, Left);

    "[1;2P", Key (S, F1);
    "[1;2Q", Key (S, F2);
    "[1;2R", Key (S, F3);
    "[1;2S", Key (S, F4);

    "[1;3P", Key (M, F1);
    "[1;3Q", Key (M, F2);
    "[1;3R", Key (M, F3);
    "[1;3S", Key (M, F4);

    "[1;4P", Key (M_S, F1);
    "[1;4Q", Key (M_S, F2);
    "[1;4R", Key (M_S, F3);
    "[1;4S", Key (M_S, F4);

    "[1;5P", Key (C, F1);
    "[1;5Q", Key (C, F2);
    "[1;5R", Key (C, F3);
    "[1;5S", Key (C, F4);

    "[1;6P", Key (C_S, F1);
    "[1;6Q", Key (C_S, F2);
    "[1;6R", Key (C_S, F3);
    "[1;6S", Key (C_S, F4);

    "[1;7P", Key (C_M, F1);
    "[1;7Q", Key (C_M, F2);
    "[1;7R", Key (C_M, F3);
    "[1;7S", Key (C_M, F4);

    "[1;8P", Key (C_M_S, F1);
    "[1;8Q", Key (C_M_S, F2);
    "[1;8R", Key (C_M_S, F3);
    "[1;8S", Key (C_M_S, F4);

    "O1;2P", Key (S, F1);
    "O1;2Q", Key (S, F2);
    "O1;2R", Key (S, F3);
    "O1;2S", Key (S, F4);

    "O1;3P", Key (M, F1);
    "O1;3Q", Key (M, F2);
    "O1;3R", Key (M, F3);
    "O1;3S", Key (M, F4);

    "O1;4P", Key (M_S, F1);
    "O1;4Q", Key (M_S, F2);
    "O1;4R", Key (M_S, F3);
    "O1;4S", Key (M_S, F4);

    "O1;5P", Key (C, F1);
    "O1;5Q", Key (C, F2);
    "O1;5R", Key (C, F3);
    "O1;5S", Key (C, F4);

    "O1;6P", Key (C_S, F1);
    "O1;6Q", Key (C_S, F2);
    "O1;6R", Key (C_S, F3);
    "O1;6S", Key (C_S, F4);

    "O1;7P", Key (C_M, F1);
    "O1;7Q", Key (C_M, F2);
    "O1;7R", Key (C_M, F3);
    "O1;7S", Key (C_M, F4);

    "O1;8P", Key (C_M_S, F1);
    "O1;8Q", Key (C_M_S, F2);
    "O1;8R", Key (C_M_S, F3);
    "O1;8S", Key (C_M_S, F4);

    "[15;2~", Key (S, F5);
    "[17;2~", Key (S, F6);
    "[18;2~", Key (S, F7);
    "[19;2~", Key (S, F8);
    "[20;2~", Key (S, F9);
    "[21;2~", Key (S, F10);
    "[23;2~", Key (S, F11);
    "[24;2~", Key (S, F12);

    "[15;3~", Key (M, F5);
    "[17;3~", Key (M, F6);
    "[18;3~", Key (M, F7);
    "[19;3~", Key (M, F8);
    "[20;3~", Key (M, F9);
    "[21;3~", Key (M, F10);
    "[23;3~", Key (M, F11);
    "[24;3~", Key (M, F12);

    "[15;4~", Key (M_S, F5);
    "[17;4~", Key (M_S, F6);
    "[18;4~", Key (M_S, F7);
    "[19;4~", Key (M_S, F8);
    "[20;4~", Key (M_S, F9);
    "[21;4~", Key (M_S, F10);
    "[23;4~", Key (M_S, F11);
    "[24;4~", Key (M_S, F12);

    "[15;5~", Key (C, F5);
    "[17;5~", Key (C, F6);
    "[18;5~", Key (C, F7);
    "[19;5~", Key (C, F8);
    "[20;5~", Key (C, F9);
    "[21;5~", Key (C, F10);
    "[23;5~", Key (C, F11);
    "[24;5~", Key (C, F12);

    "[15;6~", Key (C_S, F5);
    "[17;6~", Key (C_S, F6);
    "[18;6~", Key (C_S, F7);
    "[19;6~", Key (C_S, F8);
    "[20;6~", Key (C_S, F9);
    "[21;6~", Key (C_S, F10);
    "[23;6~", Key (C_S, F11);
    "[24;6~", Key (C_S, F12);

    "[15;7~", Key (C_M, F5);
    "[17;7~", Key (C_M, F6);
    "[18;7~", Key (C_M, F7);
    "[19;7~", Key (C_M, F8);
    "[20;7~", Key (C_M, F9);
    "[21;7~", Key (C_M, F10);
    "[23;7~", Key (C_M, F11);
    "[24;7~", Key (C_M, F12);

    "[15;8~", Key (C_M_S, F5);
    "[17;8~", Key (C_M_S, F6);
    "[18;8~", Key (C_M_S, F7);
    "[19;8~", Key (C_M_S, F8);
    "[20;8~", Key (C_M_S, F9);
    "[21;8~", Key (C_M_S, F10);
    "[23;8~", Key (C_M_S, F11);
    "[24;8~", Key (C_M_S, F12);

    "[1;2H", Key (S, Home);
    "[1;2F", Key (S, End);

    "[1;3H", Key (M, Home);
    "[1;3F", Key (M, End);

    "[1;4H", Key (M_S, Home);
    "[1;4F", Key (M_S, End);

    "[1;5H", Key (C, Home);
    "[1;5F", Key (C, End);

    "[1;6H", Key (C_S, Home);
    "[1;6F", Key (C_S, End);

    "[1;7H", Key (C_M, Home);
    "[1;7F", Key (C_M, End);

    "[1;8H", Key (C_M_S, Home);
    "[1;8F", Key (C_M_S, End);

    "[2;2~", Key (S, Insert);
    "[3;2~", Key (S, Delete);
    "[5;2~", Key (S, Prev);
    "[6;2~", Key (S, Next);

    "[2;3~", Key (M, Insert);
    "[3;3~", Key (M, Delete);
    "[5;3~", Key (M, Prev);
    "[6;3~", Key (M, Next);

    "[2;4~", Key (M_S, Insert);
    "[3;4~", Key (M_S, Delete);
    "[5;4~", Key (M_S, Prev);
    "[6;4~", Key (M_S, Next);

    "[2;5~", Key (C, Insert);
    "[3;5~", Key (C, Delete);
    "[5;5~", Key (C, Prev);
    "[6;5~", Key (C, Next);

    "[2;6~", Key (C_S, Insert);
    "[3;6~", Key (C_S, Delete);
    "[5;6~", Key (C_S, Prev);
    "[6;6~", Key (C_S, Next);

    "[2;7~", Key (C_M, Insert);
    "[3;7~", Key (C_M, Delete);
    "[5;7~", Key (C_M, Prev);
    "[6;7~", Key (C_M, Next);

    "[2;8~", Key (C_M_S, Insert);
    "[3;8~", Key (C_M_S, Delete);
    "[5;8~", Key (C_M_S, Prev);
    "[6;8~", Key (C_M_S, Next);
  |]

  let () = Array.sort ~cmp:(fun (seq1, _) (seq2, _) -> String.compare seq1 seq2) sequences

  let find_sequence seq =
    let rec loop a b =
      if a = b then
        None
      else
        let c = (a + b) / 2 in
        let k, v = Array.get sequences c in
        match String.compare seq k with
        | d when d < 0 ->
          loop a c
        | d when d > 0 ->
          loop (c + 1) b
        | _ ->
          Some v
    in
    loop 0 (Array.length sequences)
  ;;

  let rec scan_text t ofs =
    if ofs >= t.max then
      ofs
    else begin
      match t.buffer.{ofs} with
      | '\x00' .. '\x1f' | '\x7f' -> ofs
      | _ -> scan_text t (ofs + 1)
    end
  ;;

  let make_char m c : LTerm_event.t =
    if Char.code c <= 127 then
      Char (m, c)
    else
      Uchar (m, Uchar.of_char c)
  ;;

  let make_uchar m c : LTerm_event.t =
    if Uchar.to_int c <= 127 then
      Char (m, Uchar.to_char c)
    else
      Uchar (m, c)
  ;;

  let parse_event t  ~can_refill : LTerm_event.t =
    match t.buffer.{t.ofs} with
    | '\x00' .. '\x1a' | '\x1c' .. '\x1f' as byte ->
      (* Control characters *)
      t.ofs <- t.ofs + 1;
      controls.(Char.code byte)
    | '\x7f' ->
      (* Backspace *)
      t.ofs <- t.ofs + 1;
      Key (N, Backspace)
    | '\x20' .. '\x7e' | '\x80' .. '\xff' as byte ->
      (* Text *)
      let start_of_text = t.ofs in
      let end_of_text   = scan_text t (start_of_text + 1) in
      t.ofs <- end_of_text;
      if end_of_text = start_of_text + 1 then
        (* Fast path for the common case *)
        make_char N byte
      else begin
        let s =
          Bigstring.sub_string t.buffer
            ~pos:start_of_text
            ~len:(end_of_text - start_of_text)
        in
        match Zed_utf8.check s with
        | Ok 1 -> make_uchar N (Zed_utf8.extract s 0)
        | _    -> Text s
      end
    | '\x1b' ->
      (* Escape sequences *)
      match peek_escape t ~can_refill ~start:(t.ofs + 1) with
      | "" ->
        (* If it is not an escape, test if it is META+key. *)
        if t.ofs + 1 >= t.max then begin
          if can_refill then
            need_more ()
          else begin
            t.ofs <- t.ofs + 1;
            Key (N, Escape)
          end
        end else begin
          match t.buffer.{t.ofs + 1} with
          | '\x1b' -> begin
              (* Escape sequences *)
              match peek_escape t ~can_refill ~start:(t.ofs + 2) with
              | "" ->
                t.ofs <- t.ofs + 1;
                Key (N, Escape)
              | seq -> begin
                  t.ofs <- t.ofs + 2 + String.length seq;
                  match find_sequence seq with
                  | Some (Key (m, k)) ->
                    Key (LTerm_event.Modifiers.set_meta m true, k)
                  | Some x ->
                    x
                  | None ->
                    Sequence ("\x1b\x1b" ^ seq)
                end
            end
          | '\x00' .. '\x1a' | '\x1c' .. '\x1f' as byte ->
            (* Control characters *)
            t.ofs <- t.ofs + 2;
            (match controls.(Char.code byte) with
             | Key  (N, k) -> Key  (M, k)
             | Char (C, c) -> Char (C_M, c)
             | _ -> assert false)
          | '\x7f' ->
            (* Backspace *)
            t.ofs <- t.ofs + 2;
            Key (M, Backspace)
          | '\x20' .. '\x7e' as byte ->
            (* Other ascii characters *)
            t.ofs <- t.ofs + 2;
            Char (M, byte)
          | '\x80' .. '\xff' as byte ->
            let ch = parse_uchar t ~can_refill ~start:(t.ofs + 1) byte in
            make_uchar M ch
        end
      | "[M" as seq -> begin
          (* Mouse report *)
          if t.ofs + 5 >= t.max then begin
            if can_refill then need_more () else begin
              t.ofs <- t.ofs + 3;
              Sequence ("\x1b" ^ seq)
            end
          end else begin
            let ofs = t.ofs in
            t.ofs <- ofs + 6;
            let mask = Char.code t.buffer.{ofs + 3} in
            let coord : LTerm_geom.coord =
              { col = Char.code t.buffer.{ofs + 4} - 33
              ; row = Char.code t.buffer.{ofs + 5} - 33 }
            in
            let modifiers : LTerm_event.Modifiers.t =
              match mask land 0b00011000 with
              | 0b00000000 -> N
              | 0b00010000 -> C
              | 0b00001000 -> M
              | 0b00011000 -> C_M
              | _ -> assert false
            in
            if mask = 0b00100011 then begin
              let button_pressed = t.button_pressed in
              if button_pressed < 0 then
                discard_event ()
              else begin
                t.button_pressed <- -1;
                Button_up (modifiers, button_pressed, coord)
              end
            end else begin
              let button =
                match mask land 0b11000111 with
                | 0b00000000 -> 0
                | 0b00000001 -> 1
                | 0b00000010 -> 2
                | 0b01000000 -> 3
                | 0b01000001 -> 4
                | 0b01000010 -> 5
                | 0b01000011 -> 6
                | 0b01000100 -> 7
                | 0b01000101 -> 8
                | _          -> -1
              in
              t.button_pressed <- button;
              if button < 0 then discard_event ();
              Button_down (modifiers, button, coord)
            end
          end
        end
      | seq ->
        t.ofs <- t.ofs + 1 + String.length seq;
        match find_sequence seq with
        | Some ev -> ev
        | None    -> Sequence ("\x1b" ^ seq)
  ;;

  let rec read t =
    if not t.active then
      None
    else if t.ofs < t.max then begin
      match
        (* First try: we are allowed to refill the buffer *)
        parse_event t ~can_refill:true
      with
      | event -> Some event
      | exception Discard_event -> read t
      | exception Need_more ->
        match refill_with_timeout t ~timeout:t.escape_time with
        | `Disabled -> None
        | `Read _ | `Timeout ->
          match
            (* Second try: we must decide now *)
            parse_event t ~can_refill:false
          with
          | event -> Some event
          | exception (Need_more | Discard_event) ->
            (* If we still want more, assume we are trying with a new sequence *)
            read t
    end else
      match refill t with
      | `Read 0   -> raise End_of_file
      | `Read _   -> read t
      | `Disabled -> None

  let set_active t active = t.active <- active
end
