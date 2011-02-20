/*
 * lt_windows_stubs.c
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 */

/* Windows specific stubs */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#if defined(_WIN32) || defined(_WIN64)

#include <windows.h>

/* +-----------------------------------------------------------------+
   | Codepage functions                                              |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_windows_get_acp()
{
  return Val_int(GetACP);
}

CAMLprim value lt_windows_get_console_cp()
{
  return Val_int(GetConsoleCP());
}

CAMLprim value lt_windows_set_console_cp(value cp)
{
  SetConsoleCP(Int_val(cp));
  return Val_unit;
}

CAMLprim value lt_windows_get_console_output_cp()
{
  return Val_int(GetConsoleOutputCP());
}

CAMLprim value lt_windows_set_console_output_cp(value cp)
{
  SetConsoleOutputCP(Int_val(cp));
  return Val_unit;
}

#else

/* +-----------------------------------------------------------------+
   | For unix                                                        |
   +-----------------------------------------------------------------+ */

#include <lwt_unix.h>

#define NA(name, feature)                       \
  CAMLprim value lt_windows_##name()            \
  {                                             \
    lwt_unix_not_available(feature);            \
    return Val_unit;                            \
  }

NA(get_acp, "GetACP")
NA(get_console_cp, "GetConsoleCP")
NA(set_console_cp, "SetConsoleCP")
NA(get_console_output_cp, "GetConsoleOutputCP")
NA(set_console_output_cp, "SetConsoleOutputCP")

#endif
