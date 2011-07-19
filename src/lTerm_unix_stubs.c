/*
 * lTerm_unix_stubs.c
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>

#if defined(_WIN32) || defined(_WIN64)

#include <windows.h>
#include <stdio.h>

CAMLprim value lt_unix_get_sigwinch()
{
  return Val_int(0);
}

CAMLprim value lt_unix_get_system_encoding()
{
  char codeset[128];
  sprintf(codeset, "CP%d", GetACP());
  return caml_copy_string(codeset);
}

#else

#include <signal.h>
#include <locale.h>
#include <langinfo.h>

CAMLprim value lt_unix_get_sigwinch()
{
#ifdef SIGWINCH
  value result = caml_alloc_tuple(1);
  Field(result, 0) = Val_int(SIGWINCH);
  return result;
#else
  return Val_int(0);
#endif
}

CAMLprim value lt_unix_get_system_encoding()
{
  /* Set the locale according to environment variables: */
  char *locale = setlocale(LC_CTYPE, "");
  /* Get the codeset used by current locale: */
  char *codeset = nl_langinfo(CODESET);
  /* Reset the locale: */
  setlocale(LC_CTYPE, locale);
  /* If the encoding cannot be determined, just use ascii: */
  return caml_copy_string(codeset ? codeset : "ASCII");
}

#endif
