/*
 * lt_unix_stubs.c
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>

#if defined(_WIN32) || defined(_WIN64)

CAMLprim value lt_unix_get_sigwinch()
{
  return Val_int(0);
}

#else

#include <signal.h>

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

#endif
