/*
 * lTerm_iconv_stubs.c
 * -------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#include <iconv.h>
#include <errno.h>

/* +-----------------------------------------------------------------+
   | Conversion descriptors                                          |
   +-----------------------------------------------------------------+ */

#define Iconv_val(v) *(iconv_t*)Data_custom_val(v)

static void lt_iconv_finalize(value val_cd)
{
  iconv_t cd = Iconv_val(val_cd);
  if (cd) iconv_close(cd);
}

static int lt_iconv_compare(value v1, value v2)
{
  long d = (long)Iconv_val(v1) - (long)Iconv_val(v2);
  if (d < 0) return -1;
  if (d > 0) return +1;
  return 0;
}

static long lt_iconv_hash(value v)
{
  return (long)Iconv_val(v);
}

static struct custom_operations iconv_ops = {
  "lambda-term:iconv",
  lt_iconv_finalize,
  lt_iconv_compare,
  lt_iconv_hash,
  custom_serialize_default,
  custom_deserialize_default
};


static value alloc_iconv(iconv_t cd)
{
  value result = caml_alloc_custom(&iconv_ops, sizeof(iconv_t), 0, 1);
  Iconv_val(result) = cd;
  return result;
}

/* +-----------------------------------------------------------------+
   | Opening/closing                                                 |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_iconv_iconv_open(value to_code, value of_code)
{
  iconv_t cd = iconv_open(String_val(to_code), String_val(of_code));
  if (cd == (iconv_t)-1) {
    value args[2];
    args[0] = of_code;
    args[1] = to_code;
    caml_raise_with_args(*caml_named_value("lambda-term:iconv:unsupported"), 2, args);
  }
  return alloc_iconv(cd);
}

CAMLprim value lt_iconv_iconv_close(value val_cd)
{
  iconv_t cd = Iconv_val(val_cd);
  if (cd) {
    Iconv_val(val_cd) = NULL;
    iconv_close(cd);
  }
  return Val_unit;
}

CAMLprim value lt_iconv_iconv(value val_cd, value val_src, value val_dst)
{
  iconv_t cd = Iconv_val(val_cd);
  if (cd == NULL) caml_raise_constant(*caml_named_value("lambda-term:iconv:closed"));

  char *src = String_val(Field(val_src, 0)) + Long_val(Field(val_src, 1));
  size_t src_len = Long_val(Field(val_src, 2)) - Long_val(Field(val_src, 1));

  char *dst = String_val(Field(val_dst, 0)) + Long_val(Field(val_dst, 1));
  size_t dst_len = Long_val(Field(val_dst, 2)) - Long_val(Field(val_dst, 1));

  size_t result = iconv(cd, &src, &src_len, &dst, &dst_len);

  Field(val_src, 1) = Val_long(Long_val(Field(val_src, 2)) - src_len);
  Field(val_dst, 1) = Val_long(Long_val(Field(val_dst, 2)) - dst_len);

  if (result == (size_t)-1) {
    switch (errno) {
    case EILSEQ:
      caml_raise_constant(*caml_named_value("lambda-term:iconv:invalid-sequence"));
    case EINVAL:
      caml_raise_constant(*caml_named_value("lambda-term:iconv:unterminated-sequence"));
    case E2BIG:
      caml_raise_constant(*caml_named_value("lambda-term:iconv:insufficient-space"));
    }
  }

  return Val_unit;
}

CAMLprim value lt_iconv_reset(value val_cd)
{
  iconv_t cd = Iconv_val(val_cd);
  if (cd == NULL) caml_raise_constant(*caml_named_value("lambda-term:iconv:closed"));
  iconv(cd, NULL, 0, NULL, 0);
  return Val_unit;
}
