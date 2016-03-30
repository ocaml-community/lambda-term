/*
 * lTerm_bigstring_stubs.c
 * -----------------------
 * Copyright : (c) 2016, Jeremie Dimino <jdimino@janestreet.com>
 * Licence   : BSD3
 *
 * This file is a part of lambda-term.
 */

#include <string.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

CAMLprim value lt_bigstring_blit(value sbuf, value sofs,
                                 value dbuf, value dofs,
                                 value len)
{
  memmove(Caml_ba_data_val(dbuf) + Long_val(dofs),
          Caml_ba_data_val(sbuf) + Long_val(sofs),
          Long_val(len));
  return Val_unit;
}

CAMLprim value lt_bigstring_blit_string(value sbuf, value sofs,
                                        value dbuf, value dofs,
                                        value len)
{
  memmove(Caml_ba_data_val(dbuf) + Long_val(dofs),
          String_val(sbuf) + Long_val(sofs),
          Long_val(len));
  return Val_unit;
}

CAMLprim value lt_bigstring_blit_to_bytes(value sbuf, value sofs,
                                          value dbuf, value dofs,
                                          value len)
{
  memmove(String_val(dbuf) + Long_val(dofs),
          Caml_ba_data_val(sbuf) + Long_val(sofs),
          Long_val(len));
  return Val_unit;
}

#if defined(_WIN32) || defined(_WIN64)

/* These stubs are not used on windows */
#define NA(name, feature)                           \
  CAMLprim value lt_bigstring_##name()              \
  {                                                 \
    unix_error(ENOSYS, feature, Nothing);           \
    return Val_unit;                                \
  }

NA(read,  "bigstring_read")
NA(write, "bigstring_write")

#else

CAMLprim value lt_bigstring_read(value fd, value buf, value ofs, value len)
{
  int ret;
  enter_blocking_section();
  ret = read(Int_val(fd),
             Caml_ba_data_val(buf) + Long_val(ofs),
             Long_val(len));
  leave_blocking_section();
  if (ret == -1) uerror("read", Nothing);
  return Val_int(ret);
}

CAMLprim value lt_bigstring_write(value fd, value buf, value ofs, value len)
{
  int ret;
  enter_blocking_section();
  ret = write(Int_val(fd),
              Caml_ba_data_val(buf) + Long_val(ofs),
              Long_val(len));
  leave_blocking_section();
  if (ret == -1) uerror("write", Nothing);
  return Val_int(ret);
}

#endif
