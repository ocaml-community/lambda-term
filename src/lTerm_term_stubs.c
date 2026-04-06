/*
 * lTerm_term_stubs.c
 * ------------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 */

#include <lwt_unix.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#if defined(_WIN32) || defined(_WIN64)

/* +-----------------------------------------------------------------+
   | Terminal sizes on Windows                                       |
   +-----------------------------------------------------------------+ */

#include <windows.h>
#include <wincon.h>

CAMLprim value lt_term_get_size_from_fd(value fd)
{
  HANDLE h = Handle_val(fd);
  CONSOLE_SCREEN_BUFFER_INFO info;
  DWORD mode;
  value result;

  /* Validate that the handle is a usable console handle before calling
     GetConsoleScreenBufferInfo. On some Windows environments (e.g. GitHub
     Actions runners using ConPTY), _isatty() may return true for handles
     that are not real console screen buffers, causing
     GetConsoleScreenBufferInfo to crash with an access violation. */
  if (h == INVALID_HANDLE_VALUE || h == NULL || !GetConsoleMode(h, &mode)) {
    win32_maperr(ERROR_INVALID_HANDLE);
    uerror("GetConsoleScreenBufferInfo", Nothing);
  }

  if (!GetConsoleScreenBufferInfo(h, &info)) {
    win32_maperr(GetLastError());
    uerror("GetConsoleScreenBufferInfo", Nothing);
  }

  result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(info.srWindow.Bottom - info.srWindow.Top + 1);
  Field(result, 1) = Val_int(info.srWindow.Right - info.srWindow.Left + 1);
  return result;
}

CAMLprim value lt_term_set_size_from_fd(value fd, value val_size)
{
  HANDLE h = Handle_val(fd);
  CONSOLE_SCREEN_BUFFER_INFO info;
  SMALL_RECT rect;
  DWORD mode;

  /* Validate that the handle is a usable console handle. */
  if (h == INVALID_HANDLE_VALUE || h == NULL || !GetConsoleMode(h, &mode)) {
    win32_maperr(ERROR_INVALID_HANDLE);
    uerror("SetConsoleWindowInfo", Nothing);
  }

  if (!GetConsoleScreenBufferInfo(h, &info)) {
    win32_maperr(GetLastError());
    uerror("GetConsoleScreenBufferInfo", Nothing);
  }

  rect.Top = info.srWindow.Top;
  rect.Left = info.srWindow.Left;
  rect.Bottom = rect.Top + Int_val(Field(val_size, 0)) - 1;
  rect.Right = rect.Left + Int_val(Field(val_size, 1)) - 1;

  if (!SetConsoleWindowInfo(h, TRUE, &rect)) {
    win32_maperr(GetLastError());
    uerror("SetConsoleWindowInfo", Nothing);
  }

  return Val_unit;
}

#else

/* +-----------------------------------------------------------------+
   | Terminal sizes on Unix                                          |
   +-----------------------------------------------------------------+ */

#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <errno.h>

CAMLprim value lt_term_get_size_from_fd(value fd)
{
  struct winsize size;

  if (ioctl(Int_val(fd), TIOCGWINSZ, &size) < 0)
    uerror("ioctl", Nothing);

  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(size.ws_row);
  Field(result, 1) = Val_int(size.ws_col);
  return result;
}

CAMLprim value lt_term_set_size_from_fd(value fd, value val_size)
{
  struct winsize size;

  if (ioctl(Int_val(fd), TIOCGWINSZ, &size) < 0)
    uerror("ioctl", Nothing);

  int row = Int_val(Field(val_size, 0));
  int col = Int_val(Field(val_size, 1));

  size.ws_xpixel = size.ws_xpixel * col / size.ws_col;
  size.ws_ypixel = size.ws_ypixel * row / size.ws_row;
  size.ws_row = row;
  size.ws_col = col;

  if (ioctl(Int_val(fd), TIOCSWINSZ, &size) < 0)
    uerror("ioctl", Nothing);

  return Val_unit;
}

#endif
