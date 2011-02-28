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
#include <lwt_unix.h>

/* +-----------------------------------------------------------------+
   | Codepage functions                                              |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_windows_get_acp()
{
  return Val_int(GetACP());
}

CAMLprim value lt_windows_get_console_cp()
{
  return Val_int(GetConsoleCP());
}

CAMLprim value lt_windows_set_console_cp(value cp)
{
  if (!SetConsoleCP(Int_val(cp))) {
    win32_maperr(GetLastError());
    uerror("SetConsoleCP", Nothing);
  }
  return Val_unit;
}

CAMLprim value lt_windows_get_console_output_cp()
{
  return Val_int(GetConsoleOutputCP());
}

CAMLprim value lt_windows_set_console_output_cp(value cp)
{
  if (!SetConsoleOutputCP(Int_val(cp))) {
    win32_maperr(GetLastError());
    uerror("SetConsoleOutputCP", Nothing);
  }
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Console input                                                   |
   +-----------------------------------------------------------------+ */

static WORD code_table[] = {
  VK_RETURN,
  VK_ESCAPE,
  VK_TAB,
  VK_UP,
  VK_DOWN,
  VK_LEFT,
  VK_RIGHT,
  VK_F1,
  VK_F2,
  VK_F3,
  VK_F4,
  VK_F5,
  VK_F6,
  VK_F7,
  VK_F8,
  VK_F9,
  VK_F10,
  VK_F11,
  VK_F12,
  VK_NEXT,
  VK_PRIOR,
  VK_HOME,
  VK_END,
  VK_INSERT,
  VK_DELETE,
  VK_BACK
};

struct job_read_console_input {
  struct lwt_unix_job job;
  HANDLE handle;
  INPUT_RECORD input;
  DWORD error_code;
};

#define Job_read_console_input_val(v) *(struct job_read_console_input**)Data_custom_val(v)

static void worker_read_console_input(struct job_read_console_input *job)
{
  DWORD event_count;
  INPUT_RECORD *input = &(job->input);

  for (;;) {
    if (!ReadConsoleInput(job->handle, input, 1, &event_count)) {
      job->error_code = GetLastError();
      return;
    }

    switch (input->EventType) {
    case KEY_EVENT:
      if (input->Event.KeyEvent.bKeyDown) {
        if (input->Event.KeyEvent.uChar.UnicodeChar)
          return;
        WORD code = input->Event.KeyEvent.wVirtualKeyCode;
        int i;
        for (i = 0; i < sizeof(code_table)/sizeof(code_table[0]); i++)
          if (code == code_table[i])
            return;
      }
      break;
    case MOUSE_EVENT: {
      DWORD bs = input->Event.MouseEvent.dwButtonState;
      if (!(input->Event.MouseEvent.dwEventFlags & MOUSE_MOVED) &&
          bs & (FROM_LEFT_1ST_BUTTON_PRESSED |
                FROM_LEFT_2ND_BUTTON_PRESSED |
                FROM_LEFT_3RD_BUTTON_PRESSED |
                FROM_LEFT_4TH_BUTTON_PRESSED |
                RIGHTMOST_BUTTON_PRESSED))
        return;
      break;
    }
    case WINDOW_BUFFER_SIZE_EVENT:
      return;
    }
  }
}

CAMLprim value lt_windows_read_console_input_job(value val_fd)
{
  struct job_read_console_input *job = lwt_unix_new(struct job_read_console_input);
  job->job.worker = (lwt_unix_job_worker)worker_read_console_input;
  job->handle = Handle_val(val_fd);
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lt_windows_read_console_input_result(value val_job)
{
  CAMLparam1(val_job);
  CAMLlocal3(result, x, y);
  struct job_read_console_input *job = Job_read_console_input_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("ReadConsoleInput", Nothing);
  }
  INPUT_RECORD *input = &(job->input);
  switch (input->EventType) {
  case KEY_EVENT: {
    result = caml_alloc(1, 0);
    x = caml_alloc_tuple(3);
    Field(result, 0) = x;
    DWORD cks = input->Event.KeyEvent.dwControlKeyState;
    Field(x, 0) = Val_bool((cks & LEFT_CTRL_PRESSED) | (cks & RIGHT_CTRL_PRESSED));
    Field(x, 1) = Val_bool((cks & LEFT_ALT_PRESSED) | (cks & RIGHT_ALT_PRESSED));
    WORD code = input->Event.KeyEvent.wVirtualKeyCode;
    int i;
    for (i = 0; i < sizeof(code_table)/sizeof(code_table[0]); i++)
      if (code == code_table[i]) {
        Field(x, 2) = Val_int(i);
        CAMLreturn(result);
      }
    y = caml_alloc_tuple(1);
    Field(y, 0) = Val_int(input->Event.KeyEvent.uChar.UnicodeChar);
    Field(x, 2) = y;
    CAMLreturn(result);
  }
  case MOUSE_EVENT: {
    result = caml_alloc(1, 1);
    x = caml_alloc_tuple(5);
    Field(result, 0) = x;
    DWORD cks = input->Event.MouseEvent.dwControlKeyState;
    Field(x, 0) = Val_bool((cks & LEFT_CTRL_PRESSED) | (cks & RIGHT_CTRL_PRESSED));
    Field(x, 1) = Val_bool((cks & LEFT_ALT_PRESSED) | (cks & RIGHT_ALT_PRESSED));
    Field(x, 3) = Val_int(input->Event.MouseEvent.dwMousePosition.Y);
    Field(x, 4) = Val_int(input->Event.MouseEvent.dwMousePosition.X);
    DWORD bs = input->Event.MouseEvent.dwButtonState;
    if (bs & FROM_LEFT_1ST_BUTTON_PRESSED)
      Field(x, 2) = Val_int(0);
    else if (bs & FROM_LEFT_2ND_BUTTON_PRESSED)
      Field(x, 2) = Val_int(1);
    else if (bs & FROM_LEFT_3RD_BUTTON_PRESSED)
      Field(x, 2) = Val_int(2);
    else if (bs & FROM_LEFT_4TH_BUTTON_PRESSED)
      Field(x, 2) = Val_int(3);
    else
      Field(x, 2) = Val_int(4);
    CAMLreturn(result);
  }
  case WINDOW_BUFFER_SIZE_EVENT:
    CAMLreturn(Val_int(0));
  }
  CAMLreturn(Val_int(0));
}

CAMLprim value lt_windows_read_console_input_free(value val_job)
{
  lwt_unix_free_job(&(Job_read_console_input_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Console informations                                            |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_windows_get_console_screen_buffer_info(value val_fd)
{
  CAMLparam1(val_fd);
  CAMLlocal2(result, x);

  CONSOLE_SCREEN_BUFFER_INFO info;

  if (!GetConsoleScreenBufferInfo(Handle_val(val_fd), &info)) {
    win32_maperr(GetLastError());
    uerror("GetConsoleScreenBufferInfo", Nothing);
  }

  result = caml_alloc_tuple(5);

  x = caml_alloc_tuple(2);
  Field(x, 0) = Val_int(info.dwSize.Y);
  Field(x, 1) = Val_int(info.dwSize.X);
  Field(result, 0) = x;

  x = caml_alloc_tuple(4);
  Field(x, 0) = Val_int(info.dwCursorPosition.Y);
  Field(x, 1) = Val_int(info.dwCursorPosition.X);
  Field(result, 1) = x;

  x = caml_alloc_tuple(2);
  int color = 0;
  if (info.wAttributes & FOREGROUND_RED) color |= 1;
  if (info.wAttributes & FOREGROUND_GREEN) color |= 2;
  if (info.wAttributes & FOREGROUND_BLUE) color |= 4;
  if (info.wAttributes & FOREGROUND_INTENSITY) color |= 8;
  Field(x, 0) = Val_int(color);
  color = 0;
  if (info.wAttributes & BACKGROUND_RED) color |= 1;
  if (info.wAttributes & BACKGROUND_GREEN) color |= 2;
  if (info.wAttributes & BACKGROUND_BLUE) color |= 4;
  if (info.wAttributes & BACKGROUND_INTENSITY) color |= 8;
  Field(x, 1) = Val_int(color);
  Field(result, 2) = x;

  x = caml_alloc_tuple(4);
  Field(x, 0) = Val_int(info.srWindow.Top);
  Field(x, 1) = Val_int(info.srWindow.Left);
  Field(x, 2) = Val_int(info.srWindow.Bottom - info.srWindow.Top);
  Field(x, 3) = Val_int(info.srWindow.Right - info.srWindow.Left);
  Field(result, 3) = x;

  x = caml_alloc_tuple(2);
  Field(x, 0) = Val_int(info.dwMaximumWindowSize.Y);
  Field(x, 1) = Val_int(info.dwMaximumWindowSize.X);
  Field(result, 4) = x;

  CAMLreturn(result);
}

/* +-----------------------------------------------------------------+
   | Cursor                                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_windows_get_console_cursor_info(value val_fd)
{
  CONSOLE_CURSOR_INFO info;
  if (!GetConsoleCursorInfo(Handle_val(val_fd), &info)) {
    win32_maperr(GetLastError());
    uerror("GetConsoleCursorInfo", Nothing);
  }
  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(info.dwSize);
  Field(result, 1) = Val_bool(info.bVisible);
  return result;
}

CAMLprim value lt_windows_set_console_cursor_info(value val_fd, value val_size, value val_visible)
{
  CONSOLE_CURSOR_INFO info;
  info.dwSize = Int_val(val_size);
  info.bVisible = Bool_val(val_visible);
  if (!SetConsoleCursorInfo(Handle_val(val_fd), &info)) {
    win32_maperr(GetLastError());
    uerror("SetConsoleCursorInfo", Nothing);
  }
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Text attributes                                                 |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_windows_set_console_text_attribute(value val_fd, value val_attrs)
{
  int fg = Int_val(Field(val_attrs, 0));
  int bg = Int_val(Field(val_attrs, 1));
  WORD attrs = 0;

  if (fg & 1) attrs |= FOREGROUND_RED;
  if (fg & 2) attrs |= FOREGROUND_GREEN;
  if (fg & 4) attrs |= FOREGROUND_BLUE;
  if (fg & 8) attrs |= FOREGROUND_INTENSITY;

  if (bg & 1) attrs |= BACKGROUND_RED;
  if (bg & 2) attrs |= BACKGROUND_GREEN;
  if (bg & 4) attrs |= BACKGROUND_BLUE;
  if (bg & 8) attrs |= BACKGROUND_INTENSITY;

  if (!SetConsoleTextAttribute(Handle_val(val_fd), attrs)) {
    win32_maperr(GetLastError());
    uerror("SetConsoleTextAttribute", Nothing);
  }
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
NA(read_console_input_job, "ReadConsoleInput")
NA(read_console_input_result, "ReadConsoleInput")
NA(read_console_input_free, "ReadConsoleInput")
NA(set_console_text_attribute, "SetConsoleTextAttribute")
NA(get_console_screen_buffer_info, "GetConsoleScreenBufferInfo")
NA(get_console_cursor_info, "GetConsoleCursorInfo")
NA(set_console_cursor_info, "SetConsoleCursorInfo")

#endif
