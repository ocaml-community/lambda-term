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
  CAMLlocal2(result, ch);
  struct job_read_console_input *job = Job_read_console_input_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("ReadConsoleInput", Nothing);
  }
  INPUT_RECORD *input = &(job->input);
  switch (input->EventType) {
  case KEY_EVENT: {
    DWORD cks = input->Event.KeyEvent.dwControlKeyState;
    result = caml_alloc(3, 0);
    Field(result, 0) = Val_bool((cks & LEFT_CTRL_PRESSED) | (cks & RIGHT_CTRL_PRESSED));
    Field(result, 1) = Val_bool((cks & LEFT_ALT_PRESSED) | (cks & RIGHT_ALT_PRESSED));
    WORD code = input->Event.KeyEvent.wVirtualKeyCode;
    int i;
    for (i = 0; i < sizeof(code_table)/sizeof(code_table[0]); i++)
      if (code == code_table[i]) {
        Field(result, 2) = Val_int(i);
        CAMLreturn(result);
      }
    ch = caml_alloc_tuple(1);
    Field(ch, 0) = Val_int(input->Event.KeyEvent.uChar.UnicodeChar);
    Field(result, 2) = ch;
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

#endif
