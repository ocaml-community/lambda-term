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
  CONSOLE_SCREEN_BUFFER_INFO info;
  value result;

  if (!GetConsoleScreenBufferInfo(Handle_val(fd), &info)) {
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
  CONSOLE_SCREEN_BUFFER_INFO info;
  SMALL_RECT rect;

  if (!GetConsoleScreenBufferInfo(Handle_val(fd), &info)) {
    win32_maperr(GetLastError());
    uerror("GetConsoleScreenBufferInfo", Nothing);
  }

  rect;
  rect.Top = info.srWindow.Top;
  rect.Left = info.srWindow.Left;
  rect.Bottom = rect.Top + Int_val(Field(val_size, 0)) - 1;
  rect.Right = rect.Left + Int_val(Field(val_size, 1)) - 1;

  if (!SetConsoleWindowInfo(Handle_val(fd), TRUE, &rect)) {
    win32_maperr(GetLastError());
    uerror("SetConsoleWindowInfo", Nothing);
  }

  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Spawning a process on windows                                   |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_term_spawn(value cmdline, value hstdin, value hstdout)
{
  CAMLparam1(cmdline);
  CAMLlocal1(result);

  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  if (Descr_kind_val(hstdin) == KIND_SOCKET || Descr_kind_val(hstdout) == KIND_SOCKET)  {
    win32_maperr(ERROR_INVALID_HANDLE);
    uerror("CreateProcess", Nothing);
  }

  ZeroMemory(&si, sizeof(si));
  ZeroMemory(&pi, sizeof(pi));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = Handle_val(hstdin);
  si.hStdOutput = Handle_val(hstdout);
  si.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  if (!CreateProcess(NULL, String_val(cmdline), NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
    win32_maperr(GetLastError());
    uerror("CreateProcess", Nothing);
  }

  CloseHandle(pi.hThread);

  result = caml_alloc(1, 1);
  Store_field(result, 0, win_alloc_handle(pi.hProcess));
  CAMLreturn(result);
}

struct job_waitproc {
  struct lwt_unix_job job;
  HANDLE handle;
};

#define Job_waitproc_val(v) *(struct job_waitproc**)Data_custom_val(v)

static void worker_waitproc(struct job_waitproc *job)
{
  WaitForSingleObject(job->handle, INFINITE);
}

CAMLprim value lt_term_waitproc_job(value handle)
{
  struct job_waitproc *job = lwt_unix_new(struct job_waitproc);
  job->job.worker = (lwt_unix_job_worker)worker_waitproc;
  job->handle = Handle_val(handle);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lt_term_waitproc_free(value val_job)
{
  lwt_unix_free_job(&(Job_waitproc_val(val_job))->job);
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

/* +-----------------------------------------------------------------+
   | Spawning a process on unix                                      |
   +-----------------------------------------------------------------+ */

CAMLprim value lt_term_spawn(value cmdline, value fdin, value fdout)
{
  CAMLparam1(cmdline);
  CAMLlocal1(result);

  int pid = fork();

  if (pid == 0) {
    dup2(Int_val(fdin), STDIN_FILENO);
    dup2(Int_val(fdout), STDOUT_FILENO);
    close(Int_val(fdin));
    close(Int_val(fdout));
    execl("/bin/sh", "/bin/sh", "-c", String_val(cmdline), NULL);
    exit(127);
  }

  if (pid < 0) uerror("fork", Nothing);

  result = caml_alloc(1, 0);
  Field(result, 0) = Val_int(pid);
  CAMLreturn(result);
}

#define NA(name, feature)                       \
  CAMLprim value lt_term_##name()               \
  {                                             \
    lwt_unix_not_available(feature);            \
    return Val_unit;                            \
  }

NA(waitproc_job, "WaitForSingleObject")
NA(waitproc_free, "WaitForSingleObject")

#endif
