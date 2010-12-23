/*
  sys.c
  I/O and operating system utility functions
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <libgen.h>
#include <fcntl.h>
#include <unistd.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

// --- system word size ---

int jl_word_size()
{
#ifdef BITS64
    return 64;
#else
    return 32;
#endif
}

// --- io and select ---

void jl__not__used__()
{
    // force inclusion of lib/socket.o in executable
    short p=0;
    open_any_tcp_port(&p);
}

DLLEXPORT
int jl_wait_msg(int fd)
{
    fd_set fds, efds;
    FD_ZERO(&fds);
    FD_ZERO(&efds);
    FD_SET(fd, &fds);
    FD_SET(fd, &efds);
    select(fd+1, &fds, NULL, &efds, NULL);
    if (FD_ISSET(fd, &efds)) {
        ios_printf(ios_stderr, "error fd\n");
        return 1;
    }
    if (FD_ISSET(fd, &fds)) {
        return 0;
    }
    return 1;
}

DLLEXPORT
int jl_read_avail(ios_t *s)
{
    int fd = s->fd;
    fd_set fds;
    struct timeval tout;
    tout.tv_sec = 0;
    tout.tv_usec = 0;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    select(fd+1, &fds, NULL, NULL, &tout);
    if (FD_ISSET(fd, &fds))
        return 1;
    return 0;
}

DLLEXPORT
uint32_t jl_getutf8(ios_t *s)
{
    uint32_t wc=0;
    ios_getutf8(s, &wc);
    return wc;
}

DLLEXPORT
int32_t jl_nb_available(ios_t *s)
{
    return (int32_t)(s->size - s->bpos);
}

// --- io constructors ---

DLLEXPORT int jl_sizeof_ios_t() { return sizeof(ios_t); }

// hack to expose ios_stdout to julia. we could create a new iostream pointing
// to stdout, but then there would be two buffers for one descriptor, and
// ios_stdout is used before julia IOStream is available, creating a potential
// mess.
DLLEXPORT jl_value_t *jl_stdout_stream()
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, sizeof(ios_t));
    a->data = (void*)ios_stdout;
    return (jl_value_t*)a;
}

// --- current output stream ---

jl_value_t *jl_current_output_stream_obj()
{
    return jl_current_task->state.ostream_obj;
}

ios_t *jl_current_output_stream()
{
    return jl_current_task->state.current_output_stream;
}

void jl_set_current_output_stream_obj(jl_value_t *v)
{
    jl_current_task->state.ostream_obj = v;
    jl_value_t *ptr = jl_convert((jl_type_t*)jl_pointer_void_type, v);
    jl_current_task->state.current_output_stream =
        (ios_t*)jl_unbox_pointer(ptr);
}

// --- buffer manipulation ---

// TODO: avoid the extra copy
jl_array_t *jl_takebuf_array(ios_t *s)
{
    size_t n;
    char *b = ios_takebuf(s, &n);
    jl_array_t *a = jl_pchar_to_array(b, n-1);
    LLT_FREE(b);
    return a;
}

jl_value_t *jl_takebuf_string(ios_t *s)
{
    size_t n;
    char *b = ios_takebuf(s, &n);
    jl_value_t *v = jl_pchar_to_string(b, n-1);
    LLT_FREE(b);
    return v;
}

// -- syscall utilities --

int jl_errno() { return errno; }

jl_value_t *jl_strerror(int errnum)
{
    char *str = strerror(errnum);
    return jl_pchar_to_string((char*)str, strlen(str));
}

// -- child process status --

int jl_process_exited(int status)      { return WIFEXITED(status); }
int jl_process_signaled(int status)    { return WIFSIGNALED(status); }
int jl_process_stopped(int status)     { return WIFSTOPPED(status); }

int jl_process_exit_status(int status) { return WEXITSTATUS(status); }
int jl_process_term_signal(int status) { return WTERMSIG(status); }
int jl_process_stop_signal(int status) { return WSTOPSIG(status); }

// -- access to std filehandles --

int jl_stdin()  { return STDIN_FILENO; }
int jl_stdout() { return STDOUT_FILENO; }
int jl_stderr() { return STDERR_FILENO; }
