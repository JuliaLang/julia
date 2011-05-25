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
#include <pthread.h>
#include "llt.h"
#include "julia.h"

// --- system word size ---

int jl_word_size()
{
#ifdef __LP64__
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

DLLEXPORT int jl_sizeof_fd_set() { return sizeof(fd_set); }

DLLEXPORT int jl_sizeof_timeval() { return sizeof(struct timeval); }

DLLEXPORT void jl_set_timeval(struct timeval *tv, double tout)
{
    tv->tv_sec = (int)tout;
    tv->tv_usec = (int)((tout-trunc(tout))*1.0e6);
}

DLLEXPORT void jl_fd_clr(fd_set *set, int fd)
{
    FD_CLR(fd, set);
}

DLLEXPORT int jl_fd_isset(fd_set *set, int fd)
{
    return FD_ISSET(fd, set);
}

DLLEXPORT void jl_fd_set(fd_set *set, int fd)
{
    FD_SET(fd, set);
}

DLLEXPORT void jl_fd_zero(fd_set *set)
{
    FD_ZERO(set);
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
size_t jl_ios_size(ios_t *s)
{
    return s->size;
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

DLLEXPORT ios_t *jl_current_output_stream()
{
    return jl_current_task->state.current_output_stream;
}

void jl_set_current_output_stream_obj(jl_value_t *v)
{
    jl_current_task->state.ostream_obj = v;
    jl_value_t *ptr = jl_convert((jl_type_t*)jl_pointer_void_type, v);
    jl_current_task->state.current_output_stream =
        (ios_t*)jl_unbox_pointer(ptr);
    // if current stream has never been set before, propagate to all
    // outer contexts.
    jl_savestate_t *ss = jl_current_task->state.prev;
    while (ss != NULL && ss->ostream_obj == (jl_value_t*)jl_null) {
        ss->ostream_obj = v;
        ss->current_output_stream = (ios_t*)jl_unbox_pointer(ptr);
        ss = ss->prev;
    }
}

// --- buffer manipulation ---

jl_array_t *jl_takebuf_array(ios_t *s)
{
    size_t n;
    jl_array_t *a;
    if (s->buf == &s->local[0]) {
        // small data case. copies, but this can be avoided using the
        // technique of jl_readuntil below.
        a = jl_pchar_to_array(s->buf, s->size);
        ios_trunc(s, 0);
    }
    else {
        assert(s->julia_alloc);
        char *b = ios_takebuf(s, &n);
        a = jl_alloc_array_1d(jl_array_uint8_type, 0);
        a->data = b;
        a->length = n-1;
        a->nrows = n-1;
        jl_gc_acquire_buffer(b);
    }
    return a;
}

jl_value_t *jl_takebuf_string(ios_t *s)
{
    jl_array_t *a = jl_takebuf_array(s);
    JL_GC_PUSH(&a);
    jl_value_t *str = jl_array_to_string(a);
    JL_GC_POP();
    return str;
}

jl_array_t *jl_readuntil(ios_t *s, uint8_t delim)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, 80);
    ios_t dest;
    jl_ios_mem(&dest, 0);
    ios_setbuf(&dest, a->data, 80, 0);
    size_t n = ios_copyuntil(&dest, s, delim);
    if (dest.buf != a->data) {
        return jl_takebuf_array(&dest);
    }
    else {
        a->length = n;
        a->nrows = n;
        ((char*)a->data)[n] = '\0';
    }
    return a;
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

// I/O thread

static pthread_t io_thread;
static pthread_mutex_t q_mut;
static pthread_mutex_t wake_mut;
static pthread_cond_t wake_cond;

typedef struct _sendreq_t {
    int fd;
    void *buf;
    size_t n;
    struct _sendreq_t *next;
} sendreq_t;

static sendreq_t *ioq = NULL;

int _os_write_all(long fd, void *buf, size_t n, size_t *nwritten);

static void *run_io_thr(void *arg)
{
    while (1) {
        pthread_mutex_lock(&wake_mut);
        pthread_cond_wait(&wake_cond, &wake_mut);
        pthread_mutex_unlock(&wake_mut);

        pthread_mutex_lock(&q_mut);
        while (ioq != NULL) {
            sendreq_t *r = ioq;
            ioq = ioq->next;
            pthread_mutex_unlock(&q_mut);
            size_t nw;
            _os_write_all(r->fd, r->buf, r->n, &nw);
            LLT_FREE(r->buf);
            free(r);
            pthread_mutex_lock(&q_mut);
        }
        pthread_mutex_unlock(&q_mut);
    }
    return NULL;
}

DLLEXPORT
void jl_enq_send_req(ios_t *dest, ios_t *buf)
{
    sendreq_t *req = (sendreq_t*)malloc(sizeof(sendreq_t));
    req->fd = dest->fd;
    size_t sz;
    req->n = buf->size;
    req->buf = ios_takebuf(buf, &sz);
    req->next = NULL;
    pthread_mutex_lock(&q_mut);
    if (ioq == NULL) {
        ioq = req;
    }
    else {
        sendreq_t *r = ioq;
        while (r->next != NULL) {
            r = r->next;
        }
        r->next = req;
    }
    pthread_mutex_unlock(&q_mut);
    pthread_cond_signal(&wake_cond);
}

DLLEXPORT
void jl_start_io_thread()
{
    pthread_mutex_init(&q_mut, NULL);
    pthread_mutex_init(&wake_mut, NULL);
    pthread_cond_init(&wake_cond, NULL);
    pthread_create(&io_thread, NULL, run_io_thr, NULL);
}
