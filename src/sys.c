/*
  sys.c
  I/O and operating system utility functions
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/sysctl.h>
#include <errno.h>
#include <signal.h>
#include <libgen.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include "julia.h"

// --- io and select ---

void jl__not__used__(void)
{
    // force inclusion of lib/socket.o in executable
    short p=0;
    open_any_tcp_port(&p);
}

DLLEXPORT int jl_sizeof_fd_set(void) { return sizeof(fd_set); }

DLLEXPORT int jl_sizeof_timeval(void) { return sizeof(struct timeval); }

DLLEXPORT void jl_set_timeval(struct timeval *tv, double tout)
{
    tv->tv_sec = (int)tout;
    tv->tv_usec = (int)((tout-(int)tout)*1.0e6);
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

DLLEXPORT uint32_t jl_getutf8(ios_t *s)
{
    uint32_t wc=0;
    ios_getutf8(s, &wc);
    return wc;
}

DLLEXPORT size_t jl_ios_size(ios_t *s)
{
    return s->size;
}

DLLEXPORT int jl_sizeof_off_t(void) { return sizeof(off_t); }

DLLEXPORT long jl_ios_fd(ios_t *s)
{
    return s->fd;
}

DLLEXPORT int32_t jl_nb_available(ios_t *s)
{
    return (int32_t)(s->size - s->bpos);
}

DLLEXPORT int jl_ios_eof(ios_t *s)
{
    if (ios_eof(s))
        return 1;
    if (s->state == bst_rd) {
        if (ios_readprep(s, 1) < 1)
            return 1;
    }
    return 0;
}

// --- io constructors ---

DLLEXPORT int jl_sizeof_ios_t(void) { return sizeof(ios_t); }

// hack to expose ios_stdout to julia. we could create a new iostream pointing
// to stdout, but then there would be two buffers for one descriptor, and
// ios_stdout is used before julia IOStream is available, creating a potential
// mess.
DLLEXPORT jl_value_t *jl_stdout_stream(void)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_uint8_type, sizeof(ios_t));
    a->data = (void*)ios_stdout;
    return (jl_value_t*)a;
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
        a = jl_ptr_to_array_1d(jl_array_uint8_type, b, n-1, 1);
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

jl_value_t *jl_readuntil(ios_t *s, uint8_t delim)
{
    jl_array_t *a;
    // manually inlined common case
    char *pd = (char*)memchr(s->buf+s->bpos, delim, s->size - s->bpos);
    if (pd) {
        size_t n = pd-(s->buf+s->bpos)+1;
        a = jl_alloc_array_1d(jl_array_uint8_type, n);
        memcpy(jl_array_data(a), s->buf+s->bpos, n);
        s->bpos += n;
    }
    else {
        a = jl_alloc_array_1d(jl_array_uint8_type, 80);
        ios_t dest;
        jl_ios_mem(&dest, 0);
        ios_setbuf(&dest, a->data, 80, 0);
        size_t n = ios_copyuntil(&dest, s, delim);
        if (dest.buf != a->data) {
            a = jl_takebuf_array(&dest);
        }
        else {
            a->length = n;
            a->nrows = n;
            ((char*)a->data)[n] = '\0';
        }
    }
    JL_GC_PUSH(&a);
    jl_struct_type_t* string_type = u8_isvalid(a->data, a->length) == 1 ? // ASCII
        jl_ascii_string_type : jl_utf8_string_type;
    jl_value_t *str = alloc_2w();
    str->type = (jl_type_t*)string_type;
    jl_fieldref(str,0) = (jl_value_t*)a;
    JL_GC_POP();
    return str;
}

// -- syscall utilities --

int jl_errno(void) { return errno; }

jl_value_t *jl_strerror(int errnum)
{
    char *str = strerror(errnum);
    return jl_pchar_to_string((char*)str, strlen(str));
}

// -- get the number of CPU cores --

DLLEXPORT int jl_cpu_cores(void) {
#if defined(__APPLE__)
    size_t len = 4;
    int32_t count;
    int nm[2] = {CTL_HW, HW_AVAILCPU};
    sysctl(nm, 2, &count, &len, NULL, 0);
    if (count < 1) {
        nm[1] = HW_NCPU;
        sysctl(nm, 2, &count, &len, NULL, 0);
        if (count < 1) { count = 1; }
    }
    return count;
#elif defined(__linux)
    return sysconf(_SC_NPROCESSORS_ONLN);
#else // test for Windows?
    return GetActiveProcessorCount(__in WORD GroupNumber);
#endif
}

// -- iterating the environment --

#ifdef __APPLE__
#include <crt_externs.h>
#else
extern char **environ;
#endif

jl_value_t *jl_environ(int i)
{
#ifdef __APPLE__
    char **environ = *_NSGetEnviron();
#endif
    char *env = environ[i];
    return env ? jl_pchar_to_string(env, strlen(env)) : jl_nothing;
}

// -- child process status --

int jl_process_exited(int status)      { return WIFEXITED(status); }
int jl_process_signaled(int status)    { return WIFSIGNALED(status); }
int jl_process_stopped(int status)     { return WIFSTOPPED(status); }

int jl_process_exit_status(int status) { return WEXITSTATUS(status); }
int jl_process_term_signal(int status) { return WTERMSIG(status); }
int jl_process_stop_signal(int status) { return WSTOPSIG(status); }

// -- access to std filehandles --

int jl_stdin(void)  { return STDIN_FILENO; }
int jl_stdout(void) { return STDOUT_FILENO; }
int jl_stderr(void) { return STDERR_FILENO; }

// -- I/O thread --

static pthread_t io_thread;
static pthread_mutex_t q_mut;
static pthread_mutex_t wake_mut;
static pthread_cond_t wake_cond;

typedef struct _sendreq_t {
    int fd;
    ios_t *buf;
    int now;
    struct _sendreq_t *next;
} sendreq_t;

static sendreq_t *ioq = NULL;
static sendreq_t *ioq_freelist = NULL;

int _os_write_all(long fd, void *buf, size_t n, size_t *nwritten);

static void *run_io_thr(void *arg)
{
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGFPE);
    sigaddset(&set, SIGINT);
    sigaddset(&set, SIGSEGV);
    pthread_sigmask(SIG_BLOCK, &set, NULL);

    while (1) {
        while (ioq == NULL) {
            pthread_mutex_lock(&wake_mut);
            pthread_cond_wait(&wake_cond, &wake_mut);
            pthread_mutex_unlock(&wake_mut);
        }
        assert(ioq != NULL);

        pthread_mutex_lock(&q_mut);
        sendreq_t *r = ioq;
        ioq = ioq->next;
        pthread_mutex_unlock(&q_mut);

        if (!r->now) {
            int64_t now = (int64_t)(clock_now()*1e6);
            int64_t waittime = r->buf->userdata+200-now;  // microseconds
            if (waittime > 0) {
                struct timespec wt;
                wt.tv_sec = 0;
                wt.tv_nsec = waittime * 1000;
                nanosleep(&wt, NULL);
            }
        }

        pthread_mutex_lock(&r->buf->mutex);
        size_t sz;
        size_t n = r->buf->size;
        char *buf = ios_takebuf(r->buf, &sz);
        pthread_mutex_unlock(&r->buf->mutex);

        size_t nw;
        _os_write_all(r->fd, buf, n, &nw);
        julia_free(buf);

        pthread_mutex_lock(&q_mut);
        r->next = ioq_freelist;
        ioq_freelist = r;
        pthread_mutex_unlock(&q_mut);
    }
    return NULL;
}

DLLEXPORT void jl_buf_mutex_lock(ios_t *s)
{
    if (!s->mutex_initialized) {
        pthread_mutex_init(&s->mutex, NULL);
        s->mutex_initialized = 1;
    }
    pthread_mutex_lock(&s->mutex);
}

DLLEXPORT void jl_buf_mutex_unlock(ios_t *s)
{
    pthread_mutex_unlock(&s->mutex);
}

DLLEXPORT void jl_enq_send_req(ios_t *dest, ios_t *buf, int now)
{
    pthread_mutex_lock(&q_mut);
    sendreq_t *req = ioq;
    sendreq_t **pr = &ioq;
    while (req != NULL) {
        if (req->fd == dest->fd) {
            if (now && !req->now) {
                // increase priority
                *pr = req->next;
                req->next = ioq;
                ioq = req;
                req->now = 1;
            }
            pthread_mutex_unlock(&q_mut);
            return;
        }
        pr = &req->next;
        req = req->next;
    }

    if (ioq_freelist != NULL) {
        req = ioq_freelist;
        ioq_freelist = ioq_freelist->next;
    }
    else {
        req = (sendreq_t*)malloc(sizeof(sendreq_t));
    }
    req->fd = dest->fd;
    req->buf = buf;
    req->now = now;
    req->next = NULL;
    buf->userdata = (int64_t)(clock_now()*1e6);
    if (ioq == NULL) {
        ioq = req;
    }
    else {
        if (now) {
            req->next = ioq;
            ioq = req;
        }
        else {
            sendreq_t *r = ioq;
            while (r->next != NULL) {
                r = r->next;
            }
            r->next = req;
        }
    }
    pthread_mutex_unlock(&q_mut);
    pthread_cond_signal(&wake_cond);
}

DLLEXPORT void jl_start_io_thread(void)
{
    pthread_mutex_init(&q_mut, NULL);
    pthread_mutex_init(&wake_mut, NULL);
    pthread_cond_init(&wake_cond, NULL);
    pthread_create(&io_thread, NULL, run_io_thr, NULL);
}
