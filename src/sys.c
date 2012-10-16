/*
  sys.c
  I/O and operating system utility functions
*/
#include "julia.h"
#include "uv.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>
#ifndef __WIN32__
#include <sys/sysctl.h>
#include <sys/wait.h>
#endif
#include <errno.h>
#include <signal.h>
#include <libgen.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>

#ifdef __SSE__
#include <xmmintrin.h>
#endif

// --- io and select ---

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
    jl_array_data_owner(a) = (jl_value_t*)a;
    return (jl_value_t*)a;
}

// --- dir/file stuff ---

DLLEXPORT int jl_sizeof_uv_fs_t(void) { return sizeof(uv_fs_t); }
DLLEXPORT void jl_uv_fs_req_cleanup(uv_fs_t* req) {
  uv_fs_req_cleanup(req);
}

DLLEXPORT int jl_readdir(const char* path, uv_fs_t* readdir_req)
{
  // Note that the flags field is mostly ignored by libuv
  return uv_fs_readdir(uv_default_loop(), readdir_req, path, 0 /*flags*/, NULL);
}

DLLEXPORT char* jl_uv_fs_t_ptr(uv_fs_t* req) {return req->ptr; }
DLLEXPORT char* jl_uv_fs_t_ptr_offset(uv_fs_t* req, int offset) {return req->ptr + offset; }

// --- stat ---
DLLEXPORT int jl_sizeof_stat(void) { return sizeof(struct stat); }

DLLEXPORT int32_t jl_stat(const char* path, char* statbuf)
{
  uv_fs_t req;
  int ret;

  // Ideally one would use the statbuf for the storage in req, but
  // it's not clear that this is possible using libuv
  ret = uv_fs_stat(uv_default_loop(), &req, path, NULL);
  if (ret == 0)
    memcpy(statbuf, req.ptr, sizeof(struct stat));
  uv_fs_req_cleanup(&req);
  return ret;
}

DLLEXPORT int32_t jl_lstat(const char* path, char* statbuf)
{
  uv_fs_t req;
  int ret;

  ret = uv_fs_lstat(uv_default_loop(), &req, path, NULL);
  if (ret == 0)
    memcpy(statbuf, req.ptr, sizeof(struct stat));
  uv_fs_req_cleanup(&req);
  return ret;
}

DLLEXPORT int32_t jl_fstat(int fd, char *statbuf)
{
  uv_fs_t req;
  int ret;

  ret = uv_fs_fstat(uv_default_loop(), &req, fd, NULL);
  if (ret == 0)
    memcpy(statbuf, req.ptr, sizeof(struct stat));
  uv_fs_req_cleanup(&req);
  return ret;
}

DLLEXPORT unsigned int jl_stat_dev(char *statbuf)
{
  return ((struct stat*) statbuf)->st_dev;
}

DLLEXPORT unsigned int jl_stat_ino(char *statbuf)
{
  return ((struct stat*) statbuf)->st_ino;
}

DLLEXPORT unsigned int jl_stat_mode(char *statbuf)
{
  return ((struct stat*) statbuf)->st_mode;
}

DLLEXPORT unsigned int jl_stat_nlink(char *statbuf)
{
  return ((struct stat*) statbuf)->st_nlink;
}

DLLEXPORT unsigned int jl_stat_uid(char *statbuf)
{
  return ((struct stat*) statbuf)->st_uid;
}

DLLEXPORT unsigned int jl_stat_gid(char *statbuf)
{
  return ((struct stat*) statbuf)->st_gid;
}

DLLEXPORT unsigned int jl_stat_rdev(char *statbuf)
{
  return ((struct stat*) statbuf)->st_rdev;
}

DLLEXPORT off_t jl_stat_size(char *statbuf)
{
  return ((struct stat*) statbuf)->st_size;
}

DLLEXPORT unsigned int jl_stat_blksize(char *statbuf)
{
  return ((struct stat*) statbuf)->st_blksize;
}

DLLEXPORT unsigned int jl_stat_blocks(char *statbuf)
{
  return ((struct stat*) statbuf)->st_blocks;
}

#if defined(__APPLE__) || defined(__FreeBSD__)
#define st_ATIM st_atimespec
#define st_MTIM st_mtimespec
#define st_CTIM st_ctimespec
#else
#define st_ATIM st_atim
#define st_MTIM st_mtim
#define st_CTIM st_ctim
#endif

/*
// atime is stupid, let's not support it
DLLEXPORT double jl_stat_atime(char *statbuf)
{
  struct stat *s;
  s = (struct stat*) statbuf;
  return (double)s->st_ATIM.tv_sec + (double)s->st_ATIM.tv_nsec * 1e-9;
}
*/

DLLEXPORT double jl_stat_mtime(char *statbuf)
{
  struct stat *s;
  s = (struct stat*) statbuf;
  return (double)s->st_MTIM.tv_sec + (double)s->st_MTIM.tv_nsec * 1e-9;
}

DLLEXPORT double jl_stat_ctime(char *statbuf)
{
  struct stat *s;
  s = (struct stat*) statbuf;
  return (double)s->st_CTIM.tv_sec + (double)s->st_CTIM.tv_nsec * 1e-9;
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

// the returned buffer must be manually freed. To determine the size,
// call position(s) before using this function.
void *jl_takebuf_raw(ios_t *s)
{
    size_t sz;
    void *buf = ios_takebuf(s, &sz);
    return buf;
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
        ios_mem(&dest, 0);
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
    jl_set_nth_field(str, 0, (jl_value_t*)a);
    JL_GC_POP();
    return str;
}

void jl_free2(void *p, void *hint)
{
    free(p);
}

// -- syscall utilities --

int jl_errno(void) { return errno; }

// -- get the number of CPU cores --

#ifdef __WIN32__
typedef DWORD (WINAPI *GAPC)(WORD);
#ifndef ALL_PROCESSOR_GROUPS
#define ALL_PROCESSOR_GROUPS 0xffff
#endif
#endif

DLLEXPORT int jl_cpu_cores(void)
{
#if defined(HW_AVAILCPU) && defined(HW_NCPU)
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
#elif defined(_SC_NPROCESSORS_ONLN)
    return sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(__WIN32__)
    //Try to get WIN7 API method
    GAPC gapc = (GAPC) jl_dlsym(
        jl_kernel32_handle,
        "GetActiveProcessorCount"
    );

    if (gapc) {
        return gapc(ALL_PROCESSOR_GROUPS);
    } else { //fall back on GetSystemInfo
        SYSTEM_INFO info;
        GetSystemInfo(&info);
        return info.dwNumberOfProcessors;
    }
#else
    return 1;
#endif
}

// -- high resolution timers --
// Returns time in nanosec
DLLEXPORT uint64_t jl_hrtime(void)
{
  return uv_hrtime();
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

#if defined _MSC_VER || defined __MINGW32__

/* Native Woe32 API.  */
#include <process.h>
#define waitpid(pid,statusp,options) _cwait (statusp, pid, WAIT_CHILD)
#define WAIT_T int
#define WTERMSIG(x) ((x) & 0xff) /* or: SIGABRT ?? */
#define WCOREDUMP(x) 0
#define WEXITSTATUS(x) (((x) >> 8) & 0xff) /* or: (x) ?? */
#define WIFSIGNALED(x) (WTERMSIG (x) != 0) /* or: ((x) == 3) ?? */
#define WIFEXITED(x) (WTERMSIG (x) == 0) /* or: ((x) != 3) ?? */
#define WIFSTOPPED(x) 0
#define WSTOPSIG(x) 0 //Is this correct?
#endif

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
        free(buf);

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
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 262144);
    pthread_create(&io_thread, &attr, run_io_thr, NULL);
}

DLLEXPORT uint8_t jl_zero_denormals(uint8_t isZero)
{
#ifdef __SSE2__
    // SSE2 supports both FZ and DAZ
    uint32_t flags = 0x8040;
#elif __SSE__
    // SSE supports only the FZ flag
    uint32_t flags = 0x8000;
#endif

#ifdef __SSE__
    if (isZero) {
	_mm_setcsr(_mm_getcsr() | flags);
    }
    else {
	_mm_setcsr(_mm_getcsr() & ~flags);
    }
    return 1;
#else
    return 0;
#endif
}
