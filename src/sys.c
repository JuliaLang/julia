// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  sys.c
  I/O and operating system utility functions
*/
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <stdio.h>

#include "julia.h"
#include "julia_internal.h"

#ifdef _OS_WINDOWS_
#include <psapi.h>
#else
#include <unistd.h>
#if !defined(_SC_NPROCESSORS_ONLN) || defined(_OS_FREEBSD_) || defined(_OS_DARWIN_)
// try secondary location for _SC_NPROCESSORS_ONLN, or for HW_AVAILCPU on BSDs
#include <sys/sysctl.h>
#endif
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <grp.h>

// For `struct termios`
#include <termios.h>
#endif

#ifndef _OS_WINDOWS_
// for getrusage
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <mach-o/nlist.h>
#include <sys/types.h> // for jl_raise_debugger
#elif !defined(_OS_WINDOWS_)
#include <link.h>
#endif

#ifdef __SSE__
#include <xmmintrin.h>
#endif

#ifdef _COMPILER_MSAN_ENABLED_
#include <sanitizer/msan_interface.h>
#endif

#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int jl_sizeof_off_t(void) { return sizeof(off_t); }
#ifndef _OS_WINDOWS_
JL_DLLEXPORT int jl_sizeof_mode_t(void) { return sizeof(mode_t); }
JL_DLLEXPORT int jl_ftruncate(int fd, int64_t length)
{
    return ftruncate(fd, (off_t)length);
}
JL_DLLEXPORT int64_t jl_lseek(int fd, int64_t offset, int whence)
{
    return lseek(fd, (off_t)offset, whence);
}
JL_DLLEXPORT ssize_t jl_pwrite(int fd, const void *buf, size_t count, int64_t offset)
{
    return pwrite(fd, buf, count, (off_t)offset);
}
JL_DLLEXPORT void *jl_mmap(void *addr, size_t length, int prot, int flags,
                           int fd, int64_t offset)
{
    return mmap(addr, length, prot, flags, fd, (off_t)offset);
}
#else
JL_DLLEXPORT int64_t jl_lseek(HANDLE fd, int64_t offset, int whence)
{
    LARGE_INTEGER tell;
    tell.QuadPart = offset;
    if (SetFilePointerEx(fd, tell, &tell, whence) == 0)
        return -1;
    return tell.QuadPart;
}
#endif
JL_DLLEXPORT int jl_sizeof_ios_t(void) { return sizeof(ios_t); }

JL_DLLEXPORT long jl_ios_fd(ios_t *s) { return s->fd; }

JL_DLLEXPORT int32_t jl_nb_available(ios_t *s)
{
    return (int32_t)(s->size - s->bpos);
}

// --- dir/file stuff ---

JL_DLLEXPORT char *jl_uv_fs_t_ptr(uv_fs_t *req) { return (char*)req->ptr; }
JL_DLLEXPORT char *jl_uv_fs_t_path(uv_fs_t *req) { return (char*)req->path; }

// --- stat ---
JL_DLLEXPORT int jl_sizeof_stat(void) { return sizeof(uv_stat_t); }

JL_DLLEXPORT int32_t jl_stat(const char *path, char *statbuf) JL_NOTSAFEPOINT
{
    uv_fs_t req;
    int ret;

    // Ideally one would use the statbuf for the storage in req, but
    // it's not clear that this is possible using libuv
    ret = uv_fs_stat(unused_uv_loop_arg, &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

JL_DLLEXPORT int32_t jl_lstat(const char *path, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_lstat(unused_uv_loop_arg, &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

JL_DLLEXPORT int32_t jl_fstat(uv_os_fd_t fd, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_fstat(unused_uv_loop_arg, &req, fd, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

JL_DLLEXPORT unsigned int jl_stat_dev(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_dev;
}

JL_DLLEXPORT unsigned int jl_stat_ino(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_ino;
}

JL_DLLEXPORT unsigned int jl_stat_mode(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_mode;
}

JL_DLLEXPORT unsigned int jl_stat_nlink(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_nlink;
}

JL_DLLEXPORT unsigned int jl_stat_uid(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_uid;
}

JL_DLLEXPORT unsigned int jl_stat_gid(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_gid;
}

JL_DLLEXPORT unsigned int jl_stat_rdev(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_rdev;
}

JL_DLLEXPORT uint64_t jl_stat_size(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_size;
}

JL_DLLEXPORT uint64_t jl_stat_blksize(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_blksize;
}

JL_DLLEXPORT uint64_t jl_stat_blocks(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_blocks;
}

JL_DLLEXPORT double jl_stat_atime(char *statbuf)
{
    uv_stat_t *s;
    s = (uv_stat_t*)statbuf;
    return (double)s->st_atim.tv_sec + (double)s->st_atim.tv_nsec * 1e-9;
}

JL_DLLEXPORT double jl_stat_mtime(char *statbuf)
{
    uv_stat_t *s;
    s = (uv_stat_t*)statbuf;
    return (double)s->st_mtim.tv_sec + (double)s->st_mtim.tv_nsec * 1e-9;
}

JL_DLLEXPORT double jl_stat_ctime(char *statbuf)
{
    uv_stat_t *s;
    s = (uv_stat_t*)statbuf;
    return (double)s->st_ctim.tv_sec + (double)s->st_ctim.tv_nsec * 1e-9;
}

JL_DLLEXPORT unsigned long jl_getuid(void)
{
#ifdef _OS_WINDOWS_
    return -1;
#else
    return getuid();
#endif
}

JL_DLLEXPORT unsigned long jl_geteuid(void)
{
#ifdef _OS_WINDOWS_
    return -1;
#else
    return geteuid();
#endif
}

// --- buffer manipulation ---

JL_DLLEXPORT jl_array_t *jl_take_buffer(ios_t *s)
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
        char *b = ios_take_buffer(s, &n);
        a = jl_ptr_to_array_1d(jl_array_uint8_type, b, n-1, 1);
    }
    return a;
}

// str: if 1 return a string, otherwise return a Vector{UInt8}
// chomp:
//   0 - keep delimiter
//   1 - remove 1 byte delimiter
//   2 - remove 2 bytes \r\n if present
JL_DLLEXPORT jl_value_t *jl_readuntil(ios_t *s, uint8_t delim, uint8_t str, uint8_t chomp)
{
    jl_array_t *a;
    // manually inlined common case
    char *pd = (char*)memchr(s->buf + s->bpos, delim, (size_t)(s->size - s->bpos));
    if (pd) {
        size_t n = pd - (s->buf + s->bpos) + 1;
        size_t nchomp = 0;
        if (chomp) {
            nchomp = chomp == 2 ? ios_nchomp(s, n) : 1;
        }
        if (str) {
            jl_value_t *str = jl_pchar_to_string(s->buf + s->bpos, n - nchomp);
            s->bpos += n;
            return str;
        }
        a = jl_alloc_array_1d(jl_array_uint8_type, n - nchomp);
        memcpy(jl_array_data(a, uint8_t), s->buf + s->bpos, n - nchomp);
        s->bpos += n;
    }
    else {
        a = jl_alloc_array_1d(jl_array_uint8_type, 80);
        ios_t dest;
        ios_mem(&dest, 0);
        char *mem = jl_array_data(a, char);
        ios_setbuf(&dest, (char*)mem, 80, 0);
        size_t n = ios_copyuntil(&dest, s, delim, 1);
        if (chomp && n > 0 && dest.buf[n - 1] == delim) {
            n--;
            if (chomp == 2 && n > 0 && dest.buf[n - 1] == '\r') {
                n--;
            }
            int truncret = ios_trunc(&dest, n); // it should always be possible to truncate dest
            assert(truncret == 0);
            (void)truncret; // ensure the variable is used to avoid warnings
        }
        if (dest.buf != mem) {
            a = jl_take_buffer(&dest);
        }
        else {
            a->dimsize[0] = n;
        }
        if (str) {
            JL_GC_PUSH1(&a);
            jl_value_t *st = jl_array_to_string(a);
            JL_GC_POP();
            return st;
        }
    }
    return (jl_value_t*)a;
}

// read up to buflen bytes, including delim, into buf.  returns number of bytes read.
JL_DLLEXPORT size_t jl_readuntil_buf(ios_t *s, uint8_t delim, uint8_t *buf, size_t buflen)
{
    // manually inlined common case
    size_t avail = (size_t)(s->size - s->bpos);
    if (avail > buflen) avail = buflen;
    char *pd = (char*)memchr(s->buf + s->bpos, delim, avail);
    if (pd) {
        size_t n = pd - (s->buf + s->bpos) + 1;
        memcpy(buf, s->buf + s->bpos, n);
        s->bpos += n;
        return n;
    }
    else {
        size_t total = avail;
        memcpy(buf, s->buf + s->bpos, avail);
        s->bpos += avail;
        if (avail == buflen) return total;

        // code derived from ios_copyuntil
        while (!ios_eof(s)) {
            avail = ios_readprep(s, 160); // read LINE_CHUNK_SIZE
            if (avail == 0) break;
            if (total+avail > buflen) avail = buflen-total;
            char *pd = (char*)memchr(s->buf+s->bpos, delim, avail);
            if (pd == NULL) {
                memcpy(buf+total, s->buf+s->bpos, avail);
                s->bpos += avail;
                total += avail;
                if (buflen == total) return total;
            }
            else {
                size_t ntowrite = pd - (s->buf+s->bpos) + 1;
                memcpy(buf+total, s->buf+s->bpos, ntowrite);
                s->bpos += ntowrite;
                total += ntowrite;
                return total;
            }
        }
        s->_eof = 1;
        return total;
    }
}

JL_DLLEXPORT int jl_ios_buffer_n(ios_t *s, const size_t n)
{
    size_t space, ret;
    do {
        space = (size_t)(s->size - s->bpos);
        ret = ios_readprep(s, n);
        if (space == ret && ret < n)
            return 1;
    } while (ret < n);
    return 0;
}

JL_DLLEXPORT uint64_t jl_ios_get_nbyte_int(ios_t *s, const size_t n)
{
    assert(n <= 8);
    uint64_t x = 0;
    uint8_t *buf = (uint8_t*)&s->buf[s->bpos];
    if (n == 8) {
        // expecting loop unrolling optimization
        for (size_t i = 0; i < 8; i++)
            x |= (uint64_t)buf[i] << (i << 3);
    }
    else if (n >= 4) {
        // expecting loop unrolling optimization
        for (size_t i = 0; i < 4; i++)
            x |= (uint64_t)buf[i] << (i << 3);
        for (size_t i = 4; i < n; i++)
            x |= (uint64_t)buf[i] << (i << 3);
    }
    else {
        for (size_t i = 0; i < n; i++)
            x |= (uint64_t)buf[i] << (i << 3);
    }
    s->bpos += n;
    return x;
}

// -- syscall utilities --

JL_DLLEXPORT int jl_errno(void) JL_NOTSAFEPOINT { return errno; }
JL_DLLEXPORT void jl_set_errno(int e) JL_NOTSAFEPOINT { errno = e; }

// -- get the number of CPU threads (logical cores) --

#ifdef _OS_WINDOWS_
typedef DWORD (WINAPI *GAPC)(WORD);
#ifndef ALL_PROCESSOR_GROUPS
#define ALL_PROCESSOR_GROUPS 0xffff
#endif
#endif

// Apple's M1 processor is a big.LITTLE style processor, with 4x "performance"
// cores, and 4x "efficiency" cores.  Because Julia expects to be able to run
// things like heavy linear algebra workloads on all cores, it's best for us
// to only spawn as many threads as there are performance cores.  Once macOS
// 12 is released, we'll be able to query the multiple "perf levels" of the
// cores of a CPU (see this PR [0] to pytorch/cpuinfo for an example) but
// until it's released, we will just recognize the M1 by its CPU family
// identifier, then subtract how many efficiency cores we know it has.

JL_DLLEXPORT int jl_cpu_threads(void) JL_NOTSAFEPOINT
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

#if defined(__APPLE__) && defined(_CPU_AARCH64_)
//MacOS 12 added a way to query performance cores
    char buf[7];
    len = 7;
    sysctlbyname("kern.osrelease", buf, &len, NULL, 0);
    if (buf[0] > 1 && buf[1] > 0){
        len = 4;
        sysctlbyname("hw.perflevel0.physicalcpu", &count, &len, NULL, 0);
    }
    else {
        int32_t family = 0;
        len = 4;
        sysctlbyname("hw.cpufamily", &family, &len, NULL, 0);
        if (family >= 1 && count > 1) {
            if (family == CPUFAMILY_ARM_FIRESTORM_ICESTORM) {
                // We know the Apple M1 has 4 efficiency cores, so subtract them out.
                count -= 4;
            }
        }
    }
#endif
    return count;
#elif defined(_SC_NPROCESSORS_ONLN)
    long count = sysconf(_SC_NPROCESSORS_ONLN);
    if (count < 1)
        return 1;
    return count;
#elif defined(_OS_WINDOWS_)
    //Try to get WIN7 API method
    GAPC gapc;
    if (jl_dlsym(jl_kernel32_handle, "GetActiveProcessorCount", (void **)&gapc, 0, 0)) {
        return gapc(ALL_PROCESSOR_GROUPS);
    }
    else { //fall back on GetSystemInfo
        SYSTEM_INFO info;
        GetSystemInfo(&info);
        return info.dwNumberOfProcessors;
    }
#else
#warning "cpu core detection not defined for this platform"
    return 1;
#endif
}

JL_DLLEXPORT int jl_effective_threads(void) JL_NOTSAFEPOINT
{
    // We want the more conservative estimate of the two.
    int cpu_threads = jl_cpu_threads();
    int available_parallelism = uv_available_parallelism();
    return available_parallelism < cpu_threads ? available_parallelism : cpu_threads;
}

// -- precompile jobserver --
// A named-semaphore token pool created by the orchestrating precompile process
// and shared with worker subprocesses to bound the total number of AOT codegen
// threads across all parallel workers (see JuliaLang/julia#58591). Workers open
// it by name (via the JULIA_PRECOMPILE_JOBSERVER env var); the client side lives
// in aotcompile.cpp.
//
// All state is mutex-guarded: acquire/release run on arbitrary task threads and
// may race with destroy on exception paths. At most one jobserver is active per
// process; create returns NULL while one exists, so a concurrent session joins
// the existing pool (see jl_precompile_jobserver_active) rather than creating
// its own.

static uv_once_t jl_precompile_jobserver_once = UV_ONCE_INIT;
static uv_mutex_t jl_precompile_jobserver_lock;
static unsigned jl_precompile_jobserver_counter = 0;
static char jl_precompile_jobserver_name[64];
#ifdef _OS_WINDOWS_
static HANDLE jl_precompile_jobserver_sem = NULL;
#define JL_PRECOMPILE_JOBSERVER_ACTIVE() (jl_precompile_jobserver_sem != NULL)
#else
#include <semaphore.h>
static sem_t *jl_precompile_jobserver_sem = SEM_FAILED;
#define JL_PRECOMPILE_JOBSERVER_ACTIVE() (jl_precompile_jobserver_sem != SEM_FAILED)
#endif

static void jl_precompile_jobserver_init_lock(void) JL_NOTSAFEPOINT
{
    uv_mutex_init(&jl_precompile_jobserver_lock);
}

// Create the jobserver with `ntokens` tokens. Returns its name on success (for
// export to workers via the environment), or NULL on failure or when another
// jobserver is already active in this process. The name carries a per-process
// counter so workers left over from a previous session cannot open a newer
// session's pool.
JL_DLLEXPORT const char *jl_precompile_jobserver_create(int ntokens) JL_NOTSAFEPOINT
{
    if (ntokens < 1)
        ntokens = 1;
    uv_once(&jl_precompile_jobserver_once, jl_precompile_jobserver_init_lock);
    uv_mutex_lock(&jl_precompile_jobserver_lock);
    const char *name = NULL;
    if (!JL_PRECOMPILE_JOBSERVER_ACTIVE()) {
        unsigned counter = ++jl_precompile_jobserver_counter;
#ifdef _OS_WINDOWS_
        snprintf(jl_precompile_jobserver_name, sizeof(jl_precompile_jobserver_name),
                 "jl_pc_%lu_%u", (unsigned long)GetCurrentProcessId(), counter);
        jl_precompile_jobserver_sem = CreateSemaphoreA(NULL, ntokens, ntokens, jl_precompile_jobserver_name);
#else
        // Name must start with '/' and stay short (macOS limits names to ~31 chars).
        snprintf(jl_precompile_jobserver_name, sizeof(jl_precompile_jobserver_name),
                 "/jl_pc_%ld_%u", (long)getpid(), counter);
        sem_unlink(jl_precompile_jobserver_name); // clear any stale instance from a crashed run
        jl_precompile_jobserver_sem = sem_open(jl_precompile_jobserver_name, O_CREAT | O_EXCL, 0600, (unsigned)ntokens);
#endif
        if (JL_PRECOMPILE_JOBSERVER_ACTIVE())
            name = jl_precompile_jobserver_name;
    }
    uv_mutex_unlock(&jl_precompile_jobserver_lock);
    return name;
}

// Report whether this process owns an active precompile jobserver. Lets a
// session whose own create failed tell "another session already owns the pool"
// (join it) from "the OS refused the semaphore" (no pool to join).
JL_DLLEXPORT int jl_precompile_jobserver_active(void) JL_NOTSAFEPOINT
{
    uv_once(&jl_precompile_jobserver_once, jl_precompile_jobserver_init_lock);
    uv_mutex_lock(&jl_precompile_jobserver_lock);
    int active = JL_PRECOMPILE_JOBSERVER_ACTIVE();
    uv_mutex_unlock(&jl_precompile_jobserver_lock);
    return active;
}

// Tear down the jobserver created by jl_precompile_jobserver_create.
JL_DLLEXPORT void jl_precompile_jobserver_destroy(void) JL_NOTSAFEPOINT
{
    uv_once(&jl_precompile_jobserver_once, jl_precompile_jobserver_init_lock);
    uv_mutex_lock(&jl_precompile_jobserver_lock);
#ifdef _OS_WINDOWS_
    if (jl_precompile_jobserver_sem != NULL) {
        CloseHandle(jl_precompile_jobserver_sem);
        jl_precompile_jobserver_sem = NULL;
    }
#else
    if (jl_precompile_jobserver_sem != SEM_FAILED) {
        sem_close(jl_precompile_jobserver_sem);
        sem_unlink(jl_precompile_jobserver_name);
        jl_precompile_jobserver_sem = SEM_FAILED;
    }
#endif
    uv_mutex_unlock(&jl_precompile_jobserver_lock);
}

// Take one token so the orchestrator can hold a baseline per running worker
// against the shared pool. Non-blocking: returns 1 on success, 0 when no token
// is available, and -1 when no jobserver is active (e.g. torn down) so pollers
// stop waiting. The yielding poll loop lives on the Julia side.
JL_DLLEXPORT int jl_precompile_jobserver_acquire(void) JL_NOTSAFEPOINT
{
    uv_once(&jl_precompile_jobserver_once, jl_precompile_jobserver_init_lock);
    uv_mutex_lock(&jl_precompile_jobserver_lock);
    int ret;
    if (!JL_PRECOMPILE_JOBSERVER_ACTIVE())
        ret = -1;
#ifdef _OS_WINDOWS_
    else
        ret = WaitForSingleObject(jl_precompile_jobserver_sem, 0) == WAIT_OBJECT_0;
#else
    else
        ret = sem_trywait(jl_precompile_jobserver_sem) == 0;
#endif
    uv_mutex_unlock(&jl_precompile_jobserver_lock);
    return ret;
}

// Return one token previously taken with jl_precompile_jobserver_acquire.
JL_DLLEXPORT void jl_precompile_jobserver_release(void) JL_NOTSAFEPOINT
{
    uv_once(&jl_precompile_jobserver_once, jl_precompile_jobserver_init_lock);
    uv_mutex_lock(&jl_precompile_jobserver_lock);
    if (JL_PRECOMPILE_JOBSERVER_ACTIVE()) {
#ifdef _OS_WINDOWS_
        ReleaseSemaphore(jl_precompile_jobserver_sem, 1, NULL);
#else
        sem_post(jl_precompile_jobserver_sem);
#endif
    }
    uv_mutex_unlock(&jl_precompile_jobserver_lock);
}


// -- high resolution timers --
// Returns time in nanosec
JL_DLLEXPORT uint64_t jl_hrtime(void) JL_NOTSAFEPOINT
{
    return uv_hrtime();
}

// -- child process status --

#if defined _OS_WINDOWS_
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

JL_STREAM *JL_STDIN  = (JL_STREAM*)STDIN_FILENO;
JL_STREAM *JL_STDOUT = (JL_STREAM*)STDOUT_FILENO;
JL_STREAM *JL_STDERR = (JL_STREAM*)STDERR_FILENO;

JL_DLLEXPORT JL_STREAM *jl_stdin_stream(void)  { return JL_STDIN; }
JL_DLLEXPORT JL_STREAM *jl_stdout_stream(void) { return JL_STDOUT; }
JL_DLLEXPORT JL_STREAM *jl_stderr_stream(void) { return JL_STDERR; }

JL_DLLEXPORT int jl_termios_size(void) {
#if defined(_OS_WINDOWS_)
    return 0;
#else
    return sizeof(struct termios);
#endif
}

// -- processor native alignment information --

JL_DLLEXPORT void jl_native_alignment(uint_t *int8align, uint_t *int16align, uint_t *int32align,
                                      uint_t *int64align, uint_t *float32align, uint_t *float64align)
{
    *int8align = __alignof(uint8_t);
    *int16align = __alignof(uint16_t);
    *int32align = __alignof(uint32_t);
    *int64align = __alignof(uint64_t);
    *float32align = __alignof(float);
    *float64align = __alignof(double);
}

JL_DLLEXPORT jl_value_t *jl_is_char_signed(void)
{
    return ((char)255) < 0 ? jl_true : jl_false;
}

// -- misc sysconf info --

#ifdef _OS_WINDOWS_
static long cachedPagesize = 0;
JL_DLLEXPORT long jl_getpagesize(void)
{
    if (!cachedPagesize) {
        SYSTEM_INFO systemInfo;
        GetSystemInfo (&systemInfo);
        cachedPagesize = systemInfo.dwPageSize;
    }
    return cachedPagesize;
}
#else
JL_DLLEXPORT long jl_getpagesize(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    assert(page_size != -1);
    return page_size;
}
#endif

#ifdef _OS_WINDOWS_
static long cachedAllocationGranularity = 0;
JL_DLLEXPORT long jl_getallocationgranularity(void) JL_NOTSAFEPOINT
{
    if (!cachedAllocationGranularity) {
        SYSTEM_INFO systemInfo;
        GetSystemInfo (&systemInfo);
        cachedAllocationGranularity = systemInfo.dwAllocationGranularity;
    }
    return cachedAllocationGranularity;
}
#else
JL_DLLEXPORT long jl_getallocationgranularity(void) JL_NOTSAFEPOINT
{
    return jl_getpagesize();
}
#endif

JL_DLLEXPORT long jl_gethugepagesize(void) JL_NOTSAFEPOINT
{
#if defined(_OS_LINUX_)
    long detected = 0;
    FILE *f = fopen("/sys/kernel/mm/transparent_hugepage/hpage_pmd_size", "r");
    if (f) {
        unsigned long long size = 0;
        if (fscanf(f, "%llu", &size) == 1 && size > 0) {
            detected = (long)size;
        }
        fclose(f);
    }
    if (detected == 0) {
        f = fopen("/proc/meminfo", "r");
        if (f) {
            char line[256];
            while (fgets(line, sizeof(line), f)) {
                unsigned long long kb = 0;
                if (sscanf(line, "Hugepagesize:%llu kB", &kb) == 1 && kb > 0) {
                    detected = (long)(kb * 1024ULL);
                    break;
                }
            }
            fclose(f);
        }
    }
    if (detected == 0) {
        detected = 2 * 1024 * 1024; // 2 MiB fallback
    }
    return detected;
#else
    return 0;
#endif
}

JL_DLLEXPORT long jl_SC_CLK_TCK(void)
{
#ifndef _OS_WINDOWS_
    return sysconf(_SC_CLK_TCK);
#else
    return 1000; /* uv_cpu_info returns times in ms on Windows */
#endif
}

#ifdef _OS_OPENBSD_
// Helper for jl_pathname_for_handle()
struct dlinfo_data {
    void       *searched;
    const char *result;
};

static int dlinfo_helper(struct dl_phdr_info *info, size_t size, void *vdata)
{
    struct dlinfo_data *data = (struct dlinfo_data *)vdata;
    void *handle;

    /* ensure dl_phdr_info at compile-time to be compatible with the one at runtime */
    if (sizeof(*info) < size)
        return -1;

    /* dlopen the name */
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
    if (handle == NULL)
        return 0;

    /* check if the opened library is the same as the searched handle */
    if (data->searched == handle)
        data->result = info->dlpi_name;

    dlclose(handle);

    /* continue if still not found */
    return (data->result != NULL);
}
#endif

// Takes a handle (as returned from dlopen()) and returns the absolute path to the image loaded
JL_DLLEXPORT const char *jl_pathname_for_handle(void *handle) JL_NOTSAFEPOINT
{
    if (!handle)
        return NULL;

#ifdef __APPLE__
    // Iterate through all images currently in memory
    for (int32_t i = _dyld_image_count() - 1; i >= 0 ; i--) {
        // dlopen() each image, check handle.
        const char *image_name = _dyld_get_image_name(i);
        void *probe_lib = jl_dlopen(image_name, JL_RTLD_DEFAULT | JL_RTLD_NOLOAD);
        jl_dlclose(probe_lib);

        // If the handle is the same as what was passed in (modulo mode bits), return this image name
        if (((intptr_t)handle & (-4)) == ((intptr_t)probe_lib & (-4)))
            return image_name;
    }

#elif defined(_OS_WINDOWS_)

    wchar_t *pth16 = (wchar_t*)malloc_s(32768 * sizeof(*pth16)); // max long path length
    DWORD n16 = GetModuleFileNameW((HMODULE)handle, pth16, 32768);
    if (n16 <= 0) {
        free(pth16);
        return NULL;
    }
    char *filepath = NULL;
    size_t n8 = 0;
    if (uv_utf16_to_wtf8((uint16_t*)pth16, n16, &filepath, &n8)) {
        free(pth16);
        return NULL;
    }
    free(pth16);
    return filepath;

#elif defined(_OS_OPENBSD_)
    struct dlinfo_data data = {
        .searched = handle,
        .result = NULL,
    };
    dl_iterate_phdr(&dlinfo_helper, &data);
    return data.result;

#else // Linux, FreeBSD, ...

    struct link_map *map;
    dlinfo(handle, RTLD_DI_LINKMAP, &map);
#ifdef _COMPILER_MSAN_ENABLED_
    __msan_unpoison(&map,sizeof(struct link_map*));
    if (map) {
        __msan_unpoison(map, sizeof(struct link_map));
        __msan_unpoison_string(map->l_name);
    }
#endif
    if (map)
        return map->l_name;

#endif
    return NULL;
}

#if !defined(_OS_WINDOWS_) && !defined(__APPLE__) && !defined(__GLIBC__)
struct sym_phdr_query {
    uintptr_t   addr;
    const char *name;        // matched dlpi_name
    int         is_main_exe;
    int         found;
    int         index;       // running object index; 0 == main program
};

static int sym_phdr_helper(struct dl_phdr_info *info, size_t size, void *vdata) JL_NOTSAFEPOINT
{
    (void)size;
    struct sym_phdr_query *q = (struct sym_phdr_query *)vdata;
    int idx = q->index++;
    // Find the object whose mapped PT_LOAD segments contain `addr`.
    for (int i = 0; i < info->dlpi_phnum; i++) {
        if (info->dlpi_phdr[i].p_type != PT_LOAD)
            continue;
        uintptr_t beg = (uintptr_t)info->dlpi_addr + info->dlpi_phdr[i].p_vaddr;
        uintptr_t end = beg + info->dlpi_phdr[i].p_memsz;
        if (q->addr >= beg && q->addr < end) {
            q->name = info->dlpi_name;
            // Note that `dlpi_name` for the main executable is platform-dependent
            // and frequently `argv[0]` on non-GLIBC platforms. We do not want to
            // return that result from this API since it cannot be reliably dlopen'd.
            //
            // Return "" instead, which also cannot be dlopen'd either (on non-GLIBC
            // platforms) but which is at least a consistent value to match on for a
            // dlopen(NULL) fallback, which does give a handle to the main exe.
            q->is_main_exe = (idx == 0); // dl_iterate_phdr reports the main program first
            q->found = 1;
            return 1; // stop: found the containing object
        }
    }
    return 0; // keep searching
}
#endif

// Takes the address of a symbol and returns the path to the image that contains
// it, or NULL. On ELF, the main executable is reported as the empty string "".
JL_DLLEXPORT const char *jl_pathname_for_symbol(void *symbol) JL_NOTSAFEPOINT
{
    if (!symbol)
        return NULL;
#ifdef _OS_WINDOWS_
    HMODULE handle;
    if (!GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                            (LPCWSTR)symbol, &handle))
        return NULL;
    return jl_pathname_for_handle(handle);
#elif defined(__APPLE__)
    // dyld reports the real image path (including for the main executable).
    Dl_info info;
    if (!dladdr(symbol, &info) || !info.dli_fname)
        return NULL;
    return info.dli_fname;
#elif defined(__GLIBC__)
    // glibc: dladdr1 hands us the containing object's link_map directly.
    Dl_info info;
    struct link_map *map = NULL;
    if (!dladdr1(symbol, &info, (void **)&map, RTLD_DL_LINKMAP) || map == NULL)
        return NULL;
    msan_unpoison(&map, sizeof(struct link_map *));
    msan_unpoison(map, sizeof(struct link_map));
    msan_unpoison_string(map->l_name);
    return map->l_name; // the empty string for the main executable
#else
    // Other libc: walk the program headers and test PT_LOAD containment.
    struct sym_phdr_query q = { (uintptr_t)symbol, NULL, 0, 0, 0 };
    dl_iterate_phdr(&sym_phdr_helper, &q);
    if (!q.found)
        return NULL;
    return q.is_main_exe ? "" : q.name;
#endif
}

#ifdef _OS_WINDOWS_
// Get a list of all the modules in this process.
JL_DLLEXPORT int jl_dllist(jl_array_t *list)
{
    DWORD cb, cbNeeded;
    HMODULE *hMods = NULL;
    unsigned int i;
    cbNeeded = 1024 * sizeof(*hMods);
    do {
        cb = cbNeeded;
        hMods = (HMODULE*)realloc_s(hMods, cb);
        if (!EnumProcessModulesEx(GetCurrentProcess(), hMods, cb, &cbNeeded, LIST_MODULES_ALL)) {
          free(hMods);
          return FALSE;
        }
    } while (cb < cbNeeded);
    for (i = 0; i < cbNeeded / sizeof(HMODULE); i++) {
        const char *path = jl_pathname_for_handle(hMods[i]);
        if (path == NULL)
            continue;
        jl_array_grow_end((jl_array_t*)list, 1);
        jl_value_t *v = jl_cstr_to_string(path);
        free((char*)path);
        jl_array_ptr_set(list, jl_array_dim0(list) - 1, v);
    }
    free(hMods);
    return TRUE;
}
#elif !defined(__APPLE__)
struct dllist_data {
    arraylist_t *names; // strdup'd object names (freed by jl_dllist)
    int index;
};

// This runs under the dynamic linker lock (held by `dl_iterate_phdr` across the
// callback), so it must not allocate Julia objects or otherwise hit a GC safepoint.
// A stop-the-world here while another thread is blocked in the linker would deadlock
static int dllist_helper(struct dl_phdr_info *info, size_t size, void *vdata) JL_NOTSAFEPOINT
{
    (void)size;
    struct dllist_data *data = (struct dllist_data *)vdata;
    int idx = data->index++; // dl_iterate_phdr reports the main program first
    const char *name = info->dlpi_name;
    if (idx == 0 || name == NULL || name[0] == '\0')
        return 0; // skip the main executable and any unnamed objects
    arraylist_push(data->names, strdup(name));
    return 0;
}

// Get a list of all the modules in this process.
//
// This is done in C, rather than a Julia `dl_iterate_phdr` callback, so that
// no Julia code (which can allocate, run finalizers, or re-enter the linker
// via (lazy) `ccall`) runs under the dynamic loader lock. See `dllist_helper`
JL_DLLEXPORT int jl_dllist(jl_array_t *list)
{
    arraylist_t names;
    arraylist_new(&names, 0);
    struct dllist_data data = { &names, 0 };
    dl_iterate_phdr(&dllist_helper, &data);
    // The loader lock is now released: safe to allocate Julia objects.
    for (size_t i = 0; i < names.len; i++) {
        char *name = (char*)names.items[i];
        jl_array_grow_end(list, 1);
        jl_value_t *v = jl_cstr_to_string(name);
        jl_array_ptr_set(list, jl_array_dim0(list) - 1, v);
        free(name);
    }
    arraylist_free(&names);
    return 1;
}
#endif

JL_DLLEXPORT void jl_raise_debugger(void)
{
#if defined(_OS_WINDOWS_)
    if (IsDebuggerPresent() == 1)
        DebugBreak();
#else
    raise(SIGTRAP);
#endif // _OS_WINDOWS_
}

JL_DLLEXPORT jl_sym_t *jl_get_UNAME(void) JL_NOTSAFEPOINT
{
    return jl_symbol(JL_BUILD_UNAME);
}

JL_DLLEXPORT jl_sym_t *jl_get_ARCH(void) JL_NOTSAFEPOINT
{
    return jl_symbol(JL_BUILD_ARCH);
}

JL_DLLEXPORT size_t jl_maxrss(void)
{
    uv_rusage_t rusage;
    if (uv_getrusage(&rusage) == 0) {
        return rusage.ru_maxrss * 1024;
    }
    return 0;
}

// Simple `rand()` like function, with global seed and added thread-safety
// (but slow and insecure)
static _Atomic(uint64_t) g_rngseed;
JL_DLLEXPORT uint64_t jl_rand(void) JL_NOTSAFEPOINT
{
    uint64_t max = UINT64_MAX;
    uint64_t rngseed0 = jl_atomic_load_relaxed(&g_rngseed);
    uint64_t rngseed;
    uint64_t rnd;
    do {
        rngseed = rngseed0;
        rnd = cong(max, &rngseed);
    } while (!jl_atomic_cmpswap_relaxed(&g_rngseed, &rngseed0, rngseed));
    return rnd;
}

JL_DLLEXPORT void jl_srand(uint64_t rngseed) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&g_rngseed, rngseed);
}

void jl_init_rand(void)
{
    uint64_t rngseed;
    if (uv_random(NULL, NULL, &rngseed, sizeof(rngseed), 0, NULL)) {
        ios_puts("WARNING: Entropy pool not available to seed RNG; using ad-hoc entropy sources.\n", ios_stderr);
        rngseed = uv_hrtime();
        rngseed ^= int64hash(uv_os_getpid());
    }
    jl_srand(rngseed);
    srand(rngseed);
}

#ifdef __cplusplus
}
#endif
