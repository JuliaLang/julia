/*
  sys.c
  I/O and operating system utility functions
*/
#include "julia.h"
#include "uv.h"
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifndef __WIN32__
#include <sys/sysctl.h>
#include <sys/wait.h>
#endif
#include <errno.h>
#include <signal.h>
#include <libgen.h>
#include <fcntl.h>
#include <unistd.h>

#define __STDC_CONSTANT_MACROS
#define __STDC_LIMIT_MACROS
#include <llvm-c/Target.h>

#ifdef __SSE__
#include <xmmintrin.h>
#endif

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

DLLEXPORT int jl_sizeof_ios_t(void) { return sizeof(ios_t); }

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

// --- dir/file stuff ---

DLLEXPORT int jl_sizeof_uv_fs_t(void) { return sizeof(uv_fs_t); }
DLLEXPORT void jl_uv_fs_req_cleanup(uv_fs_t* req)
{
    uv_fs_req_cleanup(req);
}

DLLEXPORT int jl_readdir(const char* path, uv_fs_t* readdir_req)
{
    // Note that the flags field is mostly ignored by libuv
    return uv_fs_readdir(uv_default_loop(), readdir_req, path, 0 /*flags*/, NULL);
}

DLLEXPORT char* jl_uv_fs_t_ptr(uv_fs_t* req) {return req->ptr; }
DLLEXPORT char* jl_uv_fs_t_ptr_offset(uv_fs_t* req, int offset) {return req->ptr + offset; }
DLLEXPORT int jl_uv_fs_result(uv_fs_t *f) { return f->result; }

// --- stat ---
DLLEXPORT int jl_sizeof_stat(void) { return sizeof(uv_statbuf_t); }

DLLEXPORT int32_t jl_stat(const char* path, char* statbuf)
{
    uv_fs_t req;
    int ret;

    // Ideally one would use the statbuf for the storage in req, but
    // it's not clear that this is possible using libuv
    ret = uv_fs_stat(uv_default_loop(), &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_statbuf_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int32_t jl_lstat(const char* path, char* statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_lstat(uv_default_loop(), &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_statbuf_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int32_t jl_fstat(int fd, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_fstat(uv_default_loop(), &req, fd, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_statbuf_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT unsigned int jl_stat_dev(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_dev;
}

DLLEXPORT unsigned int jl_stat_ino(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_ino;
}

DLLEXPORT unsigned int jl_stat_mode(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_mode;
}

DLLEXPORT unsigned int jl_stat_nlink(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_nlink;
}

DLLEXPORT unsigned int jl_stat_uid(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_uid;
}

DLLEXPORT unsigned int jl_stat_gid(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_gid;
}

DLLEXPORT unsigned int jl_stat_rdev(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_rdev;
}

DLLEXPORT off_t jl_stat_size(char *statbuf)
{
    return ((uv_statbuf_t*) statbuf)->st_size;
}

DLLEXPORT unsigned int jl_stat_blksize(char *statbuf)
{
#if defined(__WIN32__)
    return 0;
#else
    return ((uv_statbuf_t*) statbuf)->st_blksize;
#endif
}

DLLEXPORT unsigned int jl_stat_blocks(char *statbuf)
{
#if defined(__WIN32__)
    return 0;
#else
    return ((uv_statbuf_t*) statbuf)->st_blocks;
#endif
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
  uv_statbuf_t *s;
  s = (uv_statbuf_t*) statbuf;
#if defined(__WIN32__)
  return (double)s->st_atime;
#else
  return (double)s->st_ATIM.tv_sec + (double)s->st_ATIM.tv_nsec * 1e-9;
#endif
}
*/

DLLEXPORT double jl_stat_mtime(char *statbuf)
{
  uv_statbuf_t *s;
  s = (uv_statbuf_t*) statbuf;
#if defined(__WIN32__)
  return (double)s->st_mtime;
#else
  return (double)s->st_MTIM.tv_sec + (double)s->st_MTIM.tv_nsec * 1e-9;
#endif
}

DLLEXPORT double jl_stat_ctime(char *statbuf)
{
  uv_statbuf_t *s;
  s = (uv_statbuf_t*) statbuf;
#if defined(__WIN32__)
  return (double)s->st_ctime;
#else
  return (double)s->st_CTIM.tv_sec + (double)s->st_CTIM.tv_nsec * 1e-9;
#endif
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
    return (jl_value_t*)a;
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
    GAPC gapc = (GAPC) jl_dlsym_e(
        jl_kernel32_handle,
        "GetActiveProcessorCount"
    );

    if (gapc) {
        return gapc(ALL_PROCESSOR_GROUPS);
    }
    else { //fall back on GetSystemInfo
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
#ifdef __WIN32__
jl_value_t *jl_env_done(char *pos)
{
    return (*pos==0)?jl_true:jl_false;
}
#endif
// -- child process status --

#if defined _MSC_VER || defined __WIN32__

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

JL_STREAM *JL_STDIN=0;
JL_STREAM *JL_STDOUT=0;
JL_STREAM *JL_STDERR=0;

JL_STREAM *jl_stdin_stream(void)  { return (JL_STREAM*) JL_STDIN; }
JL_STREAM *jl_stdout_stream(void) { return (JL_STREAM*) JL_STDOUT; }
JL_STREAM *jl_stderr_stream(void) { return (JL_STREAM*) JL_STDERR; }

// -- set/clear the FZ/DAZ flags on x86 & x86-64 --

#ifdef __SSE__

#ifdef _WIN32
#define cpuid    __cpuid
#else

void cpuid(int32_t CPUInfo[4], int32_t InfoType)
{
    __asm__ __volatile__ (
        #if defined(__i386__) && defined(__PIC__)
        "xchg %%ebx, %%esi;"
        "cpuid;"
        "xchg %%esi, %%ebx;":
        "=S" (CPUInfo[1]) ,
        #else
        "cpuid":
        "=b" (CPUInfo[1]),
        #endif
        "=a" (CPUInfo[0]),
        "=c" (CPUInfo[2]),
        "=d" (CPUInfo[3]) :
        "a" (InfoType)
    );
}

#endif

DLLEXPORT uint8_t jl_zero_denormals(uint8_t isZero)
{
    uint32_t flags = 0x00000000;
    int32_t info[4];

    cpuid(info, 0);
    if (info[0] >= 1) {
        cpuid(info, 0x00000001);
        if ((info[3] & ((int)1 << 26)) != 0) {
            // SSE2 supports both FZ and DAZ
            flags = 0x00008040;
        }
        else if ((info[3] & ((int)1 << 25)) != 0) {
            // SSE supports only the FZ flag
            flags = 0x00008000;
        }
    }

    if (flags) {
        if (isZero) {
            _mm_setcsr(_mm_getcsr() | flags);
        }
        else {
            _mm_setcsr(_mm_getcsr() & ~flags);
        }
        return 1;
    }
    return 0;
}

#else

DLLEXPORT uint8_t jl_zero_denormals(uint8_t isZero)
{
    return 0;
}

#endif

// -- processor native alignment information --

DLLEXPORT void jl_native_alignment(uint_t* int8align, uint_t* int16align, uint_t* int32align, uint_t* int64align, uint_t* float32align, uint_t* float64align)
{
    LLVMTargetDataRef tgtdata = LLVMCreateTargetData("");
    *int8align = LLVMPreferredAlignmentOfType(tgtdata, LLVMInt8Type());
    *int16align = LLVMPreferredAlignmentOfType(tgtdata, LLVMInt16Type());
    *int32align = LLVMPreferredAlignmentOfType(tgtdata, LLVMInt32Type());
    *int64align = LLVMPreferredAlignmentOfType(tgtdata, LLVMInt64Type());
    *float32align = LLVMPreferredAlignmentOfType(tgtdata, LLVMFloatType());
    *float64align = LLVMPreferredAlignmentOfType(tgtdata, LLVMDoubleType());
    LLVMDisposeTargetData(tgtdata);
}

DLLEXPORT jl_value_t *jl_is_char_signed()
{
    return ((char)255) < 0 ? jl_true : jl_false;
}
