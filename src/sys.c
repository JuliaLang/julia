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

#if defined _MSC_VER
#include <io.h>
#include <intrin.h>
#endif

#ifdef JL_MSAN_ENABLED
#include <sanitizer/msan_interface.h>
#endif

#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
JL_DLLEXPORT char *dirname(char *);
#else
#include <libgen.h>
#endif

JL_DLLEXPORT uint32_t jl_getutf8(ios_t *s)
{
    uint32_t wc=0;
    ios_getutf8(s, &wc);
    return wc;
}

#ifndef JL_DISABLE_LIBUV
JL_DLLEXPORT int jl_sizeof_uv_mutex(void) { return sizeof(uv_mutex_t); }
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
        memcpy(jl_array_data(a), s->buf + s->bpos, n - nchomp);
        s->bpos += n;
    }
    else {
        a = jl_alloc_array_1d(jl_array_uint8_type, 80);
        ios_t dest;
        ios_mem(&dest, 0);
        ios_setbuf(&dest, (char*)a->data, 80, 0);
        size_t n = ios_copyuntil(&dest, s, delim);
        if (chomp && n > 0 && dest.buf[n - 1] == delim) {
            n--;
            if (chomp == 2 && n > 0 && dest.buf[n - 1] == '\r') {
                n--;
            }
            int truncret = ios_trunc(&dest, n); // it should always be possible to truncate dest
            assert(truncret == 0);
            (void)truncret; // ensure the variable is used to avoid warnings
        }
        if (dest.buf != a->data) {
            a = jl_take_buffer(&dest);
        }
        else {
#ifdef STORE_ARRAY_LEN
            a->length = n;
#endif
            a->nrows = n;
            ((char*)a->data)[n] = '\0';
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

JL_DLLEXPORT uint64_t jl_ios_get_nbyte_int(ios_t *s, const size_t n)
{
    assert(n <= 8);
    size_t space, ret;
    do {
        space = (size_t)(s->size - s->bpos);
        ret = ios_readprep(s, n);
        if (space == ret && ret < n)
            jl_eof_error();
    } while(ret < n);
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

JL_DLLEXPORT int jl_errno(void) { return errno; }
JL_DLLEXPORT void jl_set_errno(int e) { errno = e; }

// -- get the number of CPU threads (logical cores) --

#ifdef _OS_WINDOWS_
typedef DWORD (WINAPI *GAPC)(WORD);
#ifndef ALL_PROCESSOR_GROUPS
#define ALL_PROCESSOR_GROUPS 0xffff
#endif
#endif

JL_DLLEXPORT int jl_cpu_threads(void)
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
    long count = sysconf(_SC_NPROCESSORS_ONLN);
    if (count < 1)
        return 1;
    return count;
#elif defined(_OS_WINDOWS_)
    //Try to get WIN7 API method
    GAPC gapc;
    if (jl_dlsym(jl_kernel32_handle, "GetActiveProcessorCount", (void **)&gapc, 0)) {
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


// -- high resolution timers --
// Returns time in nanosec
JL_DLLEXPORT uint64_t jl_hrtime(void)
{
#ifdef JL_DISABLE_LIBUV
    const uint64_t NANOSEC = 1000000000;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (((uint64_t) ts.tv_sec) * NANOSEC + ts.tv_nsec);
#else
    return uv_hrtime();
#endif
}

// -- iterating the environment --

#ifdef __APPLE__
#include <crt_externs.h>
#else
#if !defined(_OS_WINDOWS_) || defined(_COMPILER_MINGW_)
extern char **environ;
#endif
#endif

JL_DLLEXPORT jl_value_t *jl_environ(int i)
{
#ifdef __APPLE__
    char **environ = *_NSGetEnviron();
#endif
    char *env = environ[i];
    return env ? jl_pchar_to_string(env, strlen(env)) : jl_nothing;
}

// -- child process status --

#if defined _MSC_VER || defined _OS_WINDOWS_
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
JL_DLLEXPORT long jl_getallocationgranularity(void)
{
    if (!cachedAllocationGranularity) {
        SYSTEM_INFO systemInfo;
        GetSystemInfo (&systemInfo);
        cachedAllocationGranularity = systemInfo.dwAllocationGranularity;
    }
    return cachedAllocationGranularity;
}
#else
JL_DLLEXPORT long jl_getallocationgranularity(void)
{
    return jl_getpagesize();
}
#endif

JL_DLLEXPORT long jl_SC_CLK_TCK(void)
{
#ifndef _OS_WINDOWS_
    return sysconf(_SC_CLK_TCK);
#else
    return 0;
#endif
}

// Takes a handle (as returned from dlopen()) and returns the absolute path to the image loaded
JL_DLLEXPORT const char *jl_pathname_for_handle(void *handle)
{
    if (!handle)
        return NULL;

#ifdef __APPLE__
    // Iterate through all images currently in memory
    for (int32_t i = _dyld_image_count() - 1; i >= 0 ; i--) {
        // dlopen() each image, check handle
        const char *image_name = _dyld_get_image_name(i);
        void *probe_lib = jl_load_dynamic_library(image_name, JL_RTLD_DEFAULT, 0);
        jl_dlclose(probe_lib);

        // If the handle is the same as what was passed in (modulo mode bits), return this image name
        if (((intptr_t)handle & (-4)) == ((intptr_t)probe_lib & (-4)))
            return image_name;
    }

#elif defined(_OS_WINDOWS_)

    wchar_t *pth16 = (wchar_t*)malloc(32768); // max long path length
    DWORD n16 = GetModuleFileNameW((HMODULE)handle,pth16,32768);
    if (n16 <= 0) {
        free(pth16);
        return NULL;
    }
    pth16[n16] = L'\0';
    DWORD n8 = WideCharToMultiByte(CP_UTF8, 0, pth16, -1, NULL, 0, NULL, NULL);
    if (n8 == 0) {
        free(pth16);
        return NULL;
    }
    char *filepath = (char*)malloc(++n8);
    if (!WideCharToMultiByte(CP_UTF8, 0, pth16, -1, filepath, n8, NULL, NULL)) {
        free(pth16);
        free(filepath);
        return NULL;
    }
    free(pth16);
    return filepath;

#else // Linux, FreeBSD, ...

    struct link_map *map;
    dlinfo(handle, RTLD_DI_LINKMAP, &map);
#ifdef JL_MSAN_ENABLED
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

#ifdef _OS_WINDOWS_
static BOOL CALLBACK jl_EnumerateLoadedModulesProc64(
  _In_      PCTSTR ModuleName,
  _In_      DWORD64 ModuleBase,
  _In_      ULONG ModuleSize,
  _In_opt_  PVOID a
)
{
    jl_array_grow_end((jl_array_t*)a, 1);
    //XXX: change to jl_arrayset if array storage allocation for Array{String,1} changes:
    jl_value_t *v = jl_cstr_to_string(ModuleName);
    jl_array_ptr_set(a, jl_array_dim0(a)-1, v);
    return TRUE;
}
// Takes a handle (as returned from dlopen()) and returns the absolute path to the image loaded
JL_DLLEXPORT int jl_dllist(jl_array_t *list)
{
    return EnumerateLoadedModules64(GetCurrentProcess(), jl_EnumerateLoadedModulesProc64, list);
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

JL_DLLEXPORT jl_sym_t *jl_get_UNAME(void)
{
    return jl_symbol(JL_BUILD_UNAME);
}

JL_DLLEXPORT jl_sym_t *jl_get_ARCH(void)
{
    return jl_symbol(JL_BUILD_ARCH);
}

JL_DLLEXPORT size_t jl_maxrss(void)
{
#if defined(_OS_WINDOWS_)
    PROCESS_MEMORY_COUNTERS counter;
    GetProcessMemoryInfo( GetCurrentProcess( ), &counter, sizeof(counter) );
    return (size_t)counter.PeakWorkingSetSize;

// FIXME: `rusage` is available on OpenBSD, DragonFlyBSD and NetBSD as well.
//        All of them return `ru_maxrss` in kilobytes.
#elif defined(_OS_LINUX_) || defined(_OS_DARWIN_) || defined (_OS_FREEBSD_)
    struct rusage rusage;
    getrusage( RUSAGE_SELF, &rusage );

#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
    return (size_t)(rusage.ru_maxrss * 1024);
#else
    return (size_t)rusage.ru_maxrss;
#endif

#else
    return (size_t)0;
#endif
}

JL_DLLEXPORT int jl_threading_enabled(void)
{
#ifdef JULIA_ENABLE_THREADING
    return 1;
#else
    return 0;
#endif
}

#ifdef __cplusplus
}
#endif
