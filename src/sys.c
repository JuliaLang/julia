// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  sys.c
  I/O and operating system utility functions
*/
#include "julia.h"
#include "julia_internal.h"
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <psapi.h>
#else
#include <sys/sysctl.h>
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <unistd.h>
#include <sys/mman.h>
#include <dlfcn.h>
#endif
#include <errno.h>
#include <signal.h>
#include <fcntl.h>

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

#ifdef __has_feature
#if __has_feature(memory_sanitizer)
#include <sanitizer/msan_interface.h>
#endif
#endif

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

JL_DLLEXPORT int jl_sizeof_uv_mutex(void) { return sizeof(uv_mutex_t); }
JL_DLLEXPORT int jl_sizeof_off_t(void) { return sizeof(off_t); }
#ifndef _OS_WINDOWS_
JL_DLLEXPORT off_t jl_lseek(int fd, off_t offset, int whence) { return lseek(fd, offset, whence); }
JL_DLLEXPORT ssize_t jl_pwrite(int fd, const void *buf, size_t count, off_t offset)
{
    return pwrite(fd, buf, count, offset);
}
JL_DLLEXPORT void *jl_mmap(void *addr, size_t length, int prot, int flags,
                        int fd, off_t offset)
{
    return mmap(addr, length, prot, flags, fd, offset);
}
#else
JL_DLLEXPORT off_t jl_lseek(int fd, off_t offset, int whence) { return _lseek(fd, offset, whence); }
#endif
JL_DLLEXPORT int jl_sizeof_ios_t(void) { return sizeof(ios_t); }

JL_DLLEXPORT long jl_ios_fd(ios_t *s) { return s->fd; }

JL_DLLEXPORT int32_t jl_nb_available(ios_t *s)
{
    return (int32_t)(s->size - s->bpos);
}

// --- dir/file stuff ---

JL_DLLEXPORT int jl_sizeof_uv_fs_t(void) { return sizeof(uv_fs_t); }
JL_DLLEXPORT void jl_uv_fs_req_cleanup(uv_fs_t *req)
{
    uv_fs_req_cleanup(req);
}

JL_DLLEXPORT int jl_readdir(const char *path, uv_fs_t *readdir_req)
{
    // Note that the flags field is mostly ignored by libuv
    return uv_fs_readdir(uv_default_loop(), readdir_req, path, 0 /*flags*/, NULL);
}

JL_DLLEXPORT char *jl_uv_fs_t_ptr(uv_fs_t *req) { return (char*)req->ptr; }
JL_DLLEXPORT char *jl_uv_fs_t_ptr_offset(uv_fs_t *req, int offset) { return (char*)req->ptr + offset; }
JL_DLLEXPORT int jl_uv_fs_result(uv_fs_t *f) { return f->result; }

// --- stat ---
JL_DLLEXPORT int jl_sizeof_stat(void) { return sizeof(uv_stat_t); }

JL_DLLEXPORT int32_t jl_stat(const char *path, char *statbuf)
{
    uv_fs_t req;
    int ret;

    // Ideally one would use the statbuf for the storage in req, but
    // it's not clear that this is possible using libuv
    ret = uv_fs_stat(uv_default_loop(), &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

JL_DLLEXPORT int32_t jl_lstat(const char *path, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_lstat(uv_default_loop(), &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

JL_DLLEXPORT int32_t jl_fstat(int fd, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_fstat(uv_default_loop(), &req, fd, NULL);
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

/*
// atime is stupid, let's not support it
JL_DLLEXPORT double jl_stat_atime(char *statbuf)
{
  uv_stat_t *s;
  s = (uv_stat_t*)statbuf;
  return (double)s->st_atim.tv_sec + (double)s->st_atim.tv_nsec * 1e-9;
}
*/

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

// --- buffer manipulation ---

JL_DLLEXPORT jl_array_t *jl_takebuf_array(ios_t *s)
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

JL_DLLEXPORT jl_value_t *jl_takebuf_string(ios_t *s)
{
    jl_array_t *a = jl_takebuf_array(s);
    JL_GC_PUSH1(&a);
    jl_value_t *str = jl_array_to_string(a);
    JL_GC_POP();
    return str;
}

// the returned buffer must be manually freed. To determine the size,
// call position(s) before using this function.
JL_DLLEXPORT void *jl_takebuf_raw(ios_t *s)
{
    size_t sz;
    void *buf = ios_takebuf(s, &sz);
    return buf;
}

JL_DLLEXPORT jl_value_t *jl_readuntil(ios_t *s, uint8_t delim)
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
        ios_setbuf(&dest, (char*)a->data, 80, 0);
        size_t n = ios_copyuntil(&dest, s, delim);
        if (dest.buf != a->data) {
            a = jl_takebuf_array(&dest);
        }
        else {
#ifdef STORE_ARRAY_LEN
            a->length = n;
#endif
            a->nrows = n;
            ((char*)a->data)[n] = '\0';
        }
    }
    return (jl_value_t*)a;
}

static void JL_NORETURN throw_eof_error(void)
{
    jl_datatype_t *eof_error = (jl_datatype_t*)jl_get_global(jl_base_module, jl_symbol("EOFError"));
    assert(eof_error != NULL);
    jl_exceptionf(eof_error, "");
}

JL_DLLEXPORT uint64_t jl_ios_get_nbyte_int(ios_t *s, const size_t n)
{
    assert(n <= 8);
    size_t space, ret;
    do {
        space = s->size - s->bpos;
        ret = ios_readprep(s, n);
        if (space == ret && ret < n)
            throw_eof_error();
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

// -- get the number of CPU cores --

#ifdef _OS_WINDOWS_
typedef DWORD (WINAPI *GAPC)(WORD);
#ifndef ALL_PROCESSOR_GROUPS
#define ALL_PROCESSOR_GROUPS 0xffff
#endif
#endif

JL_DLLEXPORT int jl_cpu_cores(void)
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
#elif defined(_OS_WINDOWS_)
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
JL_DLLEXPORT uint64_t jl_hrtime(void)
{
    return uv_hrtime();
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

// CPUID

#ifdef HAVE_CPUID
JL_DLLEXPORT void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType)
{
#if defined _MSC_VER
    __cpuid(CPUInfo, InfoType);
#else
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
#endif
}
#endif

// -- set/clear the FZ/DAZ flags on x86 & x86-64 --
#ifdef __SSE__

// Cache of information recovered from jl_cpuid.
// In a multithreaded environment, there will be races on subnormal_flags,
// but they are harmless idempotent races.  If we ever embrace C11, then
// subnormal_flags should be declared atomic.
static volatile int32_t subnormal_flags = 1;

static int32_t get_subnormal_flags(void)
{
    uint32_t f = subnormal_flags;
    if (f & 1) {
        // CPU capabilities not yet inspected.
        f = 0;
        int32_t info[4];
        jl_cpuid(info, 0);
        if (info[0] >= 1) {
            jl_cpuid(info, 0x00000001);
            if (info[3] & (1 << 26)) {
                // SSE2 supports both FZ and DAZ
                f = 0x00008040;
            }
            else if (info[3] & (1 << 25)) {
                // SSE supports only the FZ flag
                f = 0x00008000;
            }
        }
        subnormal_flags = f;
    }
    return f;
}

// Returns non-zero if subnormals go to 0; zero otherwise.
JL_DLLEXPORT int32_t jl_get_zero_subnormals(int8_t isZero)
{
    uint32_t flags = get_subnormal_flags();
    return _mm_getcsr() & flags;
}

// Return zero on success, non-zero on failure.
JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint32_t flags = get_subnormal_flags();
    if (flags) {
        uint32_t state = _mm_getcsr();
        if (isZero)
            state |= flags;
        else
            state &= ~flags;
        _mm_setcsr(state);
        return 0;
    }
    else {
        // Report a failure only if user is trying to enable FTZ/DAZ.
        return isZero;
    }
}

#else

JL_DLLEXPORT int32_t jl_get_zero_subnormals(int8_t isZero)
{
    return 0;
}

JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    return isZero;
}

#endif

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

JL_DLLEXPORT void jl_field_offsets(jl_datatype_t *dt, ssize_t *offsets)
{
    size_t i;
    for(i=0; i < jl_datatype_nfields(dt); i++) {
        offsets[i] = jl_field_offset(dt, i);
    }
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
    return sysconf(_SC_PAGESIZE);
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

JL_DLLEXPORT size_t jl_get_field_offset(jl_datatype_t *ty, int field)
{
    if (field > jl_datatype_nfields(ty))
        jl_error("This type does not have that many fields");
    return jl_field_offset(ty, field);
}

JL_DLLEXPORT size_t jl_get_alignment(jl_datatype_t *ty)
{
    return ty->alignment;
}

// Takes a handle (as returned from dlopen()) and returns the absolute path to the image loaded
JL_DLLEXPORT const char *jl_pathname_for_handle(void *handle)
{
    if (!handle)
        return NULL;

#ifdef __APPLE__
    // Iterate through all images currently in memory
    for (int32_t i = _dyld_image_count(); i >= 0 ; i--) {
        // dlopen() each image, check handle
        const char *image_name = _dyld_get_image_name(i);
        void *probe_lib = jl_load_dynamic_library(image_name, JL_RTLD_DEFAULT);
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
#ifdef __has_feature
#if __has_feature(memory_sanitizer)
    __msan_unpoison(&map,sizeof(struct link_map*));
    if (map) {
      __msan_unpoison(map, sizeof(struct link_map));
      __msan_unpoison_string(map->l_name);
    }
#endif
#endif
    if (map)
        return map->l_name;

#endif
    return NULL;
}

#ifdef _OS_WINDOWS_
#include <dbghelp.h>
static BOOL CALLBACK jl_EnumerateLoadedModulesProc64(
  _In_      PCTSTR ModuleName,
  _In_      DWORD64 ModuleBase,
  _In_      ULONG ModuleSize,
  _In_opt_  PVOID a
)
{
    jl_array_grow_end((jl_array_t*)a, 1);
    //XXX: change to jl_arrayset if array storage allocation for Array{ByteString,1} changes:
    jl_value_t *v = jl_cstr_to_string(ModuleName);
    jl_cellset(a, jl_array_dim0(a)-1, v);
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

JL_DLLEXPORT jl_sym_t* jl_get_OS_NAME(void)
{
#if defined(_OS_WINDOWS_)
    return jl_symbol("Windows");
#elif defined(_OS_LINUX_)
    return jl_symbol("Linux");
#elif defined(_OS_FREEBSD_)
    return jl_symbol("FreeBSD");
#elif defined(_OS_DARWIN_)
    return jl_symbol("Darwin");
#else
#warning OS_NAME is Unknown
    return jl_symbol("Unknown");
#endif
}

JL_DLLEXPORT jl_sym_t* jl_get_ARCH(void)
{
    static jl_sym_t* ARCH = NULL;
    if (!ARCH)
        ARCH = (jl_sym_t*) jl_get_global(jl_base_module, jl_symbol("ARCH"));
    return ARCH;
}

JL_DLLEXPORT size_t jl_maxrss(void)
{
#if defined(_OS_WINDOWS_)
	PROCESS_MEMORY_COUNTERS counter;
	GetProcessMemoryInfo( GetCurrentProcess( ), &counter, sizeof(counter) );
	return (size_t)counter.PeakWorkingSetSize;

#elif defined(_OS_LINUX_) || defined(_OS_DARWIN_) || defined (_OS_FREEBSD_)
	struct rusage rusage;
	getrusage( RUSAGE_SELF, &rusage );

#if defined(_OS_LINUX_)
	return (size_t)(rusage.ru_maxrss * 1024);
#else
	return (size_t)rusage.ru_maxrss;
#endif

#else
	return (size_t)0;
#endif
}



#ifdef __cplusplus
}
#endif
