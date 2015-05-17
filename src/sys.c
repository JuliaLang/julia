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
#ifndef _OS_WINDOWS_
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

#ifdef __APPLE__
#include <mach-o/dyld.h>
#include <mach-o/nlist.h>
#include <sys/types.h> // for jl_raise_debugger
#elif !defined(_OS_WINDOWS_)
#include <link.h>
#endif

#define __STDC_CONSTANT_MACROS
#define __STDC_LIMIT_MACROS
#include <llvm-c/Target.h>

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
DLLEXPORT char *dirname(char *);
#else
#include <libgen.h>
#endif

DLLEXPORT uint32_t jl_getutf8(ios_t *s)
{
    uint32_t wc=0;
    ios_getutf8(s, &wc);
    return wc;
}

DLLEXPORT int jl_sizeof_off_t(void) { return sizeof(off_t); }
#ifndef _OS_WINDOWS_
DLLEXPORT off_t jl_lseek(int fd, off_t offset, int whence) { return lseek(fd, offset, whence); }
DLLEXPORT ssize_t jl_pwrite(int fd, const void *buf, size_t count, off_t offset)
{
    return pwrite(fd, buf, count, offset);
}
DLLEXPORT void *jl_mmap(void *addr, size_t length, int prot, int flags,
                        int fd, off_t offset)
{
    return mmap(addr, length, prot, flags, fd, offset);
}
#else
DLLEXPORT off_t jl_lseek(int fd, off_t offset, int whence) { return _lseek(fd, offset, whence); }
#endif
DLLEXPORT int jl_sizeof_ios_t(void) { return sizeof(ios_t); }

DLLEXPORT long jl_ios_fd(ios_t *s) { return s->fd; }

DLLEXPORT int32_t jl_nb_available(ios_t *s)
{
    return (int32_t)(s->size - s->bpos);
}

// --- dir/file stuff ---

DLLEXPORT int jl_sizeof_uv_fs_t(void) { return sizeof(uv_fs_t); }
DLLEXPORT void jl_uv_fs_req_cleanup(uv_fs_t *req)
{
    uv_fs_req_cleanup(req);
}

DLLEXPORT int jl_readdir(const char *path, uv_fs_t *readdir_req)
{
    // Note that the flags field is mostly ignored by libuv
    return uv_fs_readdir(uv_default_loop(), readdir_req, path, 0 /*flags*/, NULL);
}

DLLEXPORT char *jl_uv_fs_t_ptr(uv_fs_t *req) { return (char*)req->ptr; }
DLLEXPORT char *jl_uv_fs_t_ptr_offset(uv_fs_t *req, int offset) { return (char*)req->ptr + offset; }
DLLEXPORT int jl_uv_fs_result(uv_fs_t *f) { return f->result; }

// --- stat ---
DLLEXPORT int jl_sizeof_stat(void) { return sizeof(uv_stat_t); }

DLLEXPORT int32_t jl_stat(const char *path, char *statbuf)
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

DLLEXPORT int32_t jl_lstat(const char *path, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_lstat(uv_default_loop(), &req, path, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT int32_t jl_fstat(int fd, char *statbuf)
{
    uv_fs_t req;
    int ret;

    ret = uv_fs_fstat(uv_default_loop(), &req, fd, NULL);
    if (ret == 0)
        memcpy(statbuf, req.ptr, sizeof(uv_stat_t));
    uv_fs_req_cleanup(&req);
    return ret;
}

DLLEXPORT unsigned int jl_stat_dev(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_dev;
}

DLLEXPORT unsigned int jl_stat_ino(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_ino;
}

DLLEXPORT unsigned int jl_stat_mode(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_mode;
}

DLLEXPORT unsigned int jl_stat_nlink(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_nlink;
}

DLLEXPORT unsigned int jl_stat_uid(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_uid;
}

DLLEXPORT unsigned int jl_stat_gid(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_gid;
}

DLLEXPORT unsigned int jl_stat_rdev(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_rdev;
}

DLLEXPORT uint64_t jl_stat_size(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_size;
}

DLLEXPORT uint64_t jl_stat_blksize(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_blksize;
}

DLLEXPORT uint64_t jl_stat_blocks(char *statbuf)
{
    return ((uv_stat_t*)statbuf)->st_blocks;
}

/*
// atime is stupid, let's not support it
DLLEXPORT double jl_stat_atime(char *statbuf)
{
  uv_stat_t *s;
  s = (uv_stat_t*)statbuf;
  return (double)s->st_atim.tv_sec + (double)s->st_atim.tv_nsec * 1e-9;
}
*/

DLLEXPORT double jl_stat_mtime(char *statbuf)
{
    uv_stat_t *s;
    s = (uv_stat_t*)statbuf;
    return (double)s->st_mtim.tv_sec + (double)s->st_mtim.tv_nsec * 1e-9;
}

DLLEXPORT double jl_stat_ctime(char *statbuf)
{
    uv_stat_t *s;
    s = (uv_stat_t*)statbuf;
    return (double)s->st_ctim.tv_sec + (double)s->st_ctim.tv_nsec * 1e-9;
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
    JL_GC_PUSH1(&a);
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

void jl_free2(void *p, void *hint)
{
    free(p);
}

// -- syscall utilities --

int jl_errno(void) { return errno; }
void jl_set_errno(int e) { errno = e; }

// -- get the number of CPU cores --

#ifdef _OS_WINDOWS_
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
DLLEXPORT uint64_t jl_hrtime(void)
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

jl_value_t *jl_environ(int i)
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

JL_STREAM *jl_stdin_stream(void)  { return JL_STDIN; }
JL_STREAM *jl_stdout_stream(void) { return JL_STDOUT; }
JL_STREAM *jl_stderr_stream(void) { return JL_STDERR; }

// CPUID

#ifdef HAVE_CPUID
DLLEXPORT void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType)
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

DLLEXPORT uint8_t jl_zero_subnormals(uint8_t isZero)
{
    uint32_t flags = 0x00000000;
    int32_t info[4];

    jl_cpuid(info, 0);
    if (info[0] >= 1) {
        jl_cpuid(info, 0x00000001);
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

DLLEXPORT uint8_t jl_zero_subnormals(uint8_t isZero)
{
    return 0;
}

#endif

// -- processor native alignment information --

DLLEXPORT void jl_native_alignment(uint_t *int8align, uint_t *int16align, uint_t *int32align,
                                   uint_t *int64align, uint_t *float32align, uint_t *float64align)
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

DLLEXPORT void jl_field_offsets(jl_datatype_t *dt, ssize_t *offsets)
{
    size_t i;
    for(i=0; i < jl_datatype_nfields(dt); i++) {
        offsets[i] = jl_field_offset(dt, i);
    }
}

// -- misc sysconf info --

#ifdef _OS_WINDOWS_
static long cachedPagesize = 0;
long jl_getpagesize(void)
{
    if (!cachedPagesize) {
        SYSTEM_INFO systemInfo;
        GetSystemInfo (&systemInfo);
        cachedPagesize = systemInfo.dwPageSize;
    }
    return cachedPagesize;
}
#else
long jl_getpagesize(void)
{
    return sysconf(_SC_PAGESIZE);
}
#endif

#ifdef _OS_WINDOWS_
static long cachedAllocationGranularity = 0;
long jl_getallocationgranularity(void)
{
    if (!cachedAllocationGranularity) {
        SYSTEM_INFO systemInfo;
        GetSystemInfo (&systemInfo);
        cachedAllocationGranularity = systemInfo.dwAllocationGranularity;
    }
    return cachedAllocationGranularity;
}
#else
long jl_getallocationgranularity(void)
{
    return jl_getpagesize();
}
#endif

DLLEXPORT long jl_SC_CLK_TCK(void)
{
#ifndef _OS_WINDOWS_
    return sysconf(_SC_CLK_TCK);
#else
    return 0;
#endif
}

DLLEXPORT size_t jl_get_field_offset(jl_datatype_t *ty, int field)
{
    if (field > jl_datatype_nfields(ty))
        jl_error("This type does not have that many fields");
    return ty->fields[field].offset;
}

DLLEXPORT size_t jl_get_alignment(jl_datatype_t *ty)
{
    return ty->alignment;
}

// Takes a handle (as returned from dlopen()) and returns the absolute path to the image loaded
DLLEXPORT const char *jl_pathname_for_handle(uv_lib_t *uv_lib)
{
    if (!uv_lib)
        return NULL;

    void *handle = uv_lib->handle;
#ifdef __APPLE__
    // Iterate through all images currently in memory
    for (int32_t i = _dyld_image_count(); i >= 0 ; i--) {
        // dlopen() each image, check handle
        const char *image_name = _dyld_get_image_name(i);
        uv_lib_t *probe_lib = jl_load_dynamic_library(image_name, JL_RTLD_DEFAULT);
        void *probe_handle = probe_lib->handle;
        uv_dlclose(probe_lib);

        // If the handle is the same as what was passed in (modulo mode bits), return this image name
        if (((intptr_t)handle & (-4)) == ((intptr_t)probe_handle & (-4)))
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
    //XXX: change to jl_arrayset if array storage allocation for Array{String,1} changes:
    jl_value_t *v = jl_cstr_to_string(ModuleName);
    jl_cellset(a, jl_array_dim0(a)-1, v);
    return TRUE;
}
// Takes a handle (as returned from dlopen()) and returns the absolute path to the image loaded
DLLEXPORT int jl_dllist(jl_array_t *list)
{
    return EnumerateLoadedModules64(GetCurrentProcess(), jl_EnumerateLoadedModulesProc64, list);
}
#endif

DLLEXPORT void jl_raise_debugger(void)
{
#if defined(_OS_WINDOWS_)
    if (IsDebuggerPresent() == 1)
        DebugBreak();
#else
    raise(SIGINT);
#endif // _OS_WINDOWS_
}

#ifdef __cplusplus
}
#endif
