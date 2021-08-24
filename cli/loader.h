// This file is a part of Julia. License is MIT: https://julialang.org/license

/* Bring in definitions for `_OS_X_`, `PATH_MAX` and `PATHSEPSTRING`, `jl_ptls_t`, etc... */
#include "../src/support/platform.h"
#include "../src/support/dirpath.h"
#include "../src/julia_fasttls.h"

#ifdef _OS_WINDOWS_
/* We need to reimplement a bunch of standard library stuff on windows,
 * but we want to make sure that it doesn't conflict with the actual implementations
 * once those get linked into this process. */
#define fwrite loader_fwrite
#define fputs loader_fputs
#define exit loader_exit
#define strlen loader_strlen
#define wcslen loader_wcslen
#define strncat loader_strncat
#define memcpy loader_memcpy
#define dirname loader_dirname
#define strchr loader_strchr
#define malloc loader_malloc
#define realloc loader_realloc
#endif

#ifdef _OS_WINDOWS_
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#ifdef _OS_DARWIN_
#include <mach-o/dyld.h>
#endif
#ifdef _OS_FREEBSD_
#include <stddef.h>
#include <sys/sysctl.h>
#endif

#define _GNU_SOURCE // Need this for `dladdr()`
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <libgen.h>
#include <unistd.h>
#include <dlfcn.h>
#endif

// Borrow definition from `support/dtypes.h`
#ifdef _OS_WINDOWS_
# ifdef LIBRARY_EXPORTS
#  define JL_DLLEXPORT __declspec(dllexport)
# else
#  define JL_DLLEXPORT __declspec(dllimport)
# endif
#define JL_HIDDEN
#else
# if defined(LIBRARY_EXPORTS) && defined(_OS_LINUX)
#  define JL_DLLEXPORT __attribute__ ((visibility("protected")))
# else
#  define JL_DLLEXPORT __attribute__ ((visibility("default")))
# endif
#define JL_HIDDEN    __attribute__ ((visibility("hidden")))
#endif
/*
 * DEP_LIBS is our list of dependent libraries that must be loaded before `libjulia`.
 * Note that order matters, as each entry will be opened in-order.  We define here a
 * dummy value just so this file compiles on its own, and also so that developers can
 * see what this value should look like.  Note that the last entry must always be
 * `libjulia`, and that all paths should be relative to this loader library path.
 */
#if !defined(DEP_LIBS)
#define DEP_LIBS "../lib/example.so:../lib/libjulia.so"
#endif

// We need to dlopen() ourselves in order to introspect the libdir.
#if defined(JL_DEBUG_BUILD)
#define LIBJULIA_NAME "libjulia-debug"
#else
#define LIBJULIA_NAME "libjulia"
#endif


// Declarations from `loader_lib.c` and `loader_win_utils.c`
JL_DLLEXPORT extern int jl_load_repl(int, char **);
JL_DLLEXPORT void jl_loader_print_stderr(const char * msg);
void jl_loader_print_stderr3(const char * msg1, const char * msg2, const char * msg3);
static void * lookup_symbol(const void * lib_handle, const char * symbol_name);

#ifdef _OS_WINDOWS_
LPWSTR *CommandLineToArgv(LPWSTR lpCmdLine, int *pNumArgs);
int wchar_to_utf8(const wchar_t * wstr, char *str, size_t maxlen);
int utf8_to_wchar(const char * str, wchar_t *wstr, size_t maxlen);
void setup_stdio(void);
#endif

#ifdef _OS_WINDOWS_
typedef DWORD jl_thread_t;
#else
typedef pthread_t jl_thread_t;
#endif

// Recursive spin lock
typedef struct {
    volatile jl_thread_t owner;
    u_int32_t count;
} jl_mutex_t;

typedef struct {
    int8_t quiet;
    int8_t banner;
    const char *julia_bindir;
    const char *julia_bin;
    const char **cmds;
    const char *image_file;
    const char *cpu_target;
    int32_t nthreads;
    int32_t nprocs;
    const char *machine_file;
    const char *project;
    int8_t isinteractive;
    int8_t color;
    int8_t historyfile;
    int8_t startupfile;
    int8_t compile_enabled;
    int8_t code_coverage;
    int8_t malloc_log;
    int8_t opt_level;
    int8_t opt_level_min;
    int8_t debug_level;
    int8_t check_bounds;
    int8_t depwarn;
    int8_t warn_overwrite;
    int8_t can_inline;
    int8_t polly;
    const char *trace_compile;
    int8_t fast_math;
    int8_t worker;
    const char *cookie;
    int8_t handle_signals;
    int8_t use_sysimage_native_code;
    int8_t use_compiled_modules;
    const char *bindto;
    const char *outputbc;
    const char *outputunoptbc;
    const char *outputo;
    const char *outputasm;
    const char *outputji;
    const char *output_code_coverage;
    int8_t incremental;
    int8_t image_file_specified;
    int8_t warn_scope;
    int8_t image_codegen;
    int8_t rr_detach;
} jl_options_t;
