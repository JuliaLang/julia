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
