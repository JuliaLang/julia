// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  repl.c
  system startup, main(), and console interaction
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <signal.h>
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <ctype.h>
#include <inttypes.h>

#include "uv.h"
#define WHOLE_ARCHIVE
#include "../src/julia.h"

#ifdef __cplusplus
extern "C" {
#endif

#if defined(JULIA_ENABLE_THREADING) && !defined(_OS_DARWIN_) && !defined(_OS_WINDOWS_)
JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t jl_get_ptls_states_static(void)
{
    static __attribute__((tls_model("local-exec"))) __thread jl_tls_states_t tls_states;
    return &tls_states;
}
__attribute__((constructor)) void jl_register_ptls_states_getter(void)
{
    // We need to make sure this function is called before any reference to
    // TLS variables.
    jl_set_ptls_states_getter(jl_get_ptls_states_static);
}
#endif

static int exec_program(char *program)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TRY {
        jl_load(program);
    }
    JL_CATCH {
        jl_value_t *errs = jl_stderr_obj();
        jl_value_t *e = ptls->exception_in_transit;
        // Manually save and restore the backtrace so that we print the original
        // one instead of the one caused by `jl_show`.
        // We can't use safe_restore since that will cause any error
        // (including the ones that would have been caught) to abort.
        uintptr_t *volatile bt_data = NULL;
        size_t bt_size = ptls->bt_size;
        JL_TRY {
            if (errs) {
                bt_data = (uintptr_t*)malloc(bt_size * sizeof(void*));
                memcpy(bt_data, ptls->bt_data, bt_size * sizeof(void*));
                jl_show(errs, e);
                jl_printf(JL_STDERR, "\n");
                free(bt_data);
            }
        }
        JL_CATCH {
            ptls->bt_size = bt_size;
            memcpy(ptls->bt_data, bt_data, bt_size * sizeof(void*));
            free(bt_data);
            errs = NULL;
        }
        if (!errs) {
            jl_printf(JL_STDERR, "error during bootstrap:\n");
            jl_static_show(JL_STDERR, e);
            jl_printf(JL_STDERR, "\n");
            jlbacktrace();
            jl_printf(JL_STDERR, "\n");
        }
        return 1;
    }
    return 0;
}

void jl_lisp_prompt();

#ifndef _WIN32
int jl_repl_raise_sigtstp(void)
{
    return raise(SIGTSTP);
}
#endif

#ifdef JL_GF_PROFILE
static void print_profile(void)
{
    size_t i;
    void **table = jl_base_module->bindings.table;
    for(i=1; i < jl_base_module->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->value != NULL && jl_is_function(b->value) &&
                jl_is_gf(b->value)) {
                jl_printf(JL_STDERR, "%d\t%s\n",
                           jl_gf_mtable(b->value)->ncalls,
                           jl_gf_name(b->value)->name);
            }
        }
    }
}
#endif

static NOINLINE int true_main(int argc, char *argv[])
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_set_ARGS(argc, argv);

    jl_function_t *start_client = jl_base_module ?
        (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("_start")) : NULL;

    if (start_client) {
        JL_TRY {
            size_t last_age = jl_get_ptls_states()->world_age;
            jl_get_ptls_states()->world_age = jl_get_world_counter();
            jl_apply(&start_client, 1);
            jl_get_ptls_states()->world_age = last_age;
        }
        JL_CATCH {
            jl_no_exc_handler(jl_exception_in_transit);
        }
        return 0;
    }

    // run program if specified, otherwise enter REPL
    if (argc > 0) {
        if (strcmp(argv[0], "-")) {
            return exec_program(argv[0]);
        }
    }

    ios_puts("WARNING: Base._start not defined, falling back to economy mode repl.\n", ios_stdout);
    if (!jl_errorexception_type)
        ios_puts("WARNING: jl_errorexception_type not defined; any errors will be fatal.\n", ios_stdout);

    while (!ios_eof(ios_stdin)) {
        char *volatile line = NULL;
        JL_TRY {
            ios_puts("\njulia> ", ios_stdout);
            ios_flush(ios_stdout);
            line = ios_readline(ios_stdin);
            jl_value_t *val = (jl_value_t*)jl_eval_string(line);
            if (jl_exception_occurred()) {
                jl_printf(JL_STDERR, "error during run:\n");
                jl_static_show(JL_STDERR, ptls->exception_in_transit);
                jl_exception_clear();
            }
            else if (val) {
                jl_static_show(JL_STDOUT, val);
            }
            jl_printf(JL_STDOUT, "\n");
            free(line);
            line = NULL;
            uv_run(jl_global_event_loop(),UV_RUN_NOWAIT);
        }
        JL_CATCH {
            if (line) {
                free(line);
                line = NULL;
            }
            jl_printf(JL_STDERR, "\nparser error:\n");
            jl_static_show(JL_STDERR, ptls->exception_in_transit);
            jl_printf(JL_STDERR, "\n");
            jlbacktrace();
        }
    }
    return 0;
}

extern JL_DLLEXPORT uint64_t jl_cpuid_tag();

#ifndef _OS_WINDOWS_
int main(int argc, char *argv[])
{
    uv_setup_args(argc, argv); // no-op on Windows
#else

#if defined(_P64) && defined(JL_DEBUG_BUILD)
static int is_running_under_wine()
{
    static const char * (CDECL *pwine_get_version)(void);
    HMODULE hntdll = GetModuleHandle("ntdll.dll");
    assert(hntdll);
    pwine_get_version = (void *)GetProcAddress(hntdll, "wine_get_version");
    return pwine_get_version != 0;
}
#endif

static void lock_low32() {
#if defined(_P64) && defined(JL_DEBUG_BUILD)
    // Wine currently has a that causes it to answer VirtualQuery incorrectly.
    // See https://www.winehq.org/pipermail/wine-devel/2016-March/112188.html for details
    int under_wine = is_running_under_wine();
    // block usage of the 32-bit address space on win64, to catch pointer cast errors
    char *const max32addr = (char*)0xffffffffL;
    SYSTEM_INFO info;
    MEMORY_BASIC_INFORMATION meminfo;
    GetNativeSystemInfo(&info);
    memset(&meminfo, 0, sizeof(meminfo));
    meminfo.BaseAddress = info.lpMinimumApplicationAddress;
    while ((char*)meminfo.BaseAddress < max32addr) {
        size_t nbytes = VirtualQuery(meminfo.BaseAddress, &meminfo, sizeof(meminfo));
        assert(nbytes == sizeof(meminfo));
        if (meminfo.State == MEM_FREE) { // reserve all free pages in the first 4GB of memory
            char *first = (char*)meminfo.BaseAddress;
            char *last = first + meminfo.RegionSize;
            char *p;
            if (last > max32addr)
                last = max32addr;
            // adjust first up to the first allocation granularity boundary
            // adjust last down to the last allocation granularity boundary
            first = (char*)(((long long)first + info.dwAllocationGranularity - 1) & ~(info.dwAllocationGranularity - 1));
            last = (char*)((long long)last & ~(info.dwAllocationGranularity - 1));
            if (last != first) {
                p = VirtualAlloc(first, last - first, MEM_RESERVE, PAGE_NOACCESS); // reserve all memory in between
                assert(under_wine || p == first);
            }
        }
        meminfo.BaseAddress += meminfo.RegionSize;
    }
#endif
}
int wmain(int argc, wchar_t *argv[], wchar_t *envp[])
{
    int i;
    lock_low32();
    for (i=0; i<argc; i++) { // write the command line to UTF8
        wchar_t *warg = argv[i];
        size_t len = WideCharToMultiByte(CP_UTF8, 0, warg, -1, NULL, 0, NULL, NULL);
        if (!len) return 1;
        char *arg = (char*)alloca(len);
        if (!WideCharToMultiByte(CP_UTF8, 0, warg, -1, arg, len, NULL, NULL)) return 1;
        argv[i] = (wchar_t*)arg;
    }
#endif
    if (argc >= 2 && strcmp((char *)argv[1], "--cpuid") == 0) {
        /* Used by the build system to name CPUID-specific binaries */
        printf("%" PRIx64, jl_cpuid_tag());
        return 0;
    }
    libsupport_init();
    int lisp_prompt = (argc >= 2 && strcmp((char*)argv[1],"--lisp") == 0);
    if (lisp_prompt) {
        memmove(&argv[1], &argv[2], (argc-2)*sizeof(void*));
        argc--;
    }
    jl_parse_opts(&argc, (char***)&argv);
    julia_init(jl_options.image_file_specified ? JL_IMAGE_CWD : JL_IMAGE_JULIA_HOME);
    if (lisp_prompt) {
        jl_lisp_prompt();
        return 0;
    }
    int ret = true_main(argc, (char**)argv);
    jl_atexit_hook(ret);
    return ret;
}

#ifdef __cplusplus
}
#endif
