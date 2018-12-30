// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  repl.c
  system startup, main(), and console interaction
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <ctype.h>
#include <inttypes.h>

#include "uv.h"
#include "../src/julia.h"
#include "../src/julia_assert.h"

JULIA_DEFINE_FAST_TLS()

#ifdef __cplusplus
extern "C" {
#endif

static int exec_program(char *program)
{
    JL_TRY {
        jl_load(jl_main_module, program);
    }
    JL_CATCH {
        jl_value_t *errs = jl_stderr_obj();
        volatile int shown_err = 0;
        jl_printf(JL_STDERR, "error during bootstrap:\n");
        JL_TRY {
            if (errs) {
                jl_value_t *showf = jl_get_function(jl_base_module, "show");
                if (showf != NULL) {
                    jl_call2(showf, errs, jl_current_exception());
                    jl_printf(JL_STDERR, "\n");
                    shown_err = 1;
                }
            }
        }
        JL_CATCH {
        }
        if (!shown_err) {
            jl_static_show(JL_STDERR, jl_current_exception());
            jl_printf(JL_STDERR, "\n");
        }
        jlbacktrace();
        jl_printf(JL_STDERR, "\n");
        return 1;
    }
    return 0;
}

void jl_lisp_prompt();

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
            jl_no_exc_handler(jl_current_exception());
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
                jl_static_show(JL_STDERR, jl_exception_occurred());
                jl_exception_clear();
            }
            else if (val) {
                jl_static_show(JL_STDOUT, val);
            }
            jl_printf(JL_STDOUT, "\n");
            free(line);
            line = NULL;
#ifndef JL_DISABLE_LIBUV
            uv_run(jl_global_event_loop(),UV_RUN_NOWAIT);
#endif
        }
        JL_CATCH {
            if (line) {
                free(line);
                line = NULL;
            }
            jl_printf(JL_STDERR, "\nparser error:\n");
            jl_static_show(JL_STDERR, jl_current_exception());
            jl_printf(JL_STDERR, "\n");
            jlbacktrace();
        }
    }
    return 0;
}

#ifndef _OS_WINDOWS_
int main(int argc, char *argv[])
{
#ifndef JL_DISABLE_LIBUV
    uv_setup_args(argc, argv); // no-op on Windows
#endif
#else

static void lock_low32() {
#if defined(_P64) && defined(JL_DEBUG_BUILD)
    // Wine currently has a that causes it to answer VirtualQuery incorrectly.
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
            if (last > max32addr)
                last = max32addr;
            // adjust first up to the first allocation granularity boundary
            // adjust last down to the last allocation granularity boundary
            first = (char*)(((long long)first + info.dwAllocationGranularity - 1) & ~(info.dwAllocationGranularity - 1));
            last = (char*)((long long)last & ~(info.dwAllocationGranularity - 1));
            if (last != first) {
                void *p = VirtualAlloc(first, last - first, MEM_RESERVE, PAGE_NOACCESS); // reserve all memory in between
                if ((char*)p != first)
                    // Wine and Windows10 seem to have issues with reporting memory access information correctly
                    // so we sometimes end up with unexpected results - this is just ignore those and continue
                    // this is just a debugging aid to help find accidental pointer truncation anyways, so it's not critical
                    VirtualFree(p, 0, MEM_RELEASE);
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
    libsupport_init();
    int lisp_prompt = (argc >= 2 && strcmp((char*)argv[1],"--lisp") == 0);
    if (lisp_prompt) {
        memmove(&argv[1], &argv[2], (argc-2)*sizeof(void*));
        argc--;
    }
    jl_parse_opts(&argc, (char***)&argv);
    julia_init(jl_options.image_file_specified ? JL_IMAGE_CWD : JL_IMAGE_JULIA_HOME);
    if (lisp_prompt) {
        jl_get_ptls_states()->world_age = jl_get_world_counter();
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
