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
#include <stdio.h>

#include "../src/julia.h"
#include "../src/julia_assert.h"

JULIA_DEFINE_FAST_TLS()

#ifdef __cplusplus
extern "C" {
#endif

static int exec_program(char *program)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TRY {
        jl_load(jl_main_module, program);
    }
    JL_CATCH {
        jl_value_t *errs = jl_stderr_obj();
        jl_value_t *e = jl_current_exception();
        // Manually save and restore the backtrace so that we print the original
        // one instead of the one caused by `show`.
        // We can't use safe_restore since that will cause any error
        // (including the ones that would have been caught) to abort.
        uintptr_t *volatile bt_data = NULL;
        size_t bt_size = ptls->bt_size;
        volatile int shown_err = 0;
        jl_printf(JL_STDERR, "error during bootstrap:\n");
        JL_TRY {
            if (errs) {
                bt_data = (uintptr_t*)malloc(bt_size * sizeof(void*));
                memcpy(bt_data, ptls->bt_data, bt_size * sizeof(void*));
                jl_value_t *showf = jl_get_function(jl_base_module, "show");
                if (showf != NULL) {
                    jl_call2(showf, errs, e);
                    jl_printf(JL_STDERR, "\n");
                    shown_err = 1;
                }
            }
        }
        JL_CATCH {
        }
        if (bt_data) {
            ptls->bt_size = bt_size;
            memcpy(ptls->bt_data, bt_data, bt_size * sizeof(void*));
            free(bt_data);
        }
        if (!shown_err) {
            jl_static_show(JL_STDERR, e);
            jl_printf(JL_STDERR, "\n");
        }
        jlbacktrace();
        jl_printf(JL_STDERR, "\n");
        return 1;
    }
    return 0;
}

void jl_lisp_prompt();

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
                jl_static_show(JL_STDERR, jl_current_exception());
                jl_exception_clear();
            }
            else if (val) {
                jl_static_show(JL_STDOUT, val);
            }
            jl_printf(JL_STDOUT, "\n");
            free(line);
            line = NULL;
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

void JL_DLLEXPORT jl_initialize() {
    char* argv2[]={"./julia", "-C", "\"native\"", "--compile=no", "--startup-file=no", "-J", "sys.ji", 0};
    int argc = 9;
    char *argv = argv2;
    libsupport_init();
    jl_parse_opts(&argc, (char***)&argv);
    julia_init(jl_options.image_file_specified ? JL_IMAGE_CWD : JL_IMAGE_JULIA_HOME);
}

void JL_DLLEXPORT jl_eval_and_print(const char *line)
{
    JL_TRY {
        jl_value_t *val = (jl_value_t*)jl_eval_string(line);
        if (jl_exception_occurred()) {
            jl_printf(JL_STDERR, "error during run:\n");
            jl_static_show(JL_STDERR, jl_current_exception());
            jl_exception_clear();
        }
        else if (val) {
            jl_static_show(JL_STDOUT, val);
        }
        jl_printf(JL_STDOUT, "\n");
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "\nparser error:\n");
        jl_static_show(JL_STDERR, jl_current_exception());
        jl_printf(JL_STDERR, "\n");
        jlbacktrace();
    }
}

int main(int argc, char *argv[]) {
    char* argv2[]={"./julia", "-C", "\"native\"", "--compile=no", "--startup-file=no", "-J", "sys.ji", 0};
    argc = 9;
    argv = argv2;
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
