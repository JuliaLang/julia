// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <threading.h>

// Profiler control variables //
static volatile ptrint_t *bt_data_prof = NULL;
static volatile size_t bt_size_max = 0;
static volatile size_t bt_size_cur = 0;
static volatile u_int64_t nsecprof = 0;
static volatile int running = 0;
static const    u_int64_t GIGA = 1000000000ULL;
// Timers to take samples at intervals
JL_DLLEXPORT void jl_profile_stop_timer(void);
JL_DLLEXPORT int jl_profile_start_timer(void);


volatile sig_atomic_t jl_signal_pending = 0;
volatile sig_atomic_t jl_defer_signal = 0;


int exit_on_sigint = 0;
JL_DLLEXPORT void jl_exit_on_sigint(int on) {exit_on_sigint = on;}

// what to do on SIGINT
JL_DLLEXPORT void jl_sigint_action(void)
{
    if (exit_on_sigint) jl_exit(130); // 128+SIGINT
    jl_throw(jl_interrupt_exception);
}

static void jl_critical_error(int sig, bt_context_t context, ptrint_t *bt_data, size_t *bt_size);

#if defined(_WIN32)
#include "signals-win.c"
#else
#include "signals-unix.c"
#endif

// what to do on a critical error
static void jl_critical_error(int sig, bt_context_t context, ptrint_t *bt_data, size_t *bt_size)
{
    size_t n = *bt_size;
    if (sig)
        jl_safe_printf("\nsignal (%d): %s\n", sig, strsignal(sig));
    jl_safe_printf("while loading %s, in expression starting on line %d\n", jl_filename, jl_lineno);
    if (context)
        *bt_size = n = rec_backtrace_ctx(bt_data, JL_MAX_BT_SIZE, context);
    for(size_t i=0; i < n; i++)
        gdblookup(bt_data[i]);
    gc_debug_print_status();
}


///////////////////////
// Utility functions //
///////////////////////
JL_DLLEXPORT int jl_profile_init(size_t maxsize, u_int64_t delay_nsec)
{
    bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);
    bt_data_prof = (ptrint_t*) calloc(maxsize, sizeof(ptrint_t));
    if (bt_data_prof == NULL && maxsize > 0)
        return -1;
    bt_size_cur = 0;
    return 0;
}

JL_DLLEXPORT u_int8_t *jl_profile_get_data(void)
{
    return (u_int8_t*) bt_data_prof;
}

JL_DLLEXPORT size_t jl_profile_len_data(void)
{
    return bt_size_cur;
}

JL_DLLEXPORT size_t jl_profile_maxlen_data(void)
{
    return bt_size_max;
}

JL_DLLEXPORT u_int64_t jl_profile_delay_nsec(void)
{
    return nsecprof;
}

JL_DLLEXPORT void jl_profile_clear_data(void)
{
    bt_size_cur = 0;
}

JL_DLLEXPORT int jl_profile_is_running(void)
{
    return running;
}

#ifdef __cplusplus
}
#endif
