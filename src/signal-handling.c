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
static volatile intptr_t *bt_data_prof = NULL;
static volatile size_t bt_size_max = 0;
static volatile size_t bt_size_cur = 0;
static volatile uint64_t nsecprof = 0;
static volatile int running = 0;
static const    uint64_t GIGA = 1000000000ULL;
// Timers to take samples at intervals
JL_DLLEXPORT void jl_profile_stop_timer(void);
JL_DLLEXPORT int jl_profile_start_timer(void);

static uint64_t jl_last_sigint_trigger = 0;
static void jl_clear_force_sigint(void)
{
    jl_last_sigint_trigger = 0;
}

static int jl_check_force_sigint(void)
{
    static double accum_weight = 0;
    uint64_t cur_time = uv_hrtime();
    uint64_t dt = cur_time - jl_last_sigint_trigger;
    uint64_t last_t = jl_last_sigint_trigger;
    jl_last_sigint_trigger = cur_time;
    if (last_t == 0) {
        accum_weight = 0;
        return 0;
    }
    double new_weight = accum_weight * exp(-(dt / 1e9)) + 0.3;
    if (!isnormal(new_weight))
        new_weight = 0;
    accum_weight = new_weight;
    return new_weight > 1;
}

static int exit_on_sigint = 0;
JL_DLLEXPORT void jl_exit_on_sigint(int on)
{
    exit_on_sigint = on;
}

#if defined(_WIN32)
#include "signals-win.c"
#else
#include "signals-unix.c"
#endif

// what to do on a critical error
void jl_critical_error(int sig, bt_context_t *context, uintptr_t *bt_data, size_t *bt_size)
{
    // This function is not allowed to reference any TLS variables.
    // We need to explicitly pass in the TLS buffer pointer when
    // we make `jl_filename` and `jl_lineno` thread local.
    size_t i, n = *bt_size;
    if (sig)
        jl_safe_printf("\nsignal (%d): %s\n", sig, strsignal(sig));
    jl_safe_printf("while loading %s, in expression starting on line %d\n", jl_filename, jl_lineno);
    if (context)
        *bt_size = n = rec_backtrace_ctx(bt_data, JL_MAX_BT_SIZE, context);
    for (i = 0; i < n; i++)
        jl_gdblookup(bt_data[i] - 1);
    gc_debug_print_status();
    gc_debug_critical_error();
}

///////////////////////
// Utility functions //
///////////////////////
JL_DLLEXPORT int jl_profile_init(size_t maxsize, uint64_t delay_nsec)
{
    bt_size_max = maxsize;
    nsecprof = delay_nsec;
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);
    bt_data_prof = (intptr_t*) calloc(maxsize, sizeof(intptr_t));
    if (bt_data_prof == NULL && maxsize > 0)
        return -1;
    bt_size_cur = 0;
    return 0;
}

JL_DLLEXPORT uint8_t *jl_profile_get_data(void)
{
    return (uint8_t*) bt_data_prof;
}

JL_DLLEXPORT size_t jl_profile_len_data(void)
{
    return bt_size_cur;
}

JL_DLLEXPORT size_t jl_profile_maxlen_data(void)
{
    return bt_size_max;
}

JL_DLLEXPORT uint64_t jl_profile_delay_nsec(void)
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
