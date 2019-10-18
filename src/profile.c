#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#include "gc.h"


//
// Shared infrastructure
//

// data structures
volatile jl_bt_element_t *bt_data_prof = NULL;
volatile size_t bt_size_max = 0;
volatile size_t bt_size_cur = 0;
volatile uint8_t bt_overflow = 0;
/// only for sampling profiler
static volatile uint64_t profile_delay_nsec = 0;

volatile int profile_running = 0;

JL_DLLEXPORT void jl_profile_clear_data(void)
{
    bt_size_cur = 0;
    bt_overflow = 0;
}

JL_DLLEXPORT int jl_profile_init(size_t maxsize, uint64_t delay_nsec)
{
    bt_size_max = maxsize;
    profile_delay_nsec = delay_nsec;

    // Free previous profile buffers, if we have any
    if (bt_data_prof != NULL)
        free((void*)bt_data_prof);

    // Initialize new profile buffers.  We assume at least 10x
    bt_data_prof = (jl_bt_element_t*) calloc(bt_size_max, sizeof(jl_bt_element_t));
    if (bt_data_prof == NULL && bt_size_max > 0)
        return -1;

    jl_profile_clear_data();
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

JL_DLLEXPORT int jl_profile_did_overflow(void)
{
    return bt_overflow;
}


//
// Sampling profiler
//

JL_DLLEXPORT uint64_t jl_profile_delay_nsec(void)
{
    return profile_delay_nsec;
}

JL_DLLEXPORT int jl_profile_is_running(void)
{
    return profile_running;
}

// jl_profile_start_timer and jl_profile_stop_timer defined in signal-handling.c
