// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <inttypes.h>
#include "julia.h"
#include "options.h"
#include "stdio.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef ENABLE_TIMINGS
#include "timing.h"

#ifndef HAVE_TIMING_SUPPORT
#error Timings are not supported on your compiler
#endif

jl_timing_block_t *jl_root_timing;
uint64_t jl_timing_data[(int)JL_TIMING_LAST] = {0};
const char *jl_timing_names[(int)JL_TIMING_LAST] =
    {
#define X(name) #name
        JL_TIMING_OWNERS
#undef X
    };

void jl_print_timings(void)
{
    uint64_t total_time = 0;
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        total_time += jl_timing_data[i];
    }
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        if (jl_timing_data[i] != 0)
            fprintf(stderr,"%-25s : %5.2f %%   %" PRIu64 "\n", jl_timing_names[i],
                    100 * (((double)jl_timing_data[i]) / total_time), jl_timing_data[i]);
    }
}

void jl_init_timing(void)
{
    jl_root_timing = (jl_timing_block_t*)malloc_s(sizeof(jl_timing_block_t));
    _jl_timing_block_init(jl_root_timing, JL_TIMING_ROOT);
    jl_root_timing->prev = NULL;
}

void jl_destroy_timing(void)
{
    jl_timing_block_t *stack = jl_current_task ? jl_current_task->timing_stack : jl_root_timing;
    while (stack) {
        _jl_timing_block_destroy(stack);
        stack = stack->prev;
    }
    free(jl_root_timing);
}

jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block)
{
    _jl_timing_block_destroy(cur_block);
    return cur_block->prev;
}

void jl_timing_block_start(jl_timing_block_t *cur_block)
{
    _jl_timing_block_start(cur_block, cycleclock());
}

void jl_timing_block_stop(jl_timing_block_t *cur_block)
{
    _jl_timing_block_stop(cur_block, cycleclock());
}

#else

void jl_init_timing(void) { }
void jl_destroy_timing(void) { }

#endif

#ifdef __cplusplus
}
#endif
