#include <inttypes.h>
#include "julia.h"
#include "options.h"

#ifdef ENABLE_TIMINGS
#include "timing.h"

jl_timing_block_t *jl_root_timing;
uint64_t jl_timing_data[(int)JL_TIMING_LAST] = {0};
const char *jl_timing_names[(int)JL_TIMING_LAST] =
    {
#define X(name) #name
        JL_TIMING_OWNERS
#undef X
    };

extern "C" void jl_print_timings(void)
{
    uint64_t total_time = 0;
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        total_time += jl_timing_data[i];
    }
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        if (jl_timing_data[i] != 0)
            printf("%-25s : %.2f %%   %" PRIu64 "\n", jl_timing_names[i],
                    100 * (((double)jl_timing_data[i]) / total_time), jl_timing_data[i]);
    }
}

extern "C" void jl_init_timing(void)
{
    jl_root_timing = new jl_timing_block_t();
}

extern "C" void jl_destroy_timing(void)
{
    if (jl_root_task)
        delete jl_root_task->timing_stack;
}

extern "C" jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block)
{
    cur_block->~jl_timing_block_t();
    return cur_block->prev;
}

extern "C" void jl_timing_block_start(jl_timing_block_t *cur_block)
{
    cur_block->start(rdtscp());
}

extern "C" void jl_timing_block_stop(jl_timing_block_t *cur_block)
{
    cur_block->stop(rdtscp());
}

#else

extern "C" void jl_init_timing(void) { }
extern "C" void jl_destroy_timing(void) { }

#endif
