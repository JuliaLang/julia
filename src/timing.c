// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <inttypes.h>
#include "julia.h"
#include "julia_internal.h"
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

static uint64_t t0;
#ifdef USE_TRACY
/**
 * These sources often generate millions of events / minute. Although Tracy
 * can generally keep up with that, those events also bloat the saved ".tracy"
 * files, so we disable them by default.
 **/
JL_DLLEXPORT uint64_t jl_timing_enable_mask = 0xFFFFFFFFFFFFFFFF &
                                              ~(1ull << JL_TIMING_ROOT) &
                                              ~(1ull << JL_TIMING_TYPE_CACHE_LOOKUP) &
                                              ~(1ull << JL_TIMING_METHOD_MATCH) &
                                              ~(1ull << JL_TIMING_METHOD_LOOKUP_FAST) &
                                              ~(1ull << JL_TIMING_AST_COMPRESS) &
                                              ~(1ull << JL_TIMING_AST_UNCOMPRESS);
#else
JL_DLLEXPORT uint64_t jl_timing_enable_mask = 0xFFFFFFFFFFFFFFFF;
#endif

JL_DLLEXPORT uint64_t jl_timing_counts[(int)JL_TIMING_LAST] = {0};
const char *jl_timing_names[(int)JL_TIMING_LAST] =
    {
#define X(name) #name
        JL_TIMING_OWNERS
#undef X
    };

void jl_print_timings(void)
{
    uint64_t total_time = cycleclock() - t0;
    uint64_t root_time = total_time;
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        root_time -= jl_timing_counts[i];
    }
    jl_timing_counts[0] = root_time;
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        if (jl_timing_counts[i] != 0)
            fprintf(stderr, "%-25s : %5.2f %%   %" PRIu64 "\n", jl_timing_names[i],
                    100 * (((double)jl_timing_counts[i]) / total_time), jl_timing_counts[i]);
    }
}

void jl_init_timing(void)
{
    t0 = cycleclock();
}

void jl_destroy_timing(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_timing_block_t *stack = ptls->timing_stack;
    while (stack) {
        _jl_timing_block_destroy(stack);
        stack = stack->prev;
    }
}

jl_timing_block_t *jl_pop_timing_block(jl_timing_block_t *cur_block)
{
    _jl_timing_block_destroy(cur_block);
    return cur_block->prev;
}

void jl_timing_block_enter_task(jl_task_t *ct, jl_ptls_t ptls, jl_timing_block_t *prev_blk)
{
    if (prev_blk != NULL) {
        assert(ptls->timing_stack == NULL);

        ptls->timing_stack = prev_blk;
        if (prev_blk != NULL) {
            _COUNTS_START(&prev_blk->counts_ctx, cycleclock());
        }
    }

#ifdef USE_TRACY
    TracyCFiberEnter(ct->name);
#else
    (void)ct;
#endif
}

jl_timing_block_t *jl_timing_block_exit_task(jl_task_t *ct, jl_ptls_t ptls)
{
#ifdef USE_TRACY
    // Tracy is fairly strict about not leaving a fiber that
    // hasn't been entered, which happens often when
    // connecting to a running Julia session.
    //
    // Eventually, Tracy will support telling the server that
    // which fibers are active upon connection, but until then
    // work around around the problem by just entering the new
    // fiber directly, which implicitly leaves any active fibers.

    //TracyCFiberLeave;
#endif
    (void)ct;

    jl_timing_block_t *blk = ptls->timing_stack;
    ptls->timing_stack = NULL;

    if (blk != NULL) {
        _COUNTS_STOP(&blk->counts_ctx, cycleclock());
    }
    return blk;
}

static inline const char *gnu_basename(const char *path)
{
    char *base = strrchr(path, '/');
    return base ? base+1 : path;
}

JL_DLLEXPORT void jl_timing_show(jl_value_t *v, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    ios_t buf;
    ios_mem(&buf, IOS_INLSIZE);
    buf.growable = 0; // Restrict to inline buffer to avoid allocation

    jl_static_show((JL_STREAM*)&buf, v);
    if (buf.size == buf.maxsize)
        memset(&buf.buf[IOS_INLSIZE - 3], '.', 3);

    TracyCZoneText(*(cur_block->tracy_ctx), buf.buf, buf.size);
#endif
}

JL_DLLEXPORT void jl_timing_show_module(jl_module_t *m, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    const char *module_name = jl_symbol_name(m->name);
    TracyCZoneText(*(cur_block->tracy_ctx), module_name, strlen(module_name));
#endif
}

JL_DLLEXPORT void jl_timing_show_filename(const char *path, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    const char *filename = gnu_basename(path);
    TracyCZoneText(*(cur_block->tracy_ctx), filename, strlen(filename));
#endif
}

JL_DLLEXPORT void jl_timing_show_method_instance(jl_method_instance_t *mi, jl_timing_block_t *cur_block)
{
    jl_timing_show_func_sig(mi->specTypes, cur_block);
    jl_method_t *def = mi->def.method;
    jl_timing_printf(cur_block, "%s:%d in %s",
                     gnu_basename(jl_symbol_name(def->file)),
                     def->line,
                     jl_symbol_name(def->module->name));
}

JL_DLLEXPORT void jl_timing_show_func_sig(jl_value_t *v, jl_timing_block_t *cur_block)
{
#ifdef USE_TRACY
    ios_t buf;
    ios_mem(&buf, IOS_INLSIZE);
    buf.growable = 0; // Restrict to inline buffer to avoid allocation

    jl_static_show_func_sig((JL_STREAM*)&buf, v);
    if (buf.size == buf.maxsize)
        memset(&buf.buf[IOS_INLSIZE - 3], '.', 3);

    TracyCZoneText(*(cur_block->tracy_ctx), buf.buf, buf.size);
#endif
}

JL_DLLEXPORT void jl_timing_printf(jl_timing_block_t *cur_block, const char *format, ...)
{
    va_list args;
    va_start(args, format);

#ifdef USE_TRACY
    ios_t buf;
    ios_mem(&buf, IOS_INLSIZE);
    buf.growable = 0; // Restrict to inline buffer to avoid allocation

    jl_vprintf((JL_STREAM*)&buf, format, args);
    if (buf.size == buf.maxsize)
        memset(&buf.buf[IOS_INLSIZE - 3], '.', 3);

    TracyCZoneText(*(cur_block->tracy_ctx), buf.buf, buf.size);
#endif
    va_end(args);
}

JL_DLLEXPORT int jl_timing_set_enable(const char *subsystem, uint8_t enabled)
{
    for (int i = 0; i < JL_TIMING_LAST; i++) {
        if (strcmp(subsystem, jl_timing_names[i]) == 0) {
            uint64_t subsystem_bit = (1ul << i);
            if (enabled) {
                jl_timing_enable_mask |= subsystem_bit;
            } else {
                jl_timing_enable_mask &= ~subsystem_bit;
            }
            return 0;
        }
    }
    return -1;
}

#else

void jl_init_timing(void) { }
void jl_destroy_timing(void) { }
JL_DLLEXPORT int jl_timing_set_enable(const char *subsystem, uint8_t enabled) { return -1; }

#endif

#ifdef __cplusplus
}
#endif
