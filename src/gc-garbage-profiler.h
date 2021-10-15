// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_GARBAGE_PROFILER_H
#define JL_GC_GARBAGE_PROFILER_H

#include "julia.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT void jl_start_logging(ios_t *stream);
JL_DLLEXPORT void jl_stop_logging();

JL_DLLEXPORT void jl_start_garbage_profile(ios_t *stream);
JL_DLLEXPORT void jl_stop_garbage_profile(void);

void _report_gc_started(void);
void _report_gc_finished(uint64_t pause, uint64_t freed, uint64_t allocd);
void _record_allocated_value(jl_value_t *val);
void _record_freed_value(jl_taggedvalue_t *tagged_val);

// ---------------------------------------------------------------------
// functions to call from GC when garbage profiling is enabled
// ---------------------------------------------------------------------

extern ios_t *g_garbage_profile_stream; // TODO: replace w/ bool?
extern ios_t *g_gc_log_stream;

static inline void record_allocated_value(jl_value_t *val) {
    if (__unlikely(g_garbage_profile_stream != 0)) {
        _record_allocated_value(val);
    }
}

static inline void record_freed_value(jl_taggedvalue_t *tagged_val) {
    if (__unlikely(g_garbage_profile_stream != 0)) {
        _record_freed_value(tagged_val);
    }
}

#ifdef __cplusplus
}
#endif


#endif  // JL_GC_GARBAGE_PROFILER_H
