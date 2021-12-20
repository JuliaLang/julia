// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_ALLOC_PROFILER_H
#define JL_GC_ALLOC_PROFILER_H

#include "julia.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif

struct TypeNamePair {
    size_t addr;
    const char *name;
};

struct FreeInfo {
    size_t type_addr;
    size_t count;
};

struct RawAllocResults {
    void *allocs; // Alloc* (see gc-alloc-profiler.cpp)
    size_t num_allocs;

    struct FreeInfo *frees;
    size_t num_frees;
};

// TODO(PR): Is this correct? Are these JL_NOTSAFEPOINT?
void _report_gc_started(void) JL_NOTSAFEPOINT;
void _report_gc_finished(
    uint64_t pause, uint64_t freed, uint64_t allocd, int full, int recollect
) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_start_alloc_profile(int skip_every);
JL_DLLEXPORT struct RawAllocResults jl_stop_alloc_profile(void);
JL_DLLEXPORT void jl_free_alloc_profile(void);

void _record_allocated_value(jl_value_t *val, size_t size) JL_NOTSAFEPOINT;
void _record_freed_value(jl_taggedvalue_t *tagged_val) JL_NOTSAFEPOINT;

// ---------------------------------------------------------------------
// functions to call from GC when alloc profiling is enabled
// ---------------------------------------------------------------------

extern int g_alloc_profile_enabled;

static inline void record_allocated_value(jl_value_t *val, size_t size) JL_NOTSAFEPOINT {
    if (__unlikely(g_alloc_profile_enabled)) {
        _record_allocated_value(val, size);
    }
}

static inline void record_freed_value(jl_taggedvalue_t *tagged_val) JL_NOTSAFEPOINT {
    if (__unlikely(g_alloc_profile_enabled != 0)) {
        _record_freed_value(tagged_val);
    }
}

#ifdef __cplusplus
}
#endif


#endif  // JL_GC_ALLOC_PROFILER_H
