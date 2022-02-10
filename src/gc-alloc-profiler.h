// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_ALLOC_PROFILER_H
#define JL_GC_ALLOC_PROFILER_H

#include "julia.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------
// The public interface to call from Julia for allocations profiling
// ---------------------------------------------------------------------

// Forward-declaration to avoid depenency in header file.
struct jl_raw_alloc_t;  // Defined in gc-alloc-profiler.cpp

typedef struct {
    struct jl_raw_alloc_t *allocs;
    size_t num_allocs;
} jl_profile_allocs_raw_results_t;

JL_DLLEXPORT void jl_start_alloc_profile(double sample_rate);
JL_DLLEXPORT jl_profile_allocs_raw_results_t jl_fetch_alloc_profile(void);
JL_DLLEXPORT void jl_stop_alloc_profile(void);
JL_DLLEXPORT void jl_free_alloc_profile(void);

// ---------------------------------------------------------------------
// Functions to call from GC when alloc profiling is enabled
// ---------------------------------------------------------------------

void _maybe_record_alloc_to_profile(jl_value_t *val, size_t size, jl_datatype_t *typ) JL_NOTSAFEPOINT;

extern int g_alloc_profile_enabled;

#define jl_gc_unknown_type_tag ((jl_datatype_t*)0xdeadaa03)

static inline void maybe_record_alloc_to_profile(jl_value_t *val, size_t size, jl_datatype_t *typ) JL_NOTSAFEPOINT {
    if (__unlikely(g_alloc_profile_enabled)) {
        _maybe_record_alloc_to_profile(val, size, typ);
    }
}

#ifdef __cplusplus
}
#endif


#endif  // JL_GC_ALLOC_PROFILER_H
