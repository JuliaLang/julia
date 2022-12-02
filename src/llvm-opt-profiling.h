// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_LLVM_OPT_PROFILING_H
#define JL_LLVM_OPT_PROFILING_H

#include "julia.h"

#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------
// The public interface to call from Julia for allocations profiling
// ---------------------------------------------------------------------

struct jl_raw_llvm_mi_t;  // Defined in llvm-opt-profiling.cpp
struct jl_llvm_opt_profiling_mi_raw_results_t;

JL_DLLEXPORT void jl_start_llvm_opt_profiling();
JL_DLLEXPORT void jl_stop_llvm_opt_profiling();
JL_DLLEXPORT jl_llvm_opt_profiling_mi_raw_results_t jl_clear_and_fetch_llvm_opt_mi_names();

// ---------------------------------------------------------------------
// Functions to call from LLVM optimization for profiling
// ---------------------------------------------------------------------

void _maybe_record_llvm_mi_to_profile(const char *module_name, jl_value_t *spec_types) JL_NOTSAFEPOINT;

extern int g_llvm_opt_profile_enabled;

static inline void maybe_record_llvm_mi_to_profile(const char *module_name, jl_value_t *spec_types) JL_NOTSAFEPOINT {
    if (__unlikely(g_llvm_opt_profile_enabled)) {
        _maybe_record_llvm_mi_to_profile(module_name, spec_types);
    }
}

#ifdef __cplusplus
}
#endif


#endif  // JL_LLVM_OPT_PROFILING_H
