// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-opt-profiling.h"

#include "julia.h"
#include "julia_internal.h"

#include <vector>

using std::vector;

// == Global lock to guard access to profiling arrays ==
int g_llvm_opt_profile_enabled = false;
vector<jl_raw_llvm_mi_t> g_llvm_opt_mi_names;

// Mapping between LLVM module name and Julia method instance.
struct jl_raw_llvm_mi_t{
  const char *module;
  jl_value_t *spec_types;

  jl_raw_llvm_mi_t(const char *jl_module, jl_value_t *jl_spec_types) : module(jl_module), spec_types(jl_spec_types) {}
};

struct jl_llvm_opt_profiling_mi_raw_results_t {
  vector<jl_raw_llvm_mi_t> mi; 
};

struct jl_raw_llvm_module_t {
  const char *module; 
  u_int16_t instructions;
  u_int16_t basicblocks;
};

struct jl_raw_llvm_opt_timing_t {
  vector<jl_raw_llvm_module_t> before;
  vector<jl_raw_llvm_module_t> after;
  uint8_t optlevel;
  uint64_t time_ns;
};

// == exported interface ==

extern "C" {

JL_DLLEXPORT void jl_start_llvm_opt_profiling() {
    g_llvm_opt_profile_enabled = true;
}

JL_DLLEXPORT void jl_stop_llvm_opt_profiling() {
    g_llvm_opt_profile_enabled = false;
}

JL_DLLEXPORT jl_llvm_opt_profiling_mi_raw_results_t jl_clear_and_fetch_llvm_opt_mi_names() {
  return jl_llvm_opt_profiling_mi_raw_results_t{g_llvm_opt_mi_names};
}

// == Called from withing `codegen.cpp` ==
void _maybe_record_llvm_mi_to_profile(const char *module_name, jl_value_t *spec_types)
{
    if (!g_llvm_opt_profile_enabled) {
      return;
    }

    g_llvm_opt_mi_names.emplace_back(module_name, spec_types);
}

}  // extern "C"
