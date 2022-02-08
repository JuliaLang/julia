// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-alloc-profiler.h"

#include "julia_internal.h"
#include "gc.h"

#include <string>
#include <vector>

using std::string;
using std::vector;

struct jl_raw_backtrace_t {
    jl_bt_element_t *data;
    size_t size;
};

struct jl_raw_alloc_t {
    jl_datatype_t *type_address;
    jl_raw_backtrace_t backtrace;
    size_t size;
};

// == These structs define the global singleton profile buffer that will be used by
// callbacks to store profile results. ==
struct jl_per_thread_alloc_profile_t {
    vector<jl_raw_alloc_t> allocs;
};

struct jl_alloc_profile_t {
    double sample_rate;

    vector<jl_per_thread_alloc_profile_t> per_thread_profiles;
};

struct jl_combined_results {
    vector<jl_raw_alloc_t> combined_allocs;
};

// == Global variables manipulated by callbacks ==

jl_alloc_profile_t g_alloc_profile;
int g_alloc_profile_enabled = false;
jl_combined_results g_combined_results; // Will live forever.

// === stack stuff ===

jl_raw_backtrace_t get_raw_backtrace() JL_NOTSAFEPOINT {
    // A single large buffer to record backtraces onto
    static jl_bt_element_t static_bt_data[JL_MAX_BT_SIZE];

    size_t bt_size = rec_backtrace(static_bt_data, JL_MAX_BT_SIZE, 2);

    // Then we copy only the needed bytes out of the buffer into our profile.
    size_t bt_bytes = bt_size * sizeof(jl_bt_element_t);
    jl_bt_element_t *bt_data = (jl_bt_element_t*) malloc(bt_bytes);
    memcpy(bt_data, static_bt_data, bt_bytes);

    return jl_raw_backtrace_t{
        bt_data,
        bt_size
    };
}

// == exported interface ==

extern "C" {  // Needed since these functions doesn't take any arguments.

JL_DLLEXPORT void jl_start_alloc_profile(double sample_rate) {
    // We only need to do this once, the first time this is called.
    while (g_alloc_profile.per_thread_profiles.size() < jl_n_threads) {
        g_alloc_profile.per_thread_profiles.push_back(jl_per_thread_alloc_profile_t{});
    }

    g_alloc_profile.sample_rate = sample_rate;
    g_alloc_profile_enabled = true;
}

JL_DLLEXPORT jl_profile_allocs_raw_results_t jl_fetch_alloc_profile() {
    // combine allocs
    // TODO: interleave to preserve ordering
    for (auto& profile : g_alloc_profile.per_thread_profiles) {
        for (const auto& alloc : profile.allocs) {
            g_combined_results.combined_allocs.push_back(alloc);
        }

        profile.allocs.clear();
    }

    return jl_profile_allocs_raw_results_t{
        g_combined_results.combined_allocs.data(),
        g_combined_results.combined_allocs.size(),
    };
}

JL_DLLEXPORT void jl_stop_alloc_profile() {
    g_alloc_profile_enabled = false;
}

JL_DLLEXPORT void jl_free_alloc_profile() {
    // Free any allocs that remain in the per-thread profiles, that haven't
    // been combined yet (which happens in fetch_alloc_profiles()).
    for (auto& profile : g_alloc_profile.per_thread_profiles) {
        for (auto alloc : profile.allocs) {
            free(alloc.backtrace.data);
        }
        profile.allocs.clear();
    }

    // Free the allocs that have been already combined into the combined results object.
    for (auto alloc : g_combined_results.combined_allocs) {
        free(alloc.backtrace.data);
    }

    g_combined_results.combined_allocs.clear();
}

// == callback called into by the outside ==

void _maybe_record_alloc_to_profile(jl_value_t *val, size_t size, jl_datatype_t *type) JL_NOTSAFEPOINT {
    auto& global_profile = g_alloc_profile;
    auto thread_id = jl_atomic_load_relaxed(&jl_current_task->tid);
    auto& profile = global_profile.per_thread_profiles[thread_id];

    auto sample_val = double(rand()) / double(RAND_MAX);
    auto should_record = sample_val <= global_profile.sample_rate;
    if (!should_record) {
        return;
    }

    profile.allocs.emplace_back(jl_raw_alloc_t{
        type,
        get_raw_backtrace(),
        size
    });
}

}  // extern "C"
