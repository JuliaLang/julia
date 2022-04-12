// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-alloc-profiler.h"

#include "julia_internal.h"
#include "gc.h"

#include <string>
#include <vector>

using std::string;
using std::vector;

struct jl_raw_backtrace_t {
    size_t start_idx;
    size_t size;
};

struct jl_raw_alloc_t {
    jl_datatype_t *type_address;
    jl_raw_backtrace_t backtrace;
    size_t size;
    void *task;
    uint64_t timestamp;
};

// == These structs define the global singleton profile buffer that will be used by
// callbacks to store profile results. ==
struct jl_per_thread_alloc_profile_t {
    vector<jl_raw_alloc_t> allocs;

    // As a performance optimization, we keep this vector of backtrace frames, which the
    // jl_raw_backtrace_t objects index into. This allows us to avoid a `malloc()` for each
    // backtrace we record.
    // There are sum(trace_sizes) many backtrace frames, where each trace is appended one
    // after the other.
    vector<jl_bt_element_t> bt_frames_data;
};

struct jl_alloc_profile_t {
    double sample_rate;

    vector<jl_per_thread_alloc_profile_t> per_thread_profiles;
};

struct jl_combined_results {
    vector<jl_raw_alloc_t> combined_allocs;
    vector<jl_bt_element_t> bt_frames_data;
};

// == Global variables manipulated by callbacks ==

jl_alloc_profile_t g_alloc_profile;
int g_alloc_profile_enabled = false;
jl_combined_results g_combined_results; // Will live forever.

// === stack stuff ===

jl_raw_backtrace_t get_raw_backtrace(vector<jl_bt_element_t>& out_frames_buffer) JL_NOTSAFEPOINT {
    // We first record the backtrace onto a MAX-sized buffer, so that we don't have to
    // allocate the buffer until we know the size. To ensure thread-safety, we use a
    // per-thread backtrace buffer.
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_bt_element_t *shared_bt_data_buffer = ptls->profiling_bt_buffer;
    if (shared_bt_data_buffer == NULL) {
        size_t size = sizeof(jl_bt_element_t) * (JL_MAX_BT_SIZE + 1);
        shared_bt_data_buffer = (jl_bt_element_t*) malloc_s(size);
        ptls->profiling_bt_buffer = shared_bt_data_buffer;
    }

    size_t bt_size = rec_backtrace(shared_bt_data_buffer, JL_MAX_BT_SIZE, 2);

    // Then we copy only the needed bytes out of the buffer into our profile.
    size_t start_idx = out_frames_buffer.size();
    out_frames_buffer.insert(out_frames_buffer.end(), shared_bt_data_buffer, shared_bt_data_buffer+bt_size);

    return jl_raw_backtrace_t{
        start_idx,
        bt_size
    };
}

// == exported interface ==

extern "C" {  // Needed since these functions doesn't take any arguments.

JL_DLLEXPORT void jl_start_alloc_profile(double sample_rate) {
    // We only need to do this once, the first time this is called.
    while (g_alloc_profile.per_thread_profiles.size() < (size_t)jl_n_threads) {
        g_alloc_profile.per_thread_profiles.push_back(jl_per_thread_alloc_profile_t{});
    }

    g_alloc_profile.sample_rate = sample_rate;
    g_alloc_profile_enabled = true;
}

JL_DLLEXPORT jl_profile_allocs_raw_results_t jl_fetch_alloc_profile() {
    // combine allocs
    // TODO: interleave to preserve ordering
    for (auto& profile : g_alloc_profile.per_thread_profiles) {
        auto& out_frames_buffer = g_combined_results.bt_frames_data;
        size_t offset = out_frames_buffer.size();
        out_frames_buffer.insert(out_frames_buffer.end(), profile.bt_frames_data.begin(), profile.bt_frames_data.end());

        for (const auto& alloc : profile.allocs) {
            g_combined_results.combined_allocs.emplace_back(jl_raw_alloc_t{
                alloc.type_address,
                jl_raw_backtrace_t{alloc.backtrace.start_idx + offset, alloc.backtrace.size},
                alloc.size,
                alloc.task,
                alloc.timestamp
            });
        }

        profile.allocs.clear();
        profile.bt_frames_data.clear();
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
        profile.allocs.clear();
        profile.bt_frames_data.clear();
    }

    // Free the allocs traces that have been already combined into the combined results
    // object. (Note that these currently point into the per-thread buffers :))
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
        get_raw_backtrace(profile.bt_frames_data),
        size,
        (void *)jl_current_task,
        cycleclock()
    });
}

}  // extern "C"
