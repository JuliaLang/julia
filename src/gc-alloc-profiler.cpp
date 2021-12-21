// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-alloc-profiler.h"

#include "julia_internal.h"
#include "gc.h"

#include <string>
#include <unordered_map>
#include <vector>

using std::unordered_map;
using std::string;
using std::vector;

struct RawBacktrace {
    jl_bt_element_t *data;
    size_t size;
};

struct RawAlloc {
    jl_datatype_t *type_address;
    RawBacktrace backtrace;
    size_t size;
};

struct PerThreadAllocProfile {
    vector<RawAlloc> allocs;
    unordered_map<size_t, size_t> type_address_by_value_address;
    unordered_map<size_t, size_t> frees_by_type_address;

    size_t alloc_counter;
    size_t last_recorded_alloc;
};

struct AllocProfile {
    int skip_every;

    vector<PerThreadAllocProfile> per_thread_profiles;
};

struct CombinedResults {
    vector<RawAlloc> combined_allocs;
    vector<FreeInfo> combined_frees;
};

// == global variables manipulated by callbacks ==

AllocProfile g_alloc_profile;
int g_alloc_profile_enabled = false;
CombinedResults g_combined_results; // will live forever

// === stack stuff ===

RawBacktrace get_raw_backtrace() {
    static jl_bt_element_t static_bt_data[JL_MAX_BT_SIZE];

    // TODO: tune the number of frames that are skipped
    size_t bt_size = rec_backtrace(static_bt_data, JL_MAX_BT_SIZE, 1);

    size_t bt_bytes = bt_size * sizeof(jl_bt_element_t);
    jl_bt_element_t *bt_data = (jl_bt_element_t*) malloc(bt_bytes);
    memcpy(bt_data, static_bt_data, bt_bytes);

    return RawBacktrace{
        bt_data,
        bt_size
    };
}

// == exported interface ==

JL_DLLEXPORT void jl_start_alloc_profile(int skip_every) {
    g_alloc_profile = AllocProfile{skip_every};

    for (int i = 0; i < jl_n_threads; i++) {
        g_alloc_profile.per_thread_profiles.push_back(PerThreadAllocProfile{});
    }

    g_alloc_profile_enabled = true;
}

extern "C" {  // Needed since the function doesn't take any arguments.

JL_DLLEXPORT struct RawAllocResults jl_stop_alloc_profile() {
    g_alloc_profile_enabled = false;

    // combine allocs
    // TODO: interleave to preserve ordering
    for (const auto& profile : g_alloc_profile.per_thread_profiles) {
        for (const auto& alloc : profile.allocs) {
            g_combined_results.combined_allocs.push_back(alloc);
        }
    }

    // package up frees
    for (const auto& profile : g_alloc_profile.per_thread_profiles) {
        for (const auto& free_info : profile.frees_by_type_address) {
            g_combined_results.combined_frees.push_back(FreeInfo{
                free_info.first,
                free_info.second
            });
        }
    }

    return RawAllocResults{
        g_combined_results.combined_allocs.data(),
        g_combined_results.combined_allocs.size(),
        g_combined_results.combined_frees.data(),
        g_combined_results.combined_frees.size()
    };
}

JL_DLLEXPORT void jl_free_alloc_profile() {
    for (auto profile : g_alloc_profile.per_thread_profiles) {
        for (auto alloc : profile.allocs) {
            free(alloc.backtrace.data);
        }
    }
    g_alloc_profile.per_thread_profiles.clear();

    g_combined_results.combined_allocs.clear();
    g_combined_results.combined_frees.clear();
}

}

// == callbacks called into by the outside ==

void _record_allocated_value(jl_value_t *val, size_t size) JL_NOTSAFEPOINT {
    auto& global_profile = g_alloc_profile;

    auto& profile = global_profile.per_thread_profiles[jl_threadid()];

    profile.alloc_counter++;
    auto diff = profile.alloc_counter - profile.last_recorded_alloc;
    if (diff < g_alloc_profile.skip_every) {
        return;
    }
    profile.last_recorded_alloc = profile.alloc_counter;

    auto type = (jl_datatype_t*)jl_typeof(val);

    profile.type_address_by_value_address[(size_t)val] = (size_t)type;

    profile.allocs.emplace_back(RawAlloc{
        type,
        get_raw_backtrace(),
        size
    });
}

void _record_freed_value(jl_taggedvalue_t *tagged_val) JL_NOTSAFEPOINT {
    jl_value_t *val = jl_valueof(tagged_val);

    auto& profile = g_alloc_profile.per_thread_profiles[jl_threadid()];

    auto value_address = (size_t)val;
    auto type_address = profile.type_address_by_value_address.find(value_address);
    if (type_address == profile.type_address_by_value_address.end()) {
        return; // TODO: warn
    }
    auto frees = profile.frees_by_type_address.find(type_address->second);

    if (frees == profile.frees_by_type_address.end()) {
        profile.frees_by_type_address[type_address->second] = 1;
    } else {
        profile.frees_by_type_address[type_address->second] = frees->second + 1;
    }
}

// TODO: remove these or make them toggle-able.

void _report_gc_started() JL_NOTSAFEPOINT {
    // ...
}

// TODO: figure out how to pass all of these in as a struct
void _report_gc_finished(
    uint64_t pause, uint64_t freed, uint64_t allocd, int full, int recollect
) JL_NOTSAFEPOINT {
    // TODO: figure out how to put in commas
    jl_safe_printf("GC: pause %fms. collected %fMB. %lld allocs total. %s %s\n",
        pause/1e6, freed/1e6, allocd,
        full ? "full" : "incr", recollect ? "recollect" : ""
    );
}
