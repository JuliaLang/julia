// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-alloc-profiler.h"
#include "julia_internal.h"

#include <string.h>

typedef struct {
    jl_bt_element_t *data;
    size_t size;
} jl_raw_backtrace_t;

typedef struct jl_raw_alloc_t {
    jl_datatype_t *type_address;
    jl_raw_backtrace_t backtrace;
    size_t size;
    void *task;
    uint64_t timestamp;
} jl_raw_alloc_t;

// Simple typed growable array for alloc records.
typedef struct {
    jl_raw_alloc_t *data;
    size_t len;
    size_t cap;
} alloc_array_t;

static void alloc_array_push(alloc_array_t *a, jl_raw_alloc_t val) JL_NOTSAFEPOINT
{
    if (a->len >= a->cap) {
        a->cap = a->cap ? a->cap * 2 : 8;
        a->data = (jl_raw_alloc_t *)realloc_s(a->data, a->cap * sizeof(jl_raw_alloc_t));
    }
    a->data[a->len++] = val;
}

static void alloc_array_clear(alloc_array_t *a) JL_NOTSAFEPOINT { a->len = 0; }

// Per-thread profile: a growable array of alloc records.
typedef struct {
    alloc_array_t allocs;
} jl_per_thread_alloc_profile_t;

// Global profile state.
typedef struct {
    double sample_rate;
    jl_per_thread_alloc_profile_t *per_thread_profiles;
    size_t num_profiles;
} jl_alloc_profile_t;

// == Global variables manipulated by callbacks ==

static jl_alloc_profile_t g_alloc_profile;
int g_alloc_profile_enabled = 0;
static alloc_array_t g_combined_allocs; // Will live forever.

// === stack stuff ===

static jl_raw_backtrace_t get_raw_backtrace(void) JL_NOTSAFEPOINT
{
    // We first record the backtrace onto a MAX-sized buffer, so that we don't have to
    // allocate the buffer until we know the size. To ensure thread-safety, we use a
    // per-thread backtrace buffer.
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_bt_element_t *shared_bt_data_buffer = ptls->profiling_bt_buffer;
    if (shared_bt_data_buffer == NULL) {
        size_t size = sizeof(jl_bt_element_t) * (JL_MAX_BT_SIZE + 1);
        shared_bt_data_buffer = (jl_bt_element_t *)malloc_s(size);
        ptls->profiling_bt_buffer = shared_bt_data_buffer;
    }

    size_t bt_size = rec_backtrace(shared_bt_data_buffer, JL_MAX_BT_SIZE, 2);

    // Then we copy only the needed bytes out of the buffer into our profile.
    size_t bt_bytes = bt_size * sizeof(jl_bt_element_t);
    jl_bt_element_t *bt_data = (jl_bt_element_t *)malloc_s(bt_bytes);
    memcpy(bt_data, shared_bt_data_buffer, bt_bytes);

    jl_raw_backtrace_t result;
    result.data = bt_data;
    result.size = bt_size;
    return result;
}

// == exported interface ==

JL_DLLEXPORT void jl_start_alloc_profile(double sample_rate)
{
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    size_t num_profiles = g_alloc_profile.num_profiles;
    if (num_profiles < nthreads) {
        jl_per_thread_alloc_profile_t *profiles = g_alloc_profile.per_thread_profiles;
        profiles = (jl_per_thread_alloc_profile_t *)realloc_s(
            profiles, nthreads * sizeof(profiles[0]));
        memset(&profiles[num_profiles],
            0, (nthreads - num_profiles) * sizeof(profiles[0]));
        g_alloc_profile.per_thread_profiles = profiles;
        g_alloc_profile.num_profiles = nthreads;
    }

    g_alloc_profile.sample_rate = sample_rate;
    g_alloc_profile_enabled = 1;
}

JL_DLLEXPORT jl_profile_allocs_raw_results_t jl_fetch_alloc_profile(void)
{
    // combine allocs
    // TODO: interleave to preserve ordering
    for (size_t i = 0; i < g_alloc_profile.num_profiles; i++) {
        alloc_array_t *allocs = &g_alloc_profile.per_thread_profiles[i].allocs;
        for (size_t j = 0; j < allocs->len; j++)
            alloc_array_push(&g_combined_allocs, allocs->data[j]);

        alloc_array_clear(allocs);
    }

    jl_profile_allocs_raw_results_t result;
    result.allocs = g_combined_allocs.data;
    result.num_allocs = g_combined_allocs.len;
    return result;
}

JL_DLLEXPORT void jl_stop_alloc_profile(void)
{
    g_alloc_profile_enabled = 0;
}

JL_DLLEXPORT void jl_free_alloc_profile(void)
{
    // Free any allocs that remain in the per-thread profiles, that haven't
    // been combined yet (which happens in fetch_alloc_profiles()).
    for (size_t i = 0; i < g_alloc_profile.num_profiles; i++) {
        alloc_array_t *allocs = &g_alloc_profile.per_thread_profiles[i].allocs;
        for (size_t j = 0; j < allocs->len; j++) {
            free(allocs->data[j].backtrace.data);
        }
        alloc_array_clear(allocs);
    }

    // Free the allocs that have been already combined into the combined results object.
    for (size_t i = 0; i < g_combined_allocs.len; i++) {
        free(g_combined_allocs.data[i].backtrace.data);
    }

    alloc_array_clear(&g_combined_allocs);
}

// == callback called into by the outside ==

void _maybe_record_alloc_to_profile(jl_value_t *val, size_t size, jl_datatype_t *type) JL_NOTSAFEPOINT
{
    size_t thread_id = jl_atomic_load_relaxed(&jl_current_task->tid);
    if (thread_id >= g_alloc_profile.num_profiles)
        return; // ignore allocations on threads started after the alloc-profile started

    alloc_array_t *allocs = &g_alloc_profile.per_thread_profiles[thread_id].allocs;

    jl_ptls_t ptls = jl_current_task->ptls;
    double sample_val = (double)cong(UINT64_MAX, &ptls->rngseed) / (double)UINT64_MAX;
    if (sample_val > g_alloc_profile.sample_rate)
        return;

    jl_raw_alloc_t alloc;
    alloc.type_address = type;
    alloc.backtrace = get_raw_backtrace();
    alloc.size = size;
    alloc.task = (void *)jl_current_task;
    alloc.timestamp = cycleclock();
    alloc_array_push(allocs, alloc);
}
