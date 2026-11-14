// This file is a part of Julia. License is MIT: https://julialang.org/license

// Sampling profiler for dynamic dispatch: records the dispatch target and a
// backtrace for a random sample of the calls that go through `jl_apply_generic`.

#include "julia.h"
#include "julia_internal.h"

#include <string.h>

// keep in sync with `RawDispatch` in stdlib/Profile/src/Dispatch.jl
typedef struct {
    jl_method_instance_t *mi; // the dispatch target; never freed, as its method holds it
    jl_bt_element_t *bt;
    size_t bt_size;
    void *task;
    uint8_t path; // JL_DISPATCH_PATH_*
} jl_raw_dispatch_t;

typedef struct {
    jl_raw_dispatch_t *data;
    size_t len;
    size_t cap;
} dispatch_array_t;

static void dispatch_array_push(dispatch_array_t *a, jl_raw_dispatch_t val) JL_NOTSAFEPOINT
{
    if (a->len >= a->cap) {
        a->cap = a->cap ? a->cap * 2 : 64;
        a->data = (jl_raw_dispatch_t*)realloc_s(a->data, a->cap * sizeof(jl_raw_dispatch_t));
    }
    a->data[a->len++] = val;
}

// Bump arena for the backtraces. Blocks are only freed in `jl_free_dispatch_profile`.
typedef struct bt_block_t {
    struct bt_block_t *next;
    size_t used; // in elements
    size_t cap;  // in elements
    jl_bt_element_t data[];
} bt_block_t;

#define BT_BLOCK_ELEMENTS (size_t)(64 * 1024 / sizeof(jl_bt_element_t))

typedef struct {
    dispatch_array_t dispatches;
    bt_block_t *bt_blocks;
} thread_profile_t;

static jl_bt_element_t *bt_alloc(thread_profile_t *p, size_t n) JL_NOTSAFEPOINT
{
    bt_block_t *block = p->bt_blocks;
    size_t cap = n > BT_BLOCK_ELEMENTS ? n : BT_BLOCK_ELEMENTS;
    if (block == NULL || n > BT_BLOCK_ELEMENTS || block->cap - block->used < n) {
        bt_block_t *fresh = (bt_block_t*)malloc_s(sizeof(bt_block_t) + cap * sizeof(jl_bt_element_t));
        fresh->used = 0;
        fresh->cap = cap;
        if (block != NULL && n > BT_BLOCK_ELEMENTS) {
            // oversized: a block of its own, behind the one still being filled
            fresh->next = block->next;
            block->next = fresh;
        }
        else {
            fresh->next = block;
            p->bt_blocks = fresh;
        }
        block = fresh;
    }
    jl_bt_element_t *bt = &block->data[block->used];
    block->used += n;
    return bt;
}

static struct {
    _Atomic(uint64_t) sample_threshold; // `cong` draw at or below which a call is sampled
    _Atomic(size_t) bt_max; // size limit of the recorded backtraces
    thread_profile_t *threads;
    size_t nthreads;
    dispatch_array_t combined;
} profile;

_Atomic(int) jl_dispatch_profile_enabled = 0;

// number of threads inside `jl_dispatch_profile_record`, striped by tid
#define RECORDER_STRIPES 16
static struct {
    _Atomic(int) count;
    char _pad[64 - sizeof(_Atomic(int))];
} recorders[RECORDER_STRIPES];

// Disable recording and wait for the recorders to leave, so that the profile
// can be read, reallocated or freed.
static int suspend_recording(void) JL_NOTSAFEPOINT
{
    int was_enabled = jl_atomic_exchange(&jl_dispatch_profile_enabled, 0);
    for (int i = 0; i < RECORDER_STRIPES; i++) {
        for (int spins = 0; jl_atomic_load(&recorders[i].count) != 0; spins++) {
            if (spins < 128)
                jl_cpu_pause();
            else
                jl_cpu_suspend();
        }
    }
    return was_enabled;
}

JL_DLLEXPORT void jl_start_dispatch_profile(double sample_rate, size_t max_depth)
{
    suspend_recording();
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    if (profile.nthreads < nthreads) {
        profile.threads = (thread_profile_t*)realloc_s(profile.threads, nthreads * sizeof(thread_profile_t));
        memset(&profile.threads[profile.nthreads], 0, (nthreads - profile.nthreads) * sizeof(thread_profile_t));
        profile.nthreads = nthreads;
    }
    uint64_t threshold = sample_rate >= 1.0 ? UINT64_MAX :
                         sample_rate <= 0.0 ? 0 :
                         (uint64_t)(sample_rate * (double)UINT64_MAX);
    jl_atomic_store_relaxed(&profile.sample_threshold, threshold);
    // the unwinder stops once there is no room left for a largest entry
    size_t bt_max = max_depth < JL_MAX_BT_SIZE ? max_depth + JL_BT_MAX_ENTRY_SIZE : JL_MAX_BT_SIZE;
    jl_atomic_store_relaxed(&profile.bt_max, bt_max < JL_MAX_BT_SIZE ? bt_max : JL_MAX_BT_SIZE);
    jl_atomic_store_release(&jl_dispatch_profile_enabled, 1);
}

JL_DLLEXPORT void jl_stop_dispatch_profile(void)
{
    suspend_recording();
}

typedef struct {
    jl_raw_dispatch_t *dispatches;
    size_t num_dispatches;
} jl_dispatch_profile_raw_results_t;

JL_DLLEXPORT jl_dispatch_profile_raw_results_t jl_fetch_dispatch_profile(void)
{
    int was_enabled = suspend_recording();
    for (size_t i = 0; i < profile.nthreads; i++) {
        dispatch_array_t *a = &profile.threads[i].dispatches;
        for (size_t j = 0; j < a->len; j++)
            dispatch_array_push(&profile.combined, a->data[j]);
        a->len = 0;
    }
    jl_dispatch_profile_raw_results_t result = {profile.combined.data, profile.combined.len};
    if (was_enabled)
        jl_atomic_store_release(&jl_dispatch_profile_enabled, 1);
    return result;
}

JL_DLLEXPORT void jl_free_dispatch_profile(void)
{
    int was_enabled = suspend_recording();
    for (size_t i = 0; i < profile.nthreads; i++) {
        thread_profile_t *p = &profile.threads[i];
        p->dispatches.len = 0;
        bt_block_t *block = p->bt_blocks;
        while (block != NULL) {
            bt_block_t *next = block->next;
            free(block);
            block = next;
        }
        p->bt_blocks = NULL;
    }
    profile.combined.len = 0;
    if (was_enabled)
        jl_atomic_store_release(&jl_dispatch_profile_enabled, 1);
}

NOINLINE void jl_dispatch_profile_record(jl_method_instance_t *mi) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    uint8_t path = ptls->dispatch_profile_path;
    ptls->dispatch_profile_path = JL_DISPATCH_PATH_CALLSITE;
    if (cong(UINT64_MAX, &ptls->rngseed) > jl_atomic_load_relaxed(&profile.sample_threshold))
        return;
    size_t tid = jl_atomic_load_relaxed(&ct->tid);
    _Atomic(int) *recording = &recorders[tid % RECORDER_STRIPES].count;
    jl_atomic_fetch_add(recording, 1);
    // re-check under the recorder count: either we see the stop and leave, or
    // the stopping thread waits for us
    if (jl_atomic_load(&jl_dispatch_profile_enabled) && tid < profile.nthreads) {
        thread_profile_t *p = &profile.threads[tid];
        jl_bt_element_t *buf = ptls->profiling_bt_buffer;
        if (buf == NULL) {
            buf = (jl_bt_element_t*)malloc_s(sizeof(jl_bt_element_t) * (JL_MAX_BT_SIZE + 1));
            ptls->profiling_bt_buffer = buf;
        }
        // the frames of the dispatch itself are dropped when decoding
        size_t bt_size = rec_backtrace(buf, jl_atomic_load_relaxed(&profile.bt_max), 1);
        jl_raw_dispatch_t d;
        d.mi = mi;
        d.bt = bt_alloc(p, bt_size);
        memcpy(d.bt, buf, bt_size * sizeof(jl_bt_element_t));
        d.bt_size = bt_size;
        d.task = (void*)ct;
        d.path = path;
        dispatch_array_push(&p->dispatches, d);
    }
    jl_atomic_fetch_add(recording, -1);
}
