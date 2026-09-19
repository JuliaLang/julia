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

typedef struct {
    jl_datatype_t **data;
    size_t len;
    size_t cap;
} type_array_t;

static void type_array_push(type_array_t *a, jl_datatype_t *val) JL_NOTSAFEPOINT
{
    if (a->len >= a->cap) {
        a->cap = a->cap ? a->cap * 2 : 8;
        a->data = (jl_datatype_t **)realloc_s(a->data, a->cap * sizeof(a->data[0]));
    }
    a->data[a->len++] = val;
}

#define TYPE_FILTER_SIZE 256 // must be a power of two

// Bump arena for the recorded backtraces, to avoid a malloc per sample.
// Blocks are only freed all at once in `jl_free_alloc_profile`, since records
// combined into `g_combined_allocs` keep pointing into them.
typedef struct jl_bt_arena_block_t {
    struct jl_bt_arena_block_t *next;
    size_t used; // in elements
    size_t cap;  // in elements
    jl_bt_element_t data[];
} jl_bt_arena_block_t;

typedef struct {
    jl_bt_arena_block_t *head;
} jl_bt_arena_t;

#define BT_ARENA_BLOCK_ELEMENTS (size_t)(64 * 1024 / sizeof(jl_bt_element_t))

static jl_bt_element_t *bt_arena_alloc(jl_bt_arena_t *arena, size_t n) JL_NOTSAFEPOINT
{
    jl_bt_arena_block_t *block = arena->head;
    if (n >= BT_ARENA_BLOCK_ELEMENTS) {
        // oversized: give it a dedicated block, keeping the current block (and
        // its remaining space) as the head
        jl_bt_arena_block_t *big = (jl_bt_arena_block_t*)malloc_s(
            sizeof(jl_bt_arena_block_t) + n * sizeof(jl_bt_element_t));
        big->used = big->cap = n;
        if (block == NULL) {
            big->next = NULL;
            arena->head = big;
        }
        else {
            big->next = block->next;
            block->next = big;
        }
        return big->data;
    }
    if (block == NULL || block->cap - block->used < n) {
        block = (jl_bt_arena_block_t*)malloc_s(
            sizeof(jl_bt_arena_block_t) + BT_ARENA_BLOCK_ELEMENTS * sizeof(jl_bt_element_t));
        block->next = arena->head;
        block->used = 0;
        block->cap = BT_ARENA_BLOCK_ELEMENTS;
        arena->head = block;
    }
    jl_bt_element_t *p = &block->data[block->used];
    block->used += n;
    return p;
}

static void bt_arena_free(jl_bt_arena_t *arena) JL_NOTSAFEPOINT
{
    jl_bt_arena_block_t *block = arena->head;
    while (block != NULL) {
        jl_bt_arena_block_t *next = block->next;
        free(block);
        block = next;
    }
    arena->head = NULL;
}

// Per-thread profile: a growable array of alloc records, the arena that owns
// their backtrace memory, and the types they refer to, which have to be
// reported to the GC as roots.
typedef struct {
    alloc_array_t allocs;
    jl_bt_arena_t bt_arena;
    type_array_t type_roots;
    // direct-mapped filter over `type_roots`, so that it doesn't grow with every
    // sample. Owning thread only; a collision just lets a duplicate through.
    jl_datatype_t *type_filter[TYPE_FILTER_SIZE];
} jl_per_thread_alloc_profile_t;

// Global profile state.
typedef struct {
    // `cong(UINT64_MAX, ...)` draw at or below which an allocation is sampled. Read
    // by recorders that have not yet joined the recorder count, so it has to be
    // atomic even though it is only written while the profiler is stopped.
    _Atomic(uint64_t) sample_threshold;
    jl_per_thread_alloc_profile_t *per_thread_profiles;
    size_t num_profiles;
} jl_alloc_profile_t;

// == Global variables manipulated by callbacks ==

static jl_alloc_profile_t g_alloc_profile;
_Atomic(int) g_alloc_profile_enabled = 0;
// number of threads currently inside `_maybe_record_alloc_to_profile`, striped
// over cache-line-padded counters (indexed by tid) to avoid contention on a
// single cache line while recording
#define RECORDER_STRIPES 16
static struct {
    _Atomic(int) count;
    char _pad[64 - sizeof(_Atomic(int))];
} g_recording_threads[RECORDER_STRIPES];
static alloc_array_t g_combined_allocs; // Will live forever.

static _Atomic(int) *recorder_counter(size_t tid) JL_NOTSAFEPOINT
{
    return &g_recording_threads[tid % RECORDER_STRIPES].count;
}

// Must only be called while `g_alloc_profile_enabled` is 0, so that once a
// stripe reaches zero no new recorder on it can start touching the profile
// arrays (the same Dekker-style argument as for a single counter, per stripe).
static void wait_for_recorders(void) JL_NOTSAFEPOINT
{
    // a recorder can take a while (unwinding takes locks and mallocs), so back off
    for (int i = 0; i < RECORDER_STRIPES; i++) {
        for (int spins = 0; jl_atomic_load(&g_recording_threads[i].count) != 0; spins++) {
            if (spins < 128)
                jl_cpu_pause();
            else
                jl_cpu_suspend();
        }
    }
}

// Disable recording and wait for in-flight recorders to leave, so that the profile
// arrays can be read, reallocated or freed. Returns the state for `resume_recording`.
static int suspend_recording(void) JL_NOTSAFEPOINT
{
    int was_enabled = jl_atomic_exchange(&g_alloc_profile_enabled, 0);
    wait_for_recorders();
    return was_enabled;
}

static void resume_recording(int was_enabled) JL_NOTSAFEPOINT
{
    if (was_enabled)
        jl_atomic_store_release(&g_alloc_profile_enabled, 1);
}

// the recorded "type" is sometimes a sentinel rather than an object pointer;
// keep in sync with `load_type` in stdlib/Profile/src/Allocs.jl
static int is_object_type(jl_datatype_t *type) JL_NOTSAFEPOINT
{
    return !((uintptr_t)type < 4096 || (uintptr_t)type == jl_buff_tag ||
             type == jl_gc_unknown_type_tag);
}

// keep `type` alive for as long as an alloc record refers to it
static void record_type_root(jl_per_thread_alloc_profile_t *p, jl_datatype_t *type) JL_NOTSAFEPOINT
{
    if (!is_object_type(type))
        return;
    size_t slot = ((uintptr_t)type >> 4) & (TYPE_FILTER_SIZE - 1);
    if (p->type_filter[slot] == type)
        return;
    p->type_filter[slot] = type;
    type_array_push(&p->type_roots, type);
}

// === stack stuff ===

static jl_raw_backtrace_t get_raw_backtrace(jl_bt_arena_t *arena) JL_NOTSAFEPOINT
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
    jl_bt_element_t *bt_data = bt_arena_alloc(arena, bt_size);
    memcpy(bt_data, shared_bt_data_buffer, bt_size * sizeof(jl_bt_element_t));

    jl_raw_backtrace_t result;
    result.data = bt_data;
    result.size = bt_size;
    return result;
}

// == exported interface ==

JL_DLLEXPORT void jl_start_alloc_profile(double sample_rate)
{
    // in-flight recorders read the per-thread arrays we may be about to reallocate
    jl_stop_alloc_profile();

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

    // `cong` draws from the open interval [0, UINT64_MAX), so a threshold of
    // UINT64_MAX samples every allocation
    uint64_t threshold = sample_rate >= 1.0 ? UINT64_MAX :
                         sample_rate <= 0.0 ? 0 :
                         (uint64_t)(sample_rate * (double)UINT64_MAX);
    jl_atomic_store_relaxed(&g_alloc_profile.sample_threshold, threshold);
    jl_atomic_store_release(&g_alloc_profile_enabled, 1);
}

JL_DLLEXPORT jl_profile_allocs_raw_results_t jl_fetch_alloc_profile(void)
{
    // in-flight recorders may be pushing onto (and thus reallocating) the
    // per-thread arrays we are about to drain
    int was_enabled = suspend_recording();

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

    // recorders only ever push onto the per-thread arrays, never onto
    // `g_combined_allocs`, so the result stays valid while recording resumes
    resume_recording(was_enabled);
    return result;
}

JL_DLLEXPORT void jl_stop_alloc_profile(void)
{
    // after this returns, no thread is mid-record and none can start recording,
    // so the profile arrays are safe to read and free
    suspend_recording();
}

JL_DLLEXPORT int jl_alloc_profile_is_running(void)
{
    return jl_atomic_load_acquire(&g_alloc_profile_enabled);
}

JL_DLLEXPORT void jl_free_alloc_profile(void)
{
    // in-flight recorders may be pushing onto the arrays we are about to free
    int was_enabled = suspend_recording();

    // The per-thread arenas own all backtrace memory, including that of allocs
    // already combined into `g_combined_allocs` by jl_fetch_alloc_profile().
    for (size_t i = 0; i < g_alloc_profile.num_profiles; i++) {
        jl_per_thread_alloc_profile_t *p = &g_alloc_profile.per_thread_profiles[i];
        alloc_array_clear(&p->allocs);
        bt_arena_free(&p->bt_arena);
        // no alloc record refers to these types any more
        p->type_roots.len = 0;
        memset(p->type_filter, 0, sizeof(p->type_filter));
    }
    alloc_array_clear(&g_combined_allocs);
    resume_recording(was_enabled);
}

// == GC root marking ==

// Called during GC root marking (world stopped, so no recorder can be appending to
// `type_roots`); keeps the recorded types alive until `jl_free_alloc_profile`. Walks
// the distinct types seen, not every record, so the mark work is bounded.
void jl_gc_foreach_alloc_profile_root(jl_alloc_profile_root_cb_t f, void *env) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < g_alloc_profile.num_profiles; i++) {
        type_array_t *roots = &g_alloc_profile.per_thread_profiles[i].type_roots;
        for (size_t j = 0; j < roots->len; j++)
            f((jl_value_t*)roots->data[j], env);
    }
}

// == callback called into by the outside ==

void _maybe_record_alloc_to_profile(jl_value_t *val, size_t size, jl_datatype_t *type) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    // draw the sample before joining the recorder count: the RNG is thread-local, so
    // the common case of an allocation that isn't sampled costs no shared-memory
    // traffic beyond the relaxed threshold load. A concurrent restart can at worst
    // have this draw against the previous session's rate, which only perturbs the
    // sampling of the allocations in flight across the restart.
    if (cong(UINT64_MAX, &ct->ptls->rngseed) >
            jl_atomic_load_relaxed(&g_alloc_profile.sample_threshold))
        return;

    size_t thread_id = jl_atomic_load_relaxed(&ct->tid);
    _Atomic(int) *recording = recorder_counter(thread_id);
    jl_atomic_fetch_add(recording, 1);
    // re-check the flag under the recorder count: either we see the stop and
    // leave, or the stopping thread waits for us to finish (Dekker-style
    // synchronization; both sides use sequentially consistent operations)
    if (!jl_atomic_load(&g_alloc_profile_enabled))
        goto done;

    {
        if (thread_id >= g_alloc_profile.num_profiles)
            goto done; // ignore allocations on threads started after the alloc-profile started

        jl_per_thread_alloc_profile_t *p = &g_alloc_profile.per_thread_profiles[thread_id];

        jl_raw_alloc_t alloc;
        alloc.type_address = type;
        alloc.backtrace = get_raw_backtrace(&p->bt_arena);
        alloc.size = size;
        alloc.task = (void *)jl_current_task;
        alloc.timestamp = cycleclock();
        alloc_array_push(&p->allocs, alloc);
        record_type_root(p, type);
    }

done:
    jl_atomic_fetch_add(recording, -1);
}
