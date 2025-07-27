#include "gc-common.h"
#include "gc-tls-mmtk.h"
#include "mmtkMutator.h"
#include "threading.h"

// File exists in the binding
#include "mmtk.h"

#ifdef __cplusplus
extern "C" {
#endif

// ========================================================================= //
// Julia specific
// ========================================================================= //

extern jl_value_t *cmpswap_names JL_GLOBALLY_ROOTED;
extern const unsigned pool_sizes[];
extern jl_mutex_t finalizers_lock;

// FIXME: Should the values below be shared between both GC's?
// Note that MMTk uses a hard max heap limit, which is set by default
// as 70% of the free available memory. The min heap is set as the
// default_collect_interval variable below.

// max_total_memory is a suggestion.  We try very hard to stay
// under this limit, but we will go above it rather than halting.
#ifdef _P64
typedef uint64_t memsize_t;
static const size_t default_collect_interval = 5600 * 1024 * sizeof(void*);
// We expose this to the user/ci as jl_gc_set_max_memory
static memsize_t max_total_memory = (memsize_t) 2 * 1024 * 1024 * 1024 * 1024 * 1024;
#else
typedef uint32_t memsize_t;
static const size_t default_collect_interval = 3200 * 1024 * sizeof(void*);
// Work really hard to stay within 2GB
// Alternative is to risk running out of address space
// on 32 bit architectures.
#define MAX32HEAP 1536 * 1024 * 1024
static memsize_t max_total_memory = (memsize_t) MAX32HEAP;
#endif

// ========================================================================= //
// Defined by the binding
// ========================================================================= //

extern void mmtk_julia_copy_stack_check(int copy_stack);
extern void mmtk_gc_init(uintptr_t min_heap_size, uintptr_t max_heap_size, uintptr_t n_gcthreads, uintptr_t header_size, uintptr_t tag);
extern void mmtk_object_reference_write_post(void* mutator, const void* parent, const void* ptr);
extern void mmtk_object_reference_write_slow(void* mutator, const void* parent, const void* ptr);
extern void* mmtk_alloc(void* mutator, size_t size, size_t align, size_t offset, int allocator);
extern void mmtk_post_alloc(void* mutator, void* refer, size_t bytes, int allocator);
extern void mmtk_store_obj_size_c(void* obj, size_t size);
extern const void* MMTK_SIDE_LOG_BIT_BASE_ADDRESS;
extern const void* MMTK_SIDE_VO_BIT_BASE_ADDRESS;

// ========================================================================= //
// GC Initialization and Control
// ========================================================================= //

void jl_gc_init(void) {
    // TODO: use jl_options.heap_size_hint to set MMTk's fixed heap size? (see issue: https://github.com/mmtk/mmtk-julia/issues/167)
    JL_MUTEX_INIT(&finalizers_lock, "finalizers_lock");

    arraylist_new(&to_finalize, 0);
    arraylist_new(&finalizer_list_marked, 0);
    gc_num.interval = default_collect_interval;
    gc_num.allocd = 0;
    gc_num.max_pause = 0;
    gc_num.max_memory = 0;

    // Necessary if we want to use Julia heap resizing heuristics
    uint64_t mem_reserve = 250*1024*1024; // LLVM + other libraries need some amount of memory
    uint64_t min_heap_size_hint = mem_reserve + 1*1024*1024;
    uint64_t hint = jl_options.heap_size_hint;

    // check if heap size specified on command line
    if (jl_options.heap_size_hint == 0) {
        char *cp = getenv(HEAP_SIZE_HINT);
        if (cp)
            hint = parse_heap_size_option(cp, "JULIA_HEAP_SIZE_HINT=\"<size>[<unit>]\"", 1);
    }
#ifdef _P64
    if (hint == 0) {
        uint64_t constrained_mem = uv_get_constrained_memory();
        if (constrained_mem > 0 && constrained_mem < uv_get_total_memory())
            hint = constrained_mem;
    }
#endif
    if (hint) {
        if (hint < min_heap_size_hint)
            hint = min_heap_size_hint;
        jl_gc_set_max_memory(hint - mem_reserve);
    }

    // MMTK supports setting the heap size using the
    // MMTK_MIN_HSIZE and MMTK_MAX_HSIZE environment variables
    long long min_heap_size;
    long long max_heap_size;
    char* min_size_def = getenv("MMTK_MIN_HSIZE");
    char* min_size_gb = getenv("MMTK_MIN_HSIZE_G");

    char* max_size_def = getenv("MMTK_MAX_HSIZE");
    char* max_size_gb = getenv("MMTK_MAX_HSIZE_G");

    // If min and max values are not specified, set them to 0 here
    // and use stock heuristics as defined in the binding
    if (min_size_def != NULL) {
        char *p;
        double min_size = strtod(min_size_def, &p);
        min_heap_size = (long) 1024 * 1024 * min_size;
    } else if (min_size_gb != NULL) {
        char *p;
        double min_size = strtod(min_size_gb, &p);
        min_heap_size = (long) 1024 * 1024 * 1024 * min_size;
    } else {
        min_heap_size = 0;
    }

    if (max_size_def != NULL) {
        char *p;
        double max_size = strtod(max_size_def, &p);
        max_heap_size = (long) 1024 * 1024 * max_size;
    } else if (max_size_gb != NULL) {
        char *p;
        double max_size = strtod(max_size_gb, &p);
        max_heap_size = (long) 1024 * 1024 * 1024 * max_size;
    } else {
        max_heap_size = 0;
    }

    // Assert that the number of stock GC threads is 0; MMTK uses the number of threads in jl_options.ngcthreads
    assert(jl_n_gcthreads == 0);

    // Check that the julia_copy_stack rust feature has been defined when the COPY_STACK has been defined
    int copy_stacks;

#ifdef COPY_STACKS
    copy_stacks = 1;
#else
    copy_stacks = 0;
#endif

    mmtk_julia_copy_stack_check(copy_stacks);

    // if only max size is specified initialize MMTk with a fixed size heap
    // TODO: We just assume mark threads means GC threads, and ignore the number of concurrent sweep threads.
    // If the two values are the same, we can use either. Otherwise, we need to be careful.
    uintptr_t gcthreads = jl_options.nmarkthreads;
    if (max_size_def != NULL || (max_size_gb != NULL && (min_size_def == NULL && min_size_gb == NULL))) {
        mmtk_gc_init(0, max_heap_size, gcthreads, (sizeof(jl_taggedvalue_t)), jl_buff_tag);
    } else {
        mmtk_gc_init(min_heap_size, max_heap_size, gcthreads, (sizeof(jl_taggedvalue_t)), jl_buff_tag);
    }
}

void jl_start_gc_threads(void) {
    jl_ptls_t ptls = jl_current_task->ptls;
    mmtk_initialize_collection((void *)ptls);
}

void jl_init_thread_heap(struct _jl_tls_states_t *ptls) JL_NOTSAFEPOINT {
    jl_thread_heap_common_t *heap = &ptls->gc_tls_common.heap;
    small_arraylist_new(&heap->weak_refs, 0);
    small_arraylist_new(&heap->live_tasks, 0);
    for (int i = 0; i < JL_N_STACK_POOLS; i++)
        small_arraylist_new(&heap->free_stacks[i], 0);
    small_arraylist_new(&heap->mallocarrays, 0);
    arraylist_new(&ptls->finalizers, 0);
    // Initialize `lazily_freed_mtarraylist_buffers`
    small_arraylist_new(&ptls->lazily_freed_mtarraylist_buffers, 0);
    // Clear the malloc sz count
    jl_atomic_store_relaxed(&ptls->gc_tls.malloc_sz_since_last_poll, 0);
    // Create mutator
    MMTk_Mutator mmtk_mutator = mmtk_bind_mutator((void *)ptls, ptls->tid);
    // Copy the mutator to the thread local storage
    memcpy(&ptls->gc_tls.mmtk_mutator, mmtk_mutator, sizeof(MMTkMutatorContext));
    // Call post_bind to maintain a list of active mutators and to reclaim the old mutator (which is no longer needed)
    mmtk_post_bind_mutator(&ptls->gc_tls.mmtk_mutator, mmtk_mutator);
    memset(&ptls->gc_tls_common.gc_num, 0, sizeof(ptls->gc_tls_common.gc_num));
}

void jl_free_thread_gc_state(struct _jl_tls_states_t *ptls) {
    mmtk_destroy_mutator(&ptls->gc_tls.mmtk_mutator);
}

JL_DLLEXPORT void jl_gc_set_max_memory(uint64_t max_mem) {
#ifdef _P32
    max_mem = max_mem < MAX32HEAP ? max_mem : MAX32HEAP;
#endif
    max_total_memory = max_mem;
}

JL_DLLEXPORT uint64_t jl_gc_get_max_memory(void)
{
    // FIXME: We should return the max heap size set in MMTk
    // when not using Julia's heap resizing heuristics
    return max_total_memory;
}

STATIC_INLINE void maybe_collect(jl_ptls_t ptls)
{
    // Just do a safe point for general maybe_collect
    jl_gc_safepoint_(ptls);
}

// This is only used for malloc. We need to know if we need to do GC. However, keeping checking with MMTk (mmtk_gc_poll),
// is expensive. So we only check for every few allocations.
static inline void malloc_maybe_collect(jl_ptls_t ptls, size_t sz)
{
    // We do not need to carefully maintain malloc_sz_since_last_poll. We just need to
    // avoid using mmtk_gc_poll too frequently, and try to be precise on our heap usage
    // as much as we can.
    if (ptls->gc_tls.malloc_sz_since_last_poll > 4096) {
        jl_atomic_store_relaxed(&ptls->gc_tls.malloc_sz_since_last_poll, 0);
        mmtk_gc_poll(ptls);
    } else {
        size_t curr = jl_atomic_load_relaxed(&ptls->gc_tls.malloc_sz_since_last_poll);
        jl_atomic_store_relaxed(&ptls->gc_tls.malloc_sz_since_last_poll, curr + sz);
        jl_gc_safepoint_(ptls);
    }
}

// This is called when the user calls for a GC with Gc.gc()
JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t collection) {
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (jl_atomic_load_acquire(&jl_gc_disable_counter)) {
        size_t localbytes = jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.allocd) + gc_num.interval;
        jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.allocd, -(int64_t)gc_num.interval);
        static_assert(sizeof(_Atomic(uint64_t)) == sizeof(gc_num.deferred_alloc), "");
        jl_atomic_fetch_add_relaxed((_Atomic(uint64_t)*)&gc_num.deferred_alloc, localbytes);
        return;
    }
    mmtk_handle_user_collection_request(ptls, collection);
}


// Based on jl_gc_collect from gc-stock.c
// called when stopping the thread in `mmtk_block_for_gc`
JL_DLLEXPORT void jl_gc_prepare_to_collect(void)
{
    // FIXME: set to JL_GC_AUTO since we're calling it from mmtk
    // maybe just remove this?
    JL_PROBE_GC_BEGIN(JL_GC_AUTO);

    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (jl_atomic_load_acquire(&jl_gc_disable_counter)) {
        size_t localbytes = jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.allocd) + gc_num.interval;
        jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.allocd, -(int64_t)gc_num.interval);
        static_assert(sizeof(_Atomic(uint64_t)) == sizeof(gc_num.deferred_alloc), "");
        jl_atomic_fetch_add_relaxed((_Atomic(uint64_t)*)&gc_num.deferred_alloc, localbytes);
        return;
    }

    int8_t old_state = jl_atomic_load_relaxed(&ptls->gc_state);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    // `jl_safepoint_start_gc()` makes sure only one thread can run the GC.
    uint64_t t0 = jl_hrtime();
    if (!jl_safepoint_start_gc(ct)) {
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        jl_safepoint_wait_thread_resume(ct); // block in thread-suspend now if requested, after clearing the gc_state
        return;
    }

    JL_TIMING_SUSPEND_TASK(GC, ct);
    JL_TIMING(GC, GC);

    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    // Now we are ready to wait for other threads to hit the safepoint,
    // we can do a few things that doesn't require synchronization.
    //
    // We must sync here with the tls_lock operations, so that we have a
    // seq-cst order between these events now we know that either the new
    // thread must run into our safepoint flag or we must observe the
    // existence of the thread in the jl_n_threads count.
    //
    // TODO: concurrently queue objects
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);
    JL_PROBE_GC_STOP_THE_WORLD();

    uint64_t t1 = jl_hrtime();
    uint64_t duration = t1 - t0;
    if (duration > gc_num.max_time_to_safepoint)
        gc_num.max_time_to_safepoint = duration;
    gc_num.time_to_safepoint = duration;
    gc_num.total_time_to_safepoint += duration;

    if (!jl_atomic_load_acquire(&jl_gc_disable_counter)) {
        JL_LOCK_NOGC(&finalizers_lock); // all the other threads are stopped, so this does not make sense, right? otherwise, failing that, this seems like plausibly a deadlock
#ifndef __clang_gcanalyzer__
        mmtk_block_thread_for_gc();
#endif
        JL_UNLOCK_NOGC(&finalizers_lock);
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    JL_PROBE_GC_END();
    jl_safepoint_wait_thread_resume(ct); // block in thread-suspend now if requested, after clearing the gc_state

    // Only disable finalizers on current thread
    // Doing this on all threads is racy (it's impossible to check
    // or wait for finalizers on other threads without dead lock).
    if (!ptls->finalizers_inhibited && ptls->locks.len == 0) {
        JL_TIMING(GC, GC_Finalizers);
        run_finalizers(ct, 0);
    }
    JL_PROBE_GC_FINALIZER();

#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
}

// ========================================================================= //
// GC Statistics
// ========================================================================= //

JL_DLLEXPORT const char* jl_gc_active_impl(void) {
    const char* mmtk_version = get_mmtk_version();
    return mmtk_version;
}

int64_t last_gc_total_bytes = 0;
int64_t last_live_bytes = 0; // live_bytes at last collection
int64_t live_bytes = 0;

// FIXME: The functions combine_thread_gc_counts and reset_thread_gc_counts
// are currently nearly identical for mmtk and for stock. However, the stats
// are likely different (e.g., MMTk doesn't track the bytes allocated in the fastpath,
// but only when the slowpath is called). We might need to adapt these later so that
// the statistics are the same or as close as possible for each GC.
static void combine_thread_gc_counts(jl_gc_num_t *dest, int update_heap) JL_NOTSAFEPOINT
{
    int gc_n_threads;
    jl_ptls_t* gc_all_tls_states;
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls = gc_all_tls_states[i];
        if (ptls) {
            dest->allocd += (jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.allocd) + gc_num.interval);
            dest->malloc += jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.malloc);
            dest->realloc += jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.realloc);
            dest->poolalloc += jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.poolalloc);
            dest->bigalloc += jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.bigalloc);
            dest->freed += jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.free_acc);
            if (update_heap) {
                jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.alloc_acc, 0);
                jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.free_acc, 0);
            }
        }
    }
}

void reset_thread_gc_counts(void) JL_NOTSAFEPOINT
{
    int gc_n_threads;
    jl_ptls_t* gc_all_tls_states;
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls = gc_all_tls_states[i];
        if (ptls != NULL) {
            // don't reset `pool_live_bytes` here
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.allocd, -(int64_t)gc_num.interval);
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.malloc, 0);
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.realloc, 0);
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.poolalloc, 0);
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.bigalloc, 0);
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.alloc_acc, 0);
            jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.free_acc, 0);
        }
    }
}

// Retrieves Julia's `GC_Num` (structure that stores GC statistics).
JL_DLLEXPORT jl_gc_num_t jl_gc_num(void) {
    jl_gc_num_t num = gc_num;
    combine_thread_gc_counts(&num, 0);
    return num;
}

JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void) JL_NOTSAFEPOINT {
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb;
    jl_gc_get_total_bytes(&newtb);
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}

JL_DLLEXPORT int64_t jl_gc_sync_total_bytes(int64_t offset) JL_NOTSAFEPOINT
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb;
    jl_gc_get_total_bytes(&newtb);
    last_gc_total_bytes = newtb - offset;
    return newtb - oldtb;
}

JL_DLLEXPORT int64_t jl_gc_pool_live_bytes(void) {
    return 0;
}

void jl_gc_count_allocd(size_t sz) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.allocd) + sz);
}

void jl_gc_count_freed(size_t sz) JL_NOTSAFEPOINT
{
}

int64_t inc_live_bytes(int64_t inc) JL_NOTSAFEPOINT
{
    jl_timing_counter_inc(JL_TIMING_COUNTER_HeapSize, inc);
    return live_bytes += inc;
}

void jl_gc_reset_alloc_count(void) JL_NOTSAFEPOINT
{
    combine_thread_gc_counts(&gc_num, 0);
    inc_live_bytes(gc_num.deferred_alloc + gc_num.allocd);
    gc_num.allocd = 0;
    gc_num.deferred_alloc = 0;
    reset_thread_gc_counts();
}

JL_DLLEXPORT int64_t jl_gc_live_bytes(void) {
    return last_live_bytes;
}

JL_DLLEXPORT void jl_gc_get_total_bytes(int64_t *bytes) JL_NOTSAFEPOINT
{
    jl_gc_num_t num = gc_num;
    combine_thread_gc_counts(&num, 0);
    // Sync this logic with `base/util.jl:GC_Diff`
    *bytes = (num.total_allocd + num.deferred_alloc + num.allocd);
}

// These are needed to collect MMTk statistics from a Julia program using ccall
JL_DLLEXPORT void (jl_mmtk_harness_begin)(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    mmtk_harness_begin(ptls);
}

JL_DLLEXPORT void (jl_mmtk_harness_end)(void)
{
    mmtk_harness_end();
}

// ========================================================================= //
// Root Processing, Object Scanning and Julia-specific sweeping
// ========================================================================= //

static void add_node_to_roots_buffer(RootsWorkClosure* closure, RootsWorkBuffer* buf, size_t* buf_len, void* root) {
    if (root == NULL)
        return;

    buf->ptr[*buf_len] = root;
    *buf_len += 1;
    if (*buf_len >= buf->cap) {
        RootsWorkBuffer new_buf = (closure->report_nodes_func)(buf->ptr, *buf_len, buf->cap, closure->data, true);
        *buf = new_buf;
        *buf_len = 0;
    }
}

static void add_node_to_tpinned_roots_buffer(RootsWorkClosure* closure, RootsWorkBuffer* buf, size_t* buf_len, void* root) {
    if (root == NULL)
        return;

    buf->ptr[*buf_len] = root;
    *buf_len += 1;
    if (*buf_len >= buf->cap) {
        RootsWorkBuffer new_buf = (closure->report_tpinned_nodes_func)(buf->ptr, *buf_len, buf->cap, closure->data, true);
        *buf = new_buf;
        *buf_len = 0;
    }
}

JL_DLLEXPORT void jl_gc_scan_vm_specific_roots(RootsWorkClosure* closure)
{
    // Create a new buf
    RootsWorkBuffer buf = (closure->report_nodes_func)((void**)0, 0, 0, closure->data, true);
    size_t len = 0;

    // add module
    add_node_to_roots_buffer(closure, &buf, &len, jl_main_module);

    // buildin values
    add_node_to_roots_buffer(closure, &buf, &len, jl_an_empty_vec_any);
    add_node_to_roots_buffer(closure, &buf, &len, jl_module_init_order);
    for (size_t i = 0; i < jl_current_modules.size; i += 2) {
        if (jl_current_modules.table[i + 1] != HT_NOTFOUND) {
            add_node_to_roots_buffer(closure, &buf, &len, jl_current_modules.table[i]);
        }
    }
    add_node_to_roots_buffer(closure, &buf, &len, jl_anytuple_type_type);
    for (size_t i = 0; i < N_CALL_CACHE; i++) {
         jl_typemap_entry_t *v = jl_atomic_load_relaxed(&call_cache[i]);
        add_node_to_roots_buffer(closure, &buf, &len, v);
    }
    add_node_to_roots_buffer(closure, &buf, &len, _jl_debug_method_invalidation);

    // constants
    add_node_to_roots_buffer(closure, &buf, &len, jl_emptytuple_type);
    add_node_to_roots_buffer(closure, &buf, &len, cmpswap_names);

    // jl_global_roots_table must be transitively pinned
    RootsWorkBuffer tpinned_buf = (closure->report_tpinned_nodes_func)((void**)0, 0, 0, closure->data, true);
    size_t tpinned_len = 0;
    add_node_to_tpinned_roots_buffer(closure, &tpinned_buf, &tpinned_len, jl_global_roots_list);
    add_node_to_tpinned_roots_buffer(closure, &tpinned_buf, &tpinned_len, jl_global_roots_keyset);

    // FIXME: transivitely pinning for now, should be removed after we add moving Immix
    add_node_to_tpinned_roots_buffer(closure, &tpinned_buf, &tpinned_len, precompile_field_replace);

    // Push the result of the work.
    (closure->report_nodes_func)(buf.ptr, len, buf.cap, closure->data, false);
    (closure->report_tpinned_nodes_func)(tpinned_buf.ptr, tpinned_len, tpinned_buf.cap, closure->data, false);
}

JL_DLLEXPORT void jl_gc_scan_julia_exc_obj(void* obj_raw, void* closure, ProcessSlotFn process_slot) {
    jl_task_t *ta = (jl_task_t*)obj_raw;

    if (ta->excstack) { // inlining label `excstack` from mark_loop

        // the excstack should always be a heap object
        assert(mmtk_object_is_managed_by_mmtk(ta->excstack));

        process_slot(closure, &ta->excstack);
        jl_excstack_t *excstack = ta->excstack;
        size_t itr = ta->excstack->top;
        size_t bt_index = 0;
        size_t jlval_index = 0;
        while (itr > 0) {
            size_t bt_size = jl_excstack_bt_size(excstack, itr);
            jl_bt_element_t *bt_data = jl_excstack_bt_data(excstack, itr);
            for (; bt_index < bt_size; bt_index += jl_bt_entry_size(bt_data + bt_index)) {
                jl_bt_element_t *bt_entry = bt_data + bt_index;
                if (jl_bt_is_native(bt_entry))
                    continue;
                // Found an extended backtrace entry: iterate over any
                // GC-managed values inside.
                size_t njlvals = jl_bt_num_jlvals(bt_entry);
                while (jlval_index < njlvals) {
                    jl_value_t** new_obj_slot = &bt_entry[2 + jlval_index].jlvalue;
                    jlval_index += 1;
                    process_slot(closure, new_obj_slot);
                }
                jlval_index = 0;
            }

            jl_bt_element_t *stack_raw = (jl_bt_element_t *)(excstack+1);
            jl_value_t** stack_obj_slot = &stack_raw[itr-1].jlvalue;

            itr = jl_excstack_next(excstack, itr);
            bt_index = 0;
            jlval_index = 0;
            process_slot(closure, stack_obj_slot);
        }
    }
}

// This is used in mmtk_sweep_malloced_memory and it is slightly different
// from jl_gc_free_memory from gc-stock.c as the stock GC updates the
// information in the global variable gc_heap_stats (which is specific to the stock GC)
static void jl_gc_free_memory(jl_genericmemory_t *m, int isaligned) JL_NOTSAFEPOINT
{
    assert(jl_is_genericmemory(m));
    assert(jl_genericmemory_how(m) == 1 || jl_genericmemory_how(m) == 2);
    char *d = (char*)m->ptr;
    size_t freed_bytes = memory_block_usable_size(d, isaligned);
    assert(freed_bytes != 0);
    if (isaligned)
        jl_free_aligned(d);
    else
        free(d);
    gc_num.freed += freed_bytes;
    gc_num.freecall++;
}

JL_DLLEXPORT void jl_gc_mmtk_sweep_malloced_memory(void) JL_NOTSAFEPOINT
{
    void* iter = mmtk_new_mutator_iterator();
    jl_ptls_t ptls2 = (jl_ptls_t)mmtk_get_next_mutator_tls(iter);
    while(ptls2 != NULL) {
        size_t n = 0;
        size_t l = ptls2->gc_tls_common.heap.mallocarrays.len;
        void **lst = ptls2->gc_tls_common.heap.mallocarrays.items;
        // filter without preserving order
        while (n < l) {
            jl_genericmemory_t *m = (jl_genericmemory_t*)((uintptr_t)lst[n] & ~1);
            if (mmtk_is_live_object(m)) {
                n++;
            }
            else {
                int isaligned = (uintptr_t)lst[n] & 1;
                jl_gc_free_memory(m, isaligned);
                l--;
                lst[n] = lst[l];
            }
        }
        ptls2->gc_tls_common.heap.mallocarrays.len = l;
        ptls2 = (jl_ptls_t)mmtk_get_next_mutator_tls(iter);
    }
    mmtk_close_mutator_iterator(iter);
}

#define jl_genericmemory_elsize(a) (((jl_datatype_t*)jl_typetagof(a))->layout->size)

// if data is inlined inside the genericmemory object --- to->ptr needs to be updated when copying the array
JL_DLLEXPORT void jl_gc_update_inlined_array(void* from, void* to) {
    jl_value_t* jl_from = (jl_value_t*) from;
    jl_value_t* jl_to = (jl_value_t*) to;

    uintptr_t tag_to = (uintptr_t)jl_typeof(jl_to);
    jl_datatype_t *vt = (jl_datatype_t*)tag_to;

    if(vt->name == jl_genericmemory_typename) {
        jl_genericmemory_t *a = (jl_genericmemory_t*)jl_from;
        jl_genericmemory_t *b = (jl_genericmemory_t*)jl_to;
        int how = jl_genericmemory_how(a);

        if (how == 0 && mmtk_object_is_managed_by_mmtk(a->ptr)) { // a is inlined (a->ptr points into the mmtk object)
            size_t offset_of_data = ((size_t)a->ptr - (size_t)a);
            if (offset_of_data > 0) {
                b->ptr = (void*)((size_t) b + offset_of_data);
            }
        }
    }
}

// modified sweep_stack_pools from gc-stacks.c
JL_DLLEXPORT void jl_gc_mmtk_sweep_stack_pools(void)
{
    // Stack sweeping algorithm:
    //    // deallocate stacks if we have too many sitting around unused
    //    for (stk in halfof(free_stacks))
    //        free_stack(stk, pool_sz);
    //    // then sweep the task stacks
    //    for (t in live_tasks)
    //        if (!gc-marked(t))
    //            stkbuf = t->stkbuf
    //            bufsz = t->bufsz
    //            if (stkbuf)
    //                push(free_stacks[sz], stkbuf)
    assert(gc_n_threads);
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 == NULL)
            continue;

        // free half of stacks that remain unused since last sweep
        for (int p = 0; p < JL_N_STACK_POOLS; p++) {
            small_arraylist_t *al = &ptls2->gc_tls_common.heap.free_stacks[p];
            size_t n_to_free;
            if (jl_atomic_load_relaxed(&ptls2->current_task) == NULL) {
                n_to_free = al->len; // not alive yet or dead, so it does not need these anymore
            }
            else if (al->len > MIN_STACK_MAPPINGS_PER_POOL) {
                n_to_free = al->len / 2;
                if (n_to_free > (al->len - MIN_STACK_MAPPINGS_PER_POOL))
                    n_to_free = al->len - MIN_STACK_MAPPINGS_PER_POOL;
            }
            else {
                n_to_free = 0;
            }
            for (int n = 0; n < n_to_free; n++) {
                void *stk = small_arraylist_pop(al);
                free_stack(stk, pool_sizes[p]);
            }
            if (jl_atomic_load_relaxed(&ptls2->current_task) == NULL) {
                small_arraylist_free(al);
            }
        }
        if (jl_atomic_load_relaxed(&ptls2->current_task) == NULL) {
            small_arraylist_free(ptls2->gc_tls_common.heap.free_stacks);
        }

        small_arraylist_t *live_tasks = &ptls2->gc_tls_common.heap.live_tasks;
        size_t n = 0;
        size_t ndel = 0;
        size_t l = live_tasks->len;
        void **lst = live_tasks->items;
        if (l == 0)
            continue;
        while (1) {
            jl_task_t *t = (jl_task_t*)lst[n];
            if (mmtk_is_live_object(t)) {
                jl_task_t *maybe_forwarded = (jl_task_t*)mmtk_get_possibly_forwarded(t);
                live_tasks->items[n] = maybe_forwarded;
                t = maybe_forwarded;
                assert(jl_is_task(t));
                if (t->ctx.stkbuf == NULL)
                    ndel++; // jl_release_task_stack called
                else
                    n++;
            } else {
                ndel++;
                void *stkbuf = t->ctx.stkbuf;
                size_t bufsz = t->ctx.bufsz;
                if (stkbuf) {
                    t->ctx.stkbuf = NULL;
                    _jl_free_stack(ptls2, stkbuf, bufsz);
                }
#ifdef _COMPILER_TSAN_ENABLED_
                if (t->ctx.tsan_state) {
                    __tsan_destroy_fiber(t->ctx.tsan_state);
                    t->ctx.tsan_state = NULL;
                }
#endif
            }
            if (n >= l - ndel)
                break;
            void *tmp = lst[n];
            lst[n] = lst[n + ndel];
            lst[n + ndel] = tmp;
        }
        live_tasks->len -= ndel;
    }
}

JL_DLLEXPORT void jl_gc_sweep_stack_pools_and_mtarraylist_buffers(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    jl_gc_mmtk_sweep_stack_pools();
    sweep_mtarraylist_buffers();
}

JL_DLLEXPORT void* jl_gc_get_stackbase(int16_t tid) {
    assert(tid >= 0);
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    return ptls2->stackbase;
}

JL_DLLEXPORT void jl_gc_update_stats(uint64_t inc, size_t mmtk_live_bytes, bool is_nursery_gc) {
    gc_num.total_time += inc;
    gc_num.pause += 1;
    gc_num.full_sweep += !(is_nursery_gc);
    gc_num.total_allocd += gc_num.allocd;
    gc_num.allocd = 0;
    live_bytes = mmtk_live_bytes;
}

#define jl_genericmemory_data_owner_field_addr(a) ((jl_value_t**)((jl_genericmemory_t*)(a) + 1))

JL_DLLEXPORT void* jl_gc_get_owner_address_to_mmtk(void* m) {
    return (void*)jl_genericmemory_data_owner_field_addr(m);
}

// same as jl_genericmemory_how but with JL_DLLEXPORT
// we should probably inline this in Rust
JL_DLLEXPORT size_t jl_gc_genericmemory_how(void *arg) JL_NOTSAFEPOINT
{
    jl_genericmemory_t* m = (jl_genericmemory_t*)arg;
    if (m->ptr == (void*)((char*)m + 16)) // JL_SMALL_BYTE_ALIGNMENT (from julia_internal.h)
        return 0;
    jl_value_t *owner = jl_genericmemory_data_owner_field(m);
    if (owner == (jl_value_t*)m)
        return 1;
    if (owner == NULL)
        return 2;
    return 3;
}

// ========================================================================= //
// Weak References and Finalizers
// ========================================================================= //

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref_th(jl_ptls_t ptls, jl_value_t *value)
{
    jl_weakref_t *wr = (jl_weakref_t*)jl_gc_alloc(ptls, sizeof(void*), jl_weakref_type);
    wr->value = value;  // NOTE: wb not needed here
    mmtk_add_weak_candidate(wr);
    return wr;
}

JL_DLLEXPORT void* jl_gc_get_thread_finalizer_list(void* ptls_raw) {
    jl_ptls_t ptls = (jl_ptls_t) ptls_raw;
    return (void*)&ptls->finalizers;
}

JL_DLLEXPORT void* jl_gc_get_to_finalize_list(void) {
    return (void*)&to_finalize;
}

JL_DLLEXPORT void* jl_gc_get_marked_finalizers_list(void) {
    return (void*)&finalizer_list_marked;
}

JL_DLLEXPORT int* jl_gc_get_have_pending_finalizers(void) {
    return (int*)&jl_gc_have_pending_finalizers;
}

// ========================================================================= //
// Allocation
// ========================================================================= //

#define MMTK_DEFAULT_IMMIX_ALLOCATOR (0)
#define MMTK_IMMORTAL_BUMP_ALLOCATOR (0)

int jl_gc_classify_pools(size_t sz, int *osize)
{
    if (sz > GC_MAX_SZCLASS)
        return -1; // call big alloc function
    size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    *osize = LLT_ALIGN(allocsz, 16);
    return 0; // use MMTk's fastpath logic
}

#define MMTK_MIN_ALIGNMENT 4
// MMTk assumes allocation size is aligned to min alignment.
STATIC_INLINE size_t mmtk_align_alloc_sz(size_t sz) JL_NOTSAFEPOINT
{
    return (sz + MMTK_MIN_ALIGNMENT - 1) & ~(MMTK_MIN_ALIGNMENT - 1);
}

STATIC_INLINE void* bump_alloc_fast(MMTkMutatorContext* mutator, uintptr_t* cursor, uintptr_t limit, size_t size, size_t align, size_t offset, int allocator) {
    intptr_t delta = (-offset - *cursor) & (align - 1);
    uintptr_t result = *cursor + (uintptr_t)delta;

    if (__unlikely(result + size > limit)) {
        return (void*) mmtk_alloc(mutator, size, align, offset, allocator);
    } else{
        *cursor = result + size;
        return (void*)result;
    }
}

STATIC_INLINE void* mmtk_immix_alloc_fast(MMTkMutatorContext* mutator, size_t size, size_t align, size_t offset) {
    ImmixAllocator* allocator = &mutator->allocators.immix[MMTK_DEFAULT_IMMIX_ALLOCATOR];
    return bump_alloc_fast(mutator, (uintptr_t*)&allocator->cursor, (intptr_t)allocator->limit, size, align, offset, 0);
}

inline void mmtk_immix_post_alloc_slow(MMTkMutatorContext* mutator, void* obj, size_t size) {
    mmtk_post_alloc(mutator, obj, size, 0);
}

STATIC_INLINE void mmtk_immix_post_alloc_fast(MMTkMutatorContext* mutator, void* obj, size_t size) {
    // FIXME: for now, we do nothing
    // but when supporting moving, this is where we set the valid object (VO) bit
}

STATIC_INLINE void* mmtk_immortal_alloc_fast(MMTkMutatorContext* mutator, size_t size, size_t align, size_t offset) {
    BumpAllocator* allocator = &mutator->allocators.bump_pointer[MMTK_IMMORTAL_BUMP_ALLOCATOR];
    return bump_alloc_fast(mutator, (uintptr_t*)&allocator->cursor, (uintptr_t)allocator->limit, size, align, offset, 1);
}

STATIC_INLINE void mmtk_immortal_post_alloc_fast(MMTkMutatorContext* mutator, void* obj, size_t size) {
    // FIXME: Similarly, for now, we do nothing
    // but when supporting moving, this is where we set the valid object (VO) bit
    // and log (old gen) bit
}

JL_DLLEXPORT jl_value_t *jl_mmtk_gc_alloc_default(jl_ptls_t ptls, int osize, size_t align, void *ty)
{
    // safepoint
    jl_gc_safepoint_(ptls);

    jl_value_t *v;
    if ((uintptr_t)ty != jl_buff_tag) {
        // v needs to be 16 byte aligned, therefore v_tagged needs to be offset accordingly to consider the size of header
        jl_taggedvalue_t *v_tagged = (jl_taggedvalue_t *)mmtk_immix_alloc_fast(&ptls->gc_tls.mmtk_mutator, LLT_ALIGN(osize, align), align, sizeof(jl_taggedvalue_t));
        v = jl_valueof(v_tagged);
        mmtk_immix_post_alloc_fast(&ptls->gc_tls.mmtk_mutator, v, LLT_ALIGN(osize, align));
    } else {
        // allocating an extra word to store the size of buffer objects
        jl_taggedvalue_t *v_tagged = (jl_taggedvalue_t *)mmtk_immix_alloc_fast(&ptls->gc_tls.mmtk_mutator, LLT_ALIGN(osize+sizeof(jl_taggedvalue_t), align), align, 0);
        jl_value_t* v_tagged_aligned = ((jl_value_t*)((char*)(v_tagged) + sizeof(jl_taggedvalue_t)));
        v = jl_valueof(v_tagged_aligned);
        mmtk_store_obj_size_c(v, LLT_ALIGN(osize+sizeof(jl_taggedvalue_t), align));
        mmtk_immix_post_alloc_fast(&ptls->gc_tls.mmtk_mutator, v, LLT_ALIGN(osize+sizeof(jl_taggedvalue_t), align));
    }

    ptls->gc_tls_common.gc_num.allocd += osize;
    ptls->gc_tls_common.gc_num.poolalloc++;

    return v;
}

JL_DLLEXPORT jl_value_t *jl_mmtk_gc_alloc_big(jl_ptls_t ptls, size_t sz)
{
    // safepoint
    jl_gc_safepoint_(ptls);

    size_t offs = offsetof(bigval_t, header);
    assert(sz >= sizeof(jl_taggedvalue_t) && "sz must include tag");
    static_assert(offsetof(bigval_t, header) >= sizeof(void*), "Empty bigval header?");
    static_assert(sizeof(bigval_t) % JL_HEAP_ALIGNMENT == 0, "");
    size_t allocsz = LLT_ALIGN(sz + offs, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz) { // overflow in adding offs, size was "negative"
        assert(0 && "Error when allocating big object");
        jl_throw(jl_memory_exception);
    }

    bigval_t *v = (bigval_t*)mmtk_alloc_large(&ptls->gc_tls.mmtk_mutator, allocsz, JL_CACHE_BYTE_ALIGNMENT, 0, 2);

    if (v == NULL) {
        assert(0 && "Allocation failed");
        jl_throw(jl_memory_exception);
    }
    v->sz = allocsz;

    ptls->gc_tls_common.gc_num.allocd += allocsz;
    ptls->gc_tls_common.gc_num.bigalloc++;

    jl_value_t *result = jl_valueof(&v->header);
    mmtk_post_alloc(&ptls->gc_tls.mmtk_mutator, result, allocsz, 2);

    return result;
}

// Instrumented version of jl_gc_small_alloc_inner, called into by LLVM-generated code.
JL_DLLEXPORT jl_value_t *jl_gc_small_alloc(jl_ptls_t ptls, int offset, int osize, jl_value_t* type)
{
    assert(jl_atomic_load_relaxed(&ptls->gc_state) == 0);

    jl_value_t *val = jl_mmtk_gc_alloc_default(ptls, osize, 16, NULL);
    maybe_record_alloc_to_profile(val, osize, (jl_datatype_t*)type);
    return val;
}

// Instrumented version of jl_gc_big_alloc_inner, called into by LLVM-generated code.
JL_DLLEXPORT jl_value_t *jl_gc_big_alloc(jl_ptls_t ptls, size_t sz, jl_value_t *type)
{
    // TODO: assertion needed here?
    assert(jl_atomic_load_relaxed(&ptls->gc_state) == 0);

    jl_value_t *val = jl_mmtk_gc_alloc_big(ptls, sz);
    maybe_record_alloc_to_profile(val, sz, (jl_datatype_t*)type);
    return val;
}

inline jl_value_t *jl_gc_alloc_(jl_ptls_t ptls, size_t sz, void *ty)
{
    jl_value_t *v;
    const size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    if (sz <= GC_MAX_SZCLASS) {
        v = jl_mmtk_gc_alloc_default(ptls, allocsz, 16, ty);
    }
    else {
        if (allocsz < sz) // overflow in adding offs, size was "negative"
            jl_throw(jl_memory_exception);
        v = jl_mmtk_gc_alloc_big(ptls, allocsz);
    }
    jl_set_typeof(v, ty);
    maybe_record_alloc_to_profile(v, sz, (jl_datatype_t*)ty);
    return v;
}

// allocation wrappers that track allocation and let collection run
JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    void *data = malloc(sz);
    if (data != NULL && pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        malloc_maybe_collect(ptls, sz);
        jl_atomic_fetch_add_relaxed(&JULIA_MALLOC_BYTES, sz);
    }
    return data;
}

JL_DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    void *data = calloc(nm, sz);
    if (data != NULL && pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        malloc_maybe_collect(ptls, nm * sz);
        jl_atomic_fetch_add_relaxed(&JULIA_MALLOC_BYTES, nm * sz);
    }
    return data;
}

JL_DLLEXPORT void jl_gc_counted_free_with_size(void *p, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    free(p);
    if (pgcstack != NULL && ct->world_age) {
        jl_atomic_fetch_add_relaxed(&JULIA_MALLOC_BYTES, -sz);
    }
}

JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    if (pgcstack && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        malloc_maybe_collect(ptls, sz);
        if (sz < old)
            jl_atomic_fetch_add_relaxed(&JULIA_MALLOC_BYTES, old - sz);
        else
            jl_atomic_fetch_add_relaxed(&JULIA_MALLOC_BYTES, sz - old);
    }
    return realloc(p, sz);
}

void *jl_gc_perm_alloc_nolock(size_t sz, int zero, unsigned align, unsigned offset)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    size_t allocsz = mmtk_align_alloc_sz(sz);
    void* addr = mmtk_immortal_alloc_fast(&ptls->gc_tls.mmtk_mutator, allocsz, align, offset);
    return addr;
}

void *jl_gc_perm_alloc(size_t sz, int zero, unsigned align, unsigned offset)
{
    return jl_gc_perm_alloc_nolock(sz, zero, align, offset);
}

jl_value_t *jl_gc_permobj(size_t sz, void *ty, unsigned align) JL_NOTSAFEPOINT
{
    const size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    if (align == 0) {
        align = ((sz == 0) ? sizeof(void*) : (allocsz <= sizeof(void*) * 2 ?
                                                 sizeof(void*) * 2 : 16));
    }
    jl_taggedvalue_t *o = (jl_taggedvalue_t*)jl_gc_perm_alloc(allocsz, 0, align,
                                                              sizeof(void*) % align);

    jl_ptls_t ptls = jl_current_task->ptls;
    mmtk_immortal_post_alloc_fast(&ptls->gc_tls.mmtk_mutator, jl_valueof(o), allocsz);
    o->header = (uintptr_t)ty;
    return jl_valueof(o);
}

JL_DLLEXPORT void *jl_gc_managed_malloc(size_t sz)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    maybe_collect(ptls);
    size_t allocsz = LLT_ALIGN(sz, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);

    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    void *b = malloc_cache_align(allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);

    jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.allocd) + allocsz);
    jl_atomic_store_relaxed(&ptls->gc_tls_common.gc_num.malloc,
        jl_atomic_load_relaxed(&ptls->gc_tls_common.gc_num.malloc) + 1);
    // FIXME: Should these be part of mmtk's heap?
    // malloc_maybe_collect(ptls, sz);
    // jl_atomic_fetch_add_relaxed(&JULIA_MALLOC_BYTES, allocsz);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    // jl_gc_managed_malloc is currently always used for allocating array buffers.
    maybe_record_alloc_to_profile((jl_value_t*)b, sz, (jl_datatype_t*)jl_buff_tag);
    return b;
}

void jl_gc_notify_image_load(const char* img_data, size_t len)
{
    mmtk_set_vm_space((void*)img_data, len);
}

// ========================================================================= //
// Code specific to stock that is not supported by MMTk
// ========================================================================= //

// mutex for page profile
uv_mutex_t page_profile_lock;

JL_DLLEXPORT void jl_gc_take_page_profile(ios_t *stream)
{
    uv_mutex_lock(&page_profile_lock);
    const char *str = "Page profiler in unsupported in MMTk.";
    ios_write(stream, str, strlen(str));
    uv_mutex_unlock(&page_profile_lock);
}

// this seems to be needed by the gc tests
#define JL_GC_N_MAX_POOLS 51
JL_DLLEXPORT double jl_gc_page_utilization_stats[JL_GC_N_MAX_POOLS];

STATIC_INLINE void gc_dump_page_utilization_data(void) JL_NOTSAFEPOINT
{
    // FIXME: MMTk would have to provide its own stats
}

#define MMTK_GC_PAGE_SZ (1 << 12) // MMTk's page size is defined in mmtk-core constants

JL_DLLEXPORT uint64_t jl_get_pg_size(void)
{
    return MMTK_GC_PAGE_SZ;
}

// Not used by mmtk
// Number of GC threads that may run parallel marking
int jl_n_markthreads;
// Number of GC threads that may run concurrent sweeping (0 or 1)
int jl_n_sweepthreads;
// `tid` of first GC thread
int gc_first_tid;
// Number of threads sweeping stacks
_Atomic(int) gc_n_threads_sweeping_stacks;
// counter for sharing work when sweeping stacks
_Atomic(int) gc_ptls_sweep_idx;
// counter for round robin of giving back stack pages to the OS
_Atomic(int) gc_stack_free_idx = 0;

JL_DLLEXPORT void jl_gc_queue_root(const struct _jl_value_t *ptr) JL_NOTSAFEPOINT
{
    mmtk_unreachable();
}

JL_DLLEXPORT void jl_gc_queue_multiroot(const struct _jl_value_t *root, const void *stored,
                                        struct _jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    mmtk_unreachable();
}

JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj)
{
    mmtk_unreachable();
    return 0;
}

JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent,
                                            jl_value_t **objs, size_t nobjs)
{
    mmtk_unreachable();
}

JL_DLLEXPORT size_t jl_gc_max_internal_obj_size(void)
{
    // TODO: meaningful for MMTk?
    return GC_MAX_SZCLASS;
}

JL_DLLEXPORT void jl_gc_schedule_foreign_sweepfunc(jl_ptls_t ptls, jl_value_t *obj)
{
    // FIXME: do we need to implement this?
}

// gc-debug functions
JL_DLLEXPORT jl_taggedvalue_t *jl_gc_find_taggedvalue_pool(char *p, size_t *osize_p)
{
    return NULL;
}

void jl_gc_debug_critical_error(void) JL_NOTSAFEPOINT
{
}

int gc_is_collector_thread(int tid) JL_NOTSAFEPOINT
{
    return 0;
}

void jl_gc_debug_print_status(void) JL_NOTSAFEPOINT
{
    // May not be accurate but should be helpful enough
    uint64_t pool_count = gc_num.poolalloc;
    uint64_t big_count = gc_num.bigalloc;
    jl_safe_printf("Allocations: %" PRIu64 " "
                   "(Pool: %" PRIu64 "; Big: %" PRIu64 "); GC: %d\n",
                   pool_count + big_count, pool_count, big_count, gc_num.pause);
}

JL_DLLEXPORT size_t jl_gc_external_obj_hdr_size(void)
{
    return sizeof(bigval_t);
}

void jl_print_gc_stats(JL_STREAM *s)
{
}

JL_DLLEXPORT int jl_gc_enable_conservative_gc_support(void)
{
    return 0;
}

JL_DLLEXPORT int jl_gc_conservative_gc_support_enabled(void)
{
    return 0;
}

// TODO: if this is needed, it can be added in MMTk
JL_DLLEXPORT jl_value_t *jl_gc_internal_obj_base_ptr(void *p)
{
    return NULL;
}

#ifdef __cplusplus
}
#endif
