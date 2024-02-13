// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifdef THIRD_PARY_GC
#error "This file should not be compiled with MMTK GC"
#endif

#include "gc-stock.h"
#include "gc-interface-collector.h"
#include "gc-page-profiler.h"
#include "julia_gcext.h"

#ifdef __cplusplus
extern "C" {
#endif

jl_gc_callback_list_t *gc_cblist_root_scanner;
jl_gc_callback_list_t *gc_cblist_task_scanner;
jl_gc_callback_list_t *gc_cblist_pre_gc;
jl_gc_callback_list_t *gc_cblist_post_gc;
jl_gc_callback_list_t *gc_cblist_notify_external_alloc;
jl_gc_callback_list_t *gc_cblist_notify_external_free;
jl_gc_callback_list_t *gc_cblist_notify_gc_pressure;

#ifndef MMTK_GC

// =======
// Callbacks
// =======

static void jl_gc_register_callback(jl_gc_callback_list_t **list,
        jl_gc_cb_func_t func)
{
    while (*list != NULL) {
        if ((*list)->func == func)
            return;
        list = &((*list)->next);
    }
    *list = (jl_gc_callback_list_t *)malloc_s(sizeof(jl_gc_callback_list_t));
    (*list)->next = NULL;
    (*list)->func = func;
}

static void jl_gc_deregister_callback(jl_gc_callback_list_t **list,
        jl_gc_cb_func_t func)
{
    while (*list != NULL) {
        if ((*list)->func == func) {
            jl_gc_callback_list_t *tmp = *list;
            (*list) = (*list)->next;
            free(tmp);
            return;
        }
        list = &((*list)->next);
    }
}

JL_DLLEXPORT void jl_gc_set_cb_root_scanner(jl_gc_cb_root_scanner_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_root_scanner, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_root_scanner, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_task_scanner(jl_gc_cb_task_scanner_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_task_scanner, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_task_scanner, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_pre_gc(jl_gc_cb_pre_gc_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_pre_gc, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_pre_gc, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_post_gc(jl_gc_cb_post_gc_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_post_gc, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_post_gc, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_notify_external_alloc(jl_gc_cb_notify_external_alloc_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_notify_external_alloc, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_notify_external_alloc, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_notify_external_free(jl_gc_cb_notify_external_free_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_notify_external_free, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_notify_external_free, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_notify_gc_pressure(jl_gc_cb_notify_gc_pressure_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_notify_gc_pressure, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_notify_gc_pressure, (jl_gc_cb_func_t)cb);
}

// =======
// malloc wrappers, aligned allocation
// =======

#if defined(_OS_WINDOWS_)
void *jl_malloc_aligned(size_t sz, size_t align)
{
    return _aligned_malloc(sz ? sz : 1, align);
}
void *jl_realloc_aligned(void *p, size_t sz, size_t oldsz,
                                       size_t align)
{
    (void)oldsz;
    return _aligned_realloc(p, sz ? sz : 1, align);
}
void jl_free_aligned(void *p) JL_NOTSAFEPOINT
{
    _aligned_free(p);
}
#else
void *jl_malloc_aligned(size_t sz, size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return malloc(sz);
#endif
    void *ptr;
    if (posix_memalign(&ptr, align, sz))
        return NULL;
    return ptr;
}
void *jl_realloc_aligned(void *d, size_t sz, size_t oldsz,
                                       size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return realloc(d, sz);
#endif
    void *b = jl_malloc_aligned(sz, align);
    if (b != NULL) {
        memcpy(b, d, oldsz > sz ? sz : oldsz);
        free(d);
    }
    return b;
}
void jl_free_aligned(void *p) JL_NOTSAFEPOINT
{
    free(p);
}
#endif

// =======
// weak references
// =======

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref_th(jl_ptls_t ptls,
                                                jl_value_t *value)
{
    jl_weakref_t *wr = (jl_weakref_t*)jl_gc_alloc(ptls, sizeof(void*),
                                                  jl_weakref_type);
    wr->value = value;  // NOTE: wb not needed here
    small_arraylist_push(&ptls->heap.weak_refs, wr);
    return wr;
}

// =======
// allocation
// =======

extern void jl_batch_accum_heap_size(jl_ptls_t ptls, uint64_t sz) JL_NOTSAFEPOINT;
extern void jl_batch_accum_free_size(jl_ptls_t ptls, uint64_t sz) JL_NOTSAFEPOINT;

// Size includes the tag and the tag is not cleared!!
jl_value_t *jl_gc_big_alloc_inner(jl_ptls_t ptls, size_t sz)
{
    maybe_collect(ptls);
    size_t offs = offsetof(bigval_t, header);
    assert(sz >= sizeof(jl_taggedvalue_t) && "sz must include tag");
    static_assert(offsetof(bigval_t, header) >= sizeof(void*), "Empty bigval header?");
    static_assert(sizeof(bigval_t) % JL_HEAP_ALIGNMENT == 0, "");
    size_t allocsz = LLT_ALIGN(sz + offs, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    bigval_t *v = (bigval_t*)malloc_cache_align(allocsz);
    if (v == NULL)
        jl_throw(jl_memory_exception);
    gc_invoke_callbacks(jl_gc_cb_notify_external_alloc_t,
        gc_cblist_notify_external_alloc, (v, allocsz));
    jl_atomic_store_relaxed(&ptls->gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_num.allocd) + allocsz);
    jl_atomic_store_relaxed(&ptls->gc_num.bigalloc,
        jl_atomic_load_relaxed(&ptls->gc_num.bigalloc) + 1);
    jl_batch_accum_heap_size(ptls, allocsz);
#ifdef MEMDEBUG
    memset(v, 0xee, allocsz);
#endif
    v->sz = allocsz;
    gc_big_object_link(v, &ptls->heap.big_objects);
    return jl_valueof(&v->header);
}

// Size includes the tag and the tag is not cleared!!
jl_value_t *jl_gc_pool_alloc_inner(jl_ptls_t ptls, int pool_offset,
                                          int osize)
{
    // Use the pool offset instead of the pool address as the argument
    // to workaround a llvm bug.
    // Ref https://llvm.org/bugs/show_bug.cgi?id=27190
    jl_gc_pool_t *p = (jl_gc_pool_t*)((char*)ptls + pool_offset);
    assert(jl_atomic_load_relaxed(&ptls->gc_state) == 0);
#ifdef MEMDEBUG
    return jl_gc_big_alloc(ptls, osize);
#endif
    maybe_collect(ptls);
    jl_atomic_store_relaxed(&ptls->gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_num.allocd) + osize);
    jl_atomic_store_relaxed(&ptls->gc_num.pool_live_bytes,
        jl_atomic_load_relaxed(&ptls->gc_num.pool_live_bytes) + osize);
    jl_atomic_store_relaxed(&ptls->gc_num.poolalloc,
        jl_atomic_load_relaxed(&ptls->gc_num.poolalloc) + 1);
    // first try to use the freelist
    jl_taggedvalue_t *v = p->freelist;
    if (v != NULL) {
        jl_taggedvalue_t *next = v->next;
        p->freelist = next;
        if (__unlikely(gc_page_data(v) != gc_page_data(next))) {
            // we only update pg's fields when the freelist changes page
            // since pg's metadata is likely not in cache
            jl_gc_pagemeta_t *pg = jl_assume(page_metadata_unsafe(v));
            assert(pg->osize == p->osize);
            pg->nfree = 0;
            pg->has_young = 1;
        }
        msan_allocated_memory(v, osize);
        return jl_valueof(v);
    }
    // if the freelist is empty we reuse empty but not freed pages
    v = p->newpages;
    jl_taggedvalue_t *next = (jl_taggedvalue_t*)((char*)v + osize);
    // If there's no pages left or the current page is used up,
    // we need to use the slow path.
    char *cur_page = gc_page_data((char*)v - 1);
    if (__unlikely(v == NULL || cur_page + GC_PAGE_SZ < (char*)next)) {
        if (v != NULL) {
            // like the freelist case,
            // but only update the page metadata when it is full
            jl_gc_pagemeta_t *pg = jl_assume(page_metadata_unsafe((char*)v - 1));
            assert(pg->osize == p->osize);
            pg->nfree = 0;
            pg->has_young = 1;
        }
        v = gc_add_page(p);
        next = (jl_taggedvalue_t*)((char*)v + osize);
    }
    p->newpages = next;
    msan_allocated_memory(v, osize);
    return jl_valueof(v);
}

// =======
// Write barrier slow-path
// =======

JL_DLLEXPORT void jl_gc_queue_root(const jl_value_t *ptr)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_taggedvalue_t *o = jl_astaggedvalue(ptr);
    // The modification of the `gc_bits` is not atomic but it
    // should be safe here since GC is not allowed to run here and we only
    // write GC_OLD to the GC bits outside GC. This could cause
    // duplicated objects in the remset but that shouldn't be a problem.
    o->bits.gc = GC_MARKED;
    arraylist_push(ptls->heap.remset, (jl_value_t*)ptr);
    ptls->heap.remset_nptr++; // conservative
}

void jl_gc_queue_multiroot(const jl_value_t *parent, const void *ptr, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *ly = dt->layout;
    uint32_t npointers = ly->npointers;
    //if (npointers == 0) // this was checked by the caller
    //    return;
    jl_value_t *ptrf = ((jl_value_t**)ptr)[ly->first_ptr];
    if (ptrf && (jl_astaggedvalue(ptrf)->bits.gc & 1) == 0) {
        // this pointer was young, move the barrier back now
        jl_gc_wb_back(parent);
        return;
    }
    const uint8_t *ptrs8 = (const uint8_t *)jl_dt_layout_ptrs(ly);
    const uint16_t *ptrs16 = (const uint16_t *)jl_dt_layout_ptrs(ly);
    const uint32_t *ptrs32 = (const uint32_t*)jl_dt_layout_ptrs(ly);
    for (size_t i = 1; i < npointers; i++) {
        uint32_t fld;
        if (ly->flags.fielddesc_type == 0) {
            fld = ptrs8[i];
        }
        else if (ly->flags.fielddesc_type == 1) {
            fld = ptrs16[i];
        }
        else {
            assert(ly->flags.fielddesc_type == 2);
            fld = ptrs32[i];
        }
        jl_value_t *ptrf = ((jl_value_t**)ptr)[fld];
        if (ptrf && (jl_astaggedvalue(ptrf)->bits.gc & 1) == 0) {
            // this pointer was young, move the barrier back now
            jl_gc_wb_back(parent);
            return;
        }
    }
}

// =======
// Collection triggers
// =======

void enable_collection(void)
{
    // no-op for Julia's GC
}
void disable_collection(void)
{
    // no-op for Julia's GC
}

void maybe_collect(jl_ptls_t ptls)
{
    if (jl_atomic_load_relaxed(&gc_heap_stats.heap_size) >= jl_atomic_load_relaxed(&gc_heap_stats.heap_target) || jl_gc_debug_check_other()) {
        jl_gc_collect(JL_GC_AUTO);
    }
    else {
        jl_gc_safepoint_(ptls);
    }
}

void jl_gc_wait_for_the_world(jl_ptls_t* gc_all_tls_states, int gc_n_threads);
int _jl_gc_collect(jl_ptls_t ptls, jl_gc_collection_t collection);
void run_finalizers(jl_task_t *ct, int finalizers_thread);

JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t collection)
{
    JL_PROBE_GC_BEGIN(collection);

    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (jl_atomic_load_acquire(&jl_gc_disable_counter)) {
        size_t localbytes = jl_atomic_load_relaxed(&ptls->gc_num.allocd) + gc_num.interval;
        jl_atomic_store_relaxed(&ptls->gc_num.allocd, -(int64_t)gc_num.interval);
        static_assert(sizeof(_Atomic(uint64_t)) == sizeof(gc_num.deferred_alloc), "");
        jl_atomic_fetch_add((_Atomic(uint64_t)*)&gc_num.deferred_alloc, localbytes);
        return;
    }
    jl_gc_debug_print();

    int8_t old_state = jl_atomic_load_relaxed(&ptls->gc_state);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    // `jl_safepoint_start_gc()` makes sure only one thread can run the GC.
    uint64_t t0 = jl_hrtime();
    if (!jl_safepoint_start_gc()) {
        // either another thread is running GC, or the GC got disabled just now.
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        jl_safepoint_wait_thread_resume(); // block in thread-suspend now if requested, after clearing the gc_state
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

    gc_invoke_callbacks(jl_gc_cb_pre_gc_t,
        gc_cblist_pre_gc, (collection));

    if (!jl_atomic_load_acquire(&jl_gc_disable_counter)) {
        JL_LOCK_NOGC(&finalizers_lock); // all the other threads are stopped, so this does not make sense, right? otherwise, failing that, this seems like plausibly a deadlock
#ifndef __clang_gcanalyzer__
        if (_jl_gc_collect(ptls, collection)) {
            // recollect
            int ret = _jl_gc_collect(ptls, JL_GC_AUTO);
            (void)ret;
            assert(!ret);
        }
#endif
        JL_UNLOCK_NOGC(&finalizers_lock);
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    JL_PROBE_GC_END();
    jl_safepoint_wait_thread_resume(); // block in thread-suspend now if requested, after clearing the gc_state

    // Only disable finalizers on current thread
    // Doing this on all threads is racy (it's impossible to check
    // or wait for finalizers on other threads without dead lock).
    if (!ptls->finalizers_inhibited && ptls->locks.len == 0) {
        JL_TIMING(GC, GC_Finalizers);
        run_finalizers(ct, 0);
    }
    JL_PROBE_GC_FINALIZER();

    gc_invoke_callbacks(jl_gc_cb_post_gc_t,
        gc_cblist_post_gc, (collection));
    if (under_pressure)
        gc_invoke_callbacks(jl_gc_cb_notify_gc_pressure_t,
            gc_cblist_notify_gc_pressure, ());
    under_pressure = 0;
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
}

// =======
// Per-thread GC initialization
// =======

// Per-thread initialization
void jl_init_thread_heap(jl_ptls_t ptls)
{
    jl_thread_heap_t *heap = &ptls->heap;
    jl_gc_pool_t *p = heap->norm_pools;
    for (int i = 0; i < JL_GC_N_POOLS; i++) {
        p[i].osize = jl_gc_sizeclasses[i];
        p[i].freelist = NULL;
        p[i].newpages = NULL;
    }
    small_arraylist_new(&heap->weak_refs, 0);
    small_arraylist_new(&heap->live_tasks, 0);
    for (int i = 0; i < JL_N_STACK_POOLS; i++)
        small_arraylist_new(&heap->free_stacks[i], 0);
    heap->mallocarrays = NULL;
    heap->mafreelist = NULL;
    heap->big_objects = NULL;
    heap->remset = &heap->_remset[0];
    heap->last_remset = &heap->_remset[1];
    arraylist_new(heap->remset, 0);
    arraylist_new(heap->last_remset, 0);
    arraylist_new(&ptls->finalizers, 0);
    arraylist_new(&ptls->sweep_objs, 0);

    jl_gc_mark_cache_t *gc_cache = &ptls->gc_cache;
    gc_cache->perm_scanned_bytes = 0;
    gc_cache->scanned_bytes = 0;
    gc_cache->nbig_obj = 0;

    // Initialize GC mark-queue
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    ws_queue_t *cq = &mq->chunk_queue;
    ws_array_t *wsa = create_ws_array(GC_CHUNK_QUEUE_INIT_SIZE, sizeof(jl_gc_chunk_t));
    jl_atomic_store_relaxed(&cq->top, 0);
    jl_atomic_store_relaxed(&cq->bottom, 0);
    jl_atomic_store_relaxed(&cq->array, wsa);
    ws_queue_t *q = &mq->ptr_queue;
    ws_array_t *wsa2 = create_ws_array(GC_PTR_QUEUE_INIT_SIZE, sizeof(jl_value_t *));
    jl_atomic_store_relaxed(&q->top, 0);
    jl_atomic_store_relaxed(&q->bottom, 0);
    jl_atomic_store_relaxed(&q->array, wsa2);
    arraylist_new(&mq->reclaim_set, 32);

    memset(&ptls->gc_num, 0, sizeof(ptls->gc_num));
    jl_atomic_store_relaxed(&ptls->gc_num.allocd, -(int64_t)gc_num.interval);
}

void jl_post_init_thread_heap(jl_ptls_t ptls)
{
    // no-op for Julia's GC
}

// =======
// System-wide GC initialization
// =======

// System-wide initializations
void jl_gc_init(void)
{
    JL_MUTEX_INIT(&heapsnapshot_lock, "heapsnapshot_lock");
    JL_MUTEX_INIT(&finalizers_lock, "finalizers_lock");
    uv_mutex_init(&page_profile_lock);
    uv_mutex_init(&gc_cache_lock);
    uv_mutex_init(&gc_perm_lock);
    uv_mutex_init(&gc_threads_lock);
    uv_cond_init(&gc_threads_cond);
    uv_sem_init(&gc_sweep_assists_needed, 0);
    uv_mutex_init(&gc_queue_observer_lock);

    jl_gc_init_page();
    jl_gc_debug_init();

    arraylist_new(&finalizer_list_marked, 0);
    arraylist_new(&to_finalize, 0);
    jl_atomic_store_relaxed(&gc_heap_stats.heap_target, default_collect_interval);
    gc_num.interval = default_collect_interval;
    last_long_collect_interval = default_collect_interval;
    gc_num.allocd = 0;
    gc_num.max_pause = 0;
    gc_num.max_memory = 0;

#ifdef _P64
    total_mem = uv_get_total_memory();
    uint64_t constrained_mem = uv_get_constrained_memory();
    if (constrained_mem > 0 && constrained_mem < total_mem)
        jl_gc_set_max_memory(constrained_mem - 250*1024*1024); // LLVM + other libraries need some amount of memory
#endif
    if (jl_options.heap_size_hint)
        jl_gc_set_max_memory(jl_options.heap_size_hint - 250*1024*1024);

    t_start = jl_hrtime();
}

// =======
// allocation wrappers that track allocation and let collection run
// =======

JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    void *data = malloc(sz);
    if (data != NULL && pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        maybe_collect(ptls);
        jl_atomic_store_relaxed(&ptls->gc_num.allocd,
            jl_atomic_load_relaxed(&ptls->gc_num.allocd) + sz);
        jl_atomic_store_relaxed(&ptls->gc_num.malloc,
            jl_atomic_load_relaxed(&ptls->gc_num.malloc) + 1);
        jl_batch_accum_heap_size(ptls, sz);
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
        maybe_collect(ptls);
        jl_atomic_store_relaxed(&ptls->gc_num.allocd,
            jl_atomic_load_relaxed(&ptls->gc_num.allocd) + nm*sz);
        jl_atomic_store_relaxed(&ptls->gc_num.malloc,
            jl_atomic_load_relaxed(&ptls->gc_num.malloc) + 1);
        jl_batch_accum_heap_size(ptls, sz * nm);
    }
    return data;
}

JL_DLLEXPORT void jl_gc_counted_free_with_size(void *p, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    free(p);
    if (pgcstack != NULL && ct->world_age) {
        jl_batch_accum_free_size(ct->ptls, sz);
    }
}

JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    void *data = realloc(p, sz);
    if (data != NULL && pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        maybe_collect(ptls);
        if (!(sz < old))
            jl_atomic_store_relaxed(&ptls->gc_num.allocd,
                jl_atomic_load_relaxed(&ptls->gc_num.allocd) + (sz - old));
        jl_atomic_store_relaxed(&ptls->gc_num.realloc,
            jl_atomic_load_relaxed(&ptls->gc_num.realloc) + 1);

        int64_t diff = sz - old;
        if (diff < 0) {
            jl_batch_accum_free_size(ptls, -diff);
        }
        else {
            jl_batch_accum_heap_size(ptls, diff);
        }
    }
    return data;
}

// =======
// String reallocation
// =======

void *gc_managed_realloc_(jl_ptls_t ptls, void *d, size_t sz, size_t oldsz, int isaligned, jl_value_t *owner, int8_t can_collect);

jl_value_t *jl_gc_realloc_string(jl_value_t *s, size_t sz)
{
    size_t len = jl_string_len(s);
    if (sz <= len) return s;
    jl_taggedvalue_t *v = jl_astaggedvalue(s);
    size_t strsz = len + sizeof(size_t) + 1;
    if (strsz <= GC_MAX_SZCLASS ||
        // TODO: because of issue #17971 we can't resize old objects
        gc_marked(v->bits.gc)) {
        // pool allocated; can't be grown in place so allocate a new object.
        jl_value_t *snew = jl_alloc_string(sz);
        memcpy(jl_string_data(snew), jl_string_data(s), len);
        return snew;
    }
    size_t newsz = sz + sizeof(size_t) + 1;
    size_t offs = sizeof(bigval_t);
    size_t oldsz = LLT_ALIGN(strsz + offs, JL_CACHE_BYTE_ALIGNMENT);
    size_t allocsz = LLT_ALIGN(newsz + offs, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    bigval_t *hdr = bigval_header(v);
    jl_ptls_t ptls = jl_current_task->ptls;
    maybe_collect(ptls); // don't want this to happen during jl_gc_managed_realloc
    gc_big_object_unlink(hdr);
    // TODO: this is not safe since it frees the old pointer. ideally we'd like
    // the old pointer to be left alone if we can't grow in place.
    // for now it's up to the caller to make sure there are no references to the
    // old pointer.
    bigval_t *newbig = (bigval_t*)gc_managed_realloc_(ptls, hdr, allocsz, oldsz, 1, s, 0);
    newbig->sz = allocsz;
    gc_big_object_link(newbig, &ptls->heap.big_objects);
    jl_value_t *snew = jl_valueof(&newbig->header);
    *(size_t*)snew = sz;
    return snew;
}

JL_DLLEXPORT int jl_gc_enable_conservative_gc_support(void)
{
    if (jl_is_initialized()) {
        int result = jl_atomic_fetch_or(&support_conservative_marking, 1);
        if (!result) {
            // Do a full collection to ensure that age bits are updated
            // properly. We don't have to worry about race conditions
            // for this part, as allocation itself is unproblematic and
            // a collection will wait for safepoints.
            jl_gc_collect(JL_GC_FULL);
        }
        return result;
    } else {
        int result = jl_atomic_load(&support_conservative_marking);
        jl_atomic_store(&support_conservative_marking, 1);
        return result;
    }
}

JL_DLLEXPORT int jl_gc_conservative_gc_support_enabled(void)
{
    return jl_atomic_load(&support_conservative_marking);
}

JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj)
{
    int may_claim = gc_try_setmark_tag(jl_astaggedvalue(obj), GC_MARKED);
    if (may_claim) {
        gc_ptr_queue_push(&ptls->mark_queue, obj);
    }
    return may_claim;
}

JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent, jl_value_t **objs, size_t nobjs)
{
    uintptr_t nptr = (nobjs << 2) | (jl_astaggedvalue(parent)->bits.gc & 2);
    gc_mark_objarray(ptls, parent, objs, objs + nobjs, 1, nptr);
}

JL_DLLEXPORT jl_value_t *jl_gc_internal_obj_base_ptr(void *p)
{
    p = (char *) p - 1;
    jl_gc_pagemeta_t *meta = page_metadata(p);
    if (meta != NULL) {
        char *page = gc_page_data(p);
        // offset within page.
        size_t off = (char *)p - page;
        if (off < GC_PAGE_OFFSET)
            return NULL;
        // offset within object
        size_t off2 = (off - GC_PAGE_OFFSET);
        size_t osize = meta->osize;
        if (osize == 0)
            return NULL;
        off2 %= osize;
        if (off - off2 + osize > GC_PAGE_SZ)
            return NULL;
        jl_taggedvalue_t *cell = (jl_taggedvalue_t *)((char *)p - off2);
        // We have to distinguish between three cases:
        // 1. We are on a page where every cell is allocated.
        // 2. We are on a page where objects are currently bump-allocated
        //    from the corresponding pool->newpages list.
        // 3. We are on a page with a freelist that is used for object
        //    allocation.
        if (meta->nfree == 0) {
            // case 1: full page; `cell` must be an object
            goto valid_object;
        }
        jl_gc_pool_t *pool =
            gc_all_tls_states[meta->thread_n]->heap.norm_pools +
            meta->pool_n;
        if (meta->fl_begin_offset == UINT16_MAX) {
            // case 2: this is a page on the newpages list
            jl_taggedvalue_t *newpages = pool->newpages;
            // Check if the page is being allocated from via newpages
            if (!newpages)
                return NULL;
            char *data = gc_page_data(newpages);
            if (data != meta->data) {
                // Pages on newpages form a linked list where only the
                // first one is allocated from (see gc_reset_page()).
                // All other pages are empty.
                return NULL;
            }
            // This is the first page on the newpages list, where objects
            // are allocated from.
            if ((char *)cell >= (char *)newpages) // past allocation pointer
                return NULL;
            goto valid_object;
        }
        // case 3: this is a page with a freelist
        // marked or old objects can't be on the freelist
        if (cell->bits.gc)
            goto valid_object;
        // When allocating from a freelist, three subcases are possible:
        // * The freelist of a page has been exhausted; this was handled
        //   under case 1, as nfree == 0.
        // * The freelist of the page has not been used, and the age bits
        //   reflect whether a cell is on the freelist or an object.
        // * The freelist is currently being allocated from. In this case,
        //   pool->freelist will point to the current page; any cell with
        //   a lower address will be an allocated object, and for cells
        //   with the same or a higher address, the corresponding age
        //   bit will reflect whether it's on the freelist.
        // Age bits are set in sweep_page() and are 0 for freelist
        // entries and 1 for live objects. The above subcases arise
        // because allocating a cell will not update the age bit, so we
        // need extra logic for pages that have been allocated from.
        // We now distinguish between the second and third subcase.
        // Freelist entries are consumed in ascending order. Anything
        // before the freelist pointer was either live during the last
        // sweep or has been allocated since.
        if (gc_page_data(cell) == gc_page_data(pool->freelist)
            && (char *)cell < (char *)pool->freelist)
            goto valid_object;
        // already skipped marked or old objects above, so here
        // the age bits are 0, thus the object is on the freelist
        return NULL;
        // Not a freelist entry, therefore a valid object.
    valid_object:
        // We have to treat objects with type `jl_buff_tag` differently,
        // as they must not be passed to the usual marking functions.
        // Note that jl_buff_tag is real pointer into libjulia,
        // thus it cannot be a type reference.
        if ((cell->header & ~(uintptr_t) 3) == jl_buff_tag)
            return NULL;
        return jl_valueof(cell);
    }
    return NULL;
}

// =======
//  Permanent allocation
// =======

// **NOT** a safepoint
void *jl_gc_perm_alloc(size_t sz, int zero, unsigned align, unsigned offset)
{
    assert(align < GC_PERM_POOL_LIMIT);
#ifndef MEMDEBUG
    if (__unlikely(sz > GC_PERM_POOL_LIMIT))
#endif
        return gc_perm_alloc_large(sz, zero, align, offset);
    uv_mutex_lock(&gc_perm_lock);
    void *p = jl_gc_perm_alloc_nolock(sz, zero, align, offset);
    uv_mutex_unlock(&gc_perm_lock);
    return p;
}

void jl_gc_notify_image_load(const char* img_data, size_t len)
{
    // no-op for Julia's GC
}

void jl_gc_notify_image_alloc(char* img_data, size_t len)
{
    // no-op for Julia's GC
}

#ifdef __cplusplus
} // extern "C"
#endif

#endif // MMTK_GC
