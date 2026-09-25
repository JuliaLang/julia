// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// While a window on region n is open, the thread allocates into region n; a
// reset frees the whole region without a trace. The stock collector marks
// region objects like any other and never sweeps a region page; a census
// collects one region alone. doc/src/devdocs/gc-regions.md states the rules
// a program must keep and why they make the entries below sound.

#include "gc-common.h"
#include "gc-stock.h"
#include "gc-regions.h"

#ifdef __cplusplus
extern "C" {
#endif

// --- process-wide state ------------------------------------------------------

// The number of tasks that hold an open window, across every thread. A
// stop-the-world census, a global reset and the debug check return EBUSY
// while it is not zero; a stock collection parks the windows instead.
static _Atomic(int) region_windows_open = 0;

#ifdef WITH_GC_REGION_BARRIER
// The escape barrier is armed at the first window and stays armed. Armed,
// every write barrier compares the page tags of parent and child, and a
// store of a younger child into an older parent quarantines the child's
// region: an escape costs memory, never a dangling pointer.
JL_DLLEXPORT _Atomic(uint8_t) jl_gc_region_barrier_on = 0;
#endif
static _Atomic(uint64_t) region_quarantined_mask = 0;

// The census filter: the region of the census that runs now, 0 otherwise.
_Atomic(int) jl_gc_region_census_target = 0;

// The tasks a census reached outside the region, each scanned once. A stock
// collection leaves a task old-marked, so the mark bit cannot record the
// visit; this table does.
static htable_t region_census_tasks;
static size_t region_census_task_count = 0;

// With debug on, a refused reset reports the roots it found (jl_gc_region_set_debug).
static int region_debug_checks = 0;





STATIC_INLINE int region_valid(int n) JL_NOTSAFEPOINT
{
    return n > 0 && n < JL_GC_MAX_REGIONS;
}

// --- the region tree ------------------------------------------------------------
// region_parent[r] is the declared parent of r (0: a child of region 0);
// region_uptree[r] is the bitset of r, its ancestors and 0: the regions a
// store from an object of region r may target. The default is the chain
// 0 <- 1 <- 2 <- ..., where the test is cr <= pr; the first declaration
// replaces it by the all-root tree and applies the edge.
static uint8_t region_parent[JL_GC_MAX_REGIONS];
static _Atomic(uint64_t) region_uptree[JL_GC_MAX_REGIONS];




// The pages of region n on the calling heap.
JL_DLLEXPORT int jl_gc_region_pages(int n) JL_NOTSAFEPOINT
{
    if (!region_valid(n))
        return 0;
    jl_thread_heap_t *heap = &jl_current_task->ptls->gc_tls.heap;
    jl_gc_region_state_t *rs = heap->regions[n];
    return rs == NULL ? 0 : (int)rs->n_pages;
}

// 1 when an escape quarantined region n; the quarantine is process-wide
// and lasts until the next stock collection.
JL_DLLEXPORT int jl_gc_region_quarantined(int n) JL_NOTSAFEPOINT
{
    if (!region_valid(n))
        return 0;
    return (jl_atomic_load_relaxed(&region_quarantined_mask) >> n) & 1;
}

#ifdef WITH_GC_REGION_BARRIER
// --- the escape barrier ----------------------------------------------------------

// 1 when a store of `child` into `parent` breaks the reference rule; `cr`
// and `pr` receive the two regions.
STATIC_INLINE int region_store_escapes(const void *parent, const void *child, int *cr, int *pr) JL_NOTSAFEPOINT
{
    // Child first: a region-0 child is legal under any parent, the common case.
    jl_gc_pagemeta_t *cm = page_metadata((char*)child);
    *cr = cm ? cm->region_n : 0;
    if (__likely(*cr == 0))
        return 0;
    jl_gc_pagemeta_t *pm = page_metadata((char*)parent);
    *pr = pm ? pm->region_n : 0;
    // Legal when the child's region is the parent's or an ancestor of it.
    return !((jl_atomic_load_relaxed(&region_uptree[*pr]) >> *cr) & 1);
}

// The test without the quarantine, for the pair check of a bulk copy.
JL_DLLEXPORT int jl_gc_region_would_escape(const void *parent, const void *child) JL_NOTSAFEPOINT
{
    int cr = 0, pr = 0;
    return region_store_escapes(parent, child, &cr, &pr);
}

JL_DLLEXPORT void jl_gc_region_wb(const void *parent, const void *child) JL_NOTSAFEPOINT
{
    int cr = 0, pr = 0;
    // The null test is here and not at the call site: a comparison of the
    // stored value in the caller hides its root from the GC checker.
    if (child == NULL || __likely(!region_store_escapes(parent, child, &cr, &pr)))
        return;
    uint64_t bit = (uint64_t)1 << cr;
    uint64_t seen = jl_atomic_fetch_or_relaxed(&region_quarantined_mask, bit);
    if (!(seen & bit))
        jl_safe_printf("REGION-ESCAPE: a %s of region %d was stored into a %s "
                       "of region %d; region %d is quarantined until the next "
                       "collection hands its pages to the stock collector\n",
                       jl_typeof_str((jl_value_t*)child), cr,
                       jl_typeof_str((jl_value_t*)parent), pr, cr);
    // With debug on, the first escape of a region prints a backtrace.
    if (!(seen & bit) && region_debug_checks)
        jl_print_backtrace();
}

// The elements of a bulk copy, one by one, after the pair check of the
// containers failed: a young container of old elements is legal, so only a
// real escape quarantines. `n` boxed elements from `src`:
JL_DLLEXPORT void jl_gc_region_wb_boxed(const void *parent, _Atomic(void*) *src, size_t n) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < n; i++) {
        void *val = jl_atomic_load_relaxed(src + i);
        if (val != NULL)
            jl_gc_region_wb(parent, val);
    }
}

// `n` inline elements of type `et`, `elsz` bytes apart, from `src`: every
// pointer field of every element (n = 1 for one immutable, jl_gc_multi_wb).
JL_DLLEXPORT void jl_gc_region_wb_inline(const void *parent, const char *src, size_t n,
                                         size_t elsz, jl_datatype_t *et) JL_NOTSAFEPOINT
{
    uint32_t np = et->layout->npointers;
    for (size_t i = 0; i < n; i++) {
        jl_value_t **s = (jl_value_t**)(src + i * elsz);
        for (uint32_t j = 0; j < np; j++) {
            jl_value_t *f = s[jl_ptr_offset(et, j)];
            if (f != NULL)
                jl_gc_region_wb(parent, f);
        }
    }
}

#endif // WITH_GC_REGION_BARRIER

// --- the hooks of the allocator and the finalizer path -------------------------------

int jl_gc_region_track_malloced(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned) JL_NOTSAFEPOINT
{
    int cr = ptls->gc_tls.heap.current_region;
    if (__likely(cr == 0))
        return 0;
    small_arraylist_push(&ptls->gc_tls.heap.regions[cr]->mallocarrays,
                         (void*)(((uintptr_t)m) | !!isaligned));
    return 1;
}

// A finalizer of a region object goes to the list of its region, which the
// reset and the census run; the stock sweep never reaches a region page. A
// registration from another thread is an escape of the object: the region
// is quarantined, and the finalizer goes to the list of the registering
// thread, which the stock collector runs once the hand-over has made the
// object its own.
int jl_gc_region_add_finalizer(jl_ptls_t ptls, void *v, void *f) JL_NOTSAFEPOINT
{
    if ((uintptr_t)v & 2)
        return 0;
    jl_value_t *obj = (jl_value_t*)(((uintptr_t)v) & ~(uintptr_t)3);
    jl_gc_pagemeta_t *pm = page_metadata((char*)obj);
    int r = pm ? pm->region_n : 0;
    if (__likely(r == 0))
        return 0;
    if (__unlikely(pm->thread_n != ptls->tid)) {
        uint64_t bit = (uint64_t)1 << r;
        uint64_t seen = jl_atomic_fetch_or_relaxed(&region_quarantined_mask, bit);
        if (!(seen & bit))
            jl_safe_printf("REGION-ESCAPE: a finalizer of a %s of region %d of thread "
                           "%d was registered from thread %d; region %d is quarantined "
                           "until the next collection hands its pages to the stock "
                           "collector\n", jl_typeof_str(obj), r, (int)pm->thread_n,
                           (int)ptls->tid, r);
        return 0;
    }
    arraylist_t *lst = &ptls->gc_tls.heap.regions[r]->finalizers;
    arraylist_push(lst, v);
    arraylist_push(lst, f);
    return 1;
}

// Move a list into a fresh one, so that a finalizer that registers a
// finalizer finds a consistent list.
static void region_take_list(arraylist_t *dst, arraylist_t *src) JL_NOTSAFEPOINT
{
    memcpy(dst, src, sizeof(arraylist_t));
    if (src->items == src->_space)
        dst->items = dst->_space;
    arraylist_new(src, 0);
}

// Run a taken list and free it; gc-common.c parks the window and raises
// finalizer_depth for the run.
static void region_run_finalizer_list(jl_task_t *ct, arraylist_t *list) JL_CANSAFEPOINT
{
    if (list->len != 0)
        jl_gc_run_finalizer_list(ct, list);
    arraylist_free(list);
}

// Free the malloc'd data of a region's memories: all of it at a reset,
// only the dead at a census (the marks are still set then).
static void region_free_malloced(small_arraylist_t *lst, int only_unmarked) JL_NOTSAFEPOINT
{
    size_t n = 0, l = lst->len;
    void **items = lst->items;
    while (n < l) {
        jl_genericmemory_t *m = (jl_genericmemory_t*)((uintptr_t)items[n] & ~(uintptr_t)1);
        if (only_unmarked && gc_marked(jl_astaggedvalue(m)->bits.gc)) {
            n++;
            continue;
        }
        int isaligned = (uintptr_t)items[n] & 1;
        gc_region_free_memory(m, isaligned);
        l--;
        items[n] = items[l];
    }
    lst->len = l;
}

#ifdef __cplusplus
}
#endif
