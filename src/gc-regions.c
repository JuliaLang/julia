// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// A thread allocates into region n while a window on n is open, and a reset
// frees the whole region without a trace. The stock collector marks region
// objects like any other and never sweeps a region page. The rules an
// application must keep, and why they make the entries below sound, are in
// doc/src/devdocs/gc-regions.md.
//
// The state lives in three places: the per-heap region table in
// jl_thread_heap_t (gc-tls-stock.h), the page tag region_n in
// jl_gc_pagemeta_t (gc-stock.h), and the process-wide barrier state in this
// file. The hooks in the allocator, the sweep and the finalizer path are in
// gc-stock.c and gc-common.c; each one calls into this file through
// gc-regions.h.

#include "gc-common.h"
#include "gc-stock.h"
#include "gc-regions.h"

#ifdef __cplusplus
extern "C" {
#endif

// --- process-wide state ------------------------------------------------------

// The escape barrier. Armed at the first window; disarmed it costs every
// pointer store one well-predicted load-and-branch. Armed, the lowered write
// barrier calls jl_gc_region_wb, which compares the two page tags: a store
// whose child is younger than its parent breaks the reference rule, and the
// child's region is quarantined - its reset refuses from then on, so an
// escape costs memory, never a dangling pointer.
JL_DLLEXPORT _Atomic(uint8_t) jl_gc_region_barrier_on = 0;
static _Atomic(uint64_t) region_quarantined_mask = 0;

STATIC_INLINE int region_valid(int n) JL_NOTSAFEPOINT
{
    return n > 0 && n < JL_GC_MAX_REGIONS;
}

// 1 when an escape quarantined region n, 0 otherwise (a bad region number
// included). The quarantine is process-wide and permanent.
JL_DLLEXPORT int jl_gc_region_quarantined(int n)
{
    if (!region_valid(n))
        return 0;
    return (jl_atomic_load_relaxed(&region_quarantined_mask) >> n) & 1;
}

// --- the escape barrier ----------------------------------------------------------

// The test of the barrier: 1 when a store of `child` into `parent` breaks
// the reference rule, with the two regions in `cr` and `pr` for the report.
STATIC_INLINE int region_store_escapes(const void *parent, const void *child, int *cr, int *pr) JL_NOTSAFEPOINT
{
    // Child first: a region-0 child is legal under any parent, and almost
    // every store in ordinary code has one, so the common case pays one
    // page-map walk, not two.
    jl_gc_pagemeta_t *cm = page_metadata((char*)child);
    *cr = cm ? cm->region_n : 0;
    if (__likely(*cr == 0))
        return 0;
    jl_gc_pagemeta_t *pm = page_metadata((char*)parent);
    *pr = pm ? pm->region_n : 0;
    // Legal iff the child's region is the parent's own or an older one: the
    // regions are a chain of lifetimes, 0 <- 1 <- 2 <- ..., and a store
    // toward the root of the chain is exactly cr <= pr.
    return *cr > *pr;
}

// The test alone, with no quarantine. The bulk barriers of gc-wb-stock.h ask
// it about a whole container before they look at the elements one by one.
JL_DLLEXPORT int jl_gc_region_would_escape(const void *parent, const void *child) JL_NOTSAFEPOINT
{
    int cr = 0, pr = 0;
    return region_store_escapes(parent, child, &cr, &pr);
}

JL_DLLEXPORT void jl_gc_region_wb(const void *parent, const void *child) JL_NOTSAFEPOINT
{
    int cr = 0, pr = 0;
    if (__likely(!region_store_escapes(parent, child, &cr, &pr)))
        return;
    uint64_t bit = (uint64_t)1 << cr;
    uint64_t seen = jl_atomic_fetch_or_relaxed(&region_quarantined_mask, bit);
    if (!(seen & bit))
        jl_safe_printf("REGION-ESCAPE: a %s of region %d was stored into a %s "
                       "of region %d; region %d is quarantined - its reset now "
                       "refuses, and its memory is retained\n",
                       jl_typeof_str((jl_value_t*)child), cr,
                       jl_typeof_str((jl_value_t*)parent), pr, cr);
}

// The elements of a bulk copy, one by one, when the pair check of the
// containers fails. The source container is a proxy for its elements: a
// young container of old elements -- the result of a filter or a copy made
// inside a window, appended to an old vector after the window closed --
// fails the pair check and is legal. Only a real escape quarantines here.
// `n` boxed elements from `src`:
JL_DLLEXPORT void jl_gc_region_wb_boxed(const void *parent, _Atomic(void*) *src, size_t n) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < n; i++) {
        void *val = jl_atomic_load_relaxed(src + i);
        if (val != NULL)
            jl_gc_region_wb(parent, val);
    }
}

// `n` inline elements of type `et`, `elsz` bytes apart, from `src`: every
// pointer field of every element. One immutable object stored inline is the
// case n = 1 (jl_gc_multi_wb).
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

// A finalizer on a region object goes to the region's list, never to the
// thread list the stock collector sweeps: the region's pages are not swept,
// so the stock collector could never schedule it. The list holds the same
// (tagged object, function) pairs as the thread list; a quiescent entry
// (tag 2) names no object and stays on the thread list. A cross-thread
// registration on a region object is an error of the program, not a
// runtime condition: it throws here, before the caller takes the finalizer
// lock and before any list changes.
int jl_gc_region_add_finalizer(jl_ptls_t ptls, void *v, void *f)
{
    if ((uintptr_t)v & 2)
        return 0;
    jl_value_t *obj = (jl_value_t*)(((uintptr_t)v) & ~(uintptr_t)3);
    jl_gc_pagemeta_t *pm = page_metadata((char*)obj);
    int r = pm ? pm->region_n : 0;
    if (__likely(r == 0))
        return 0;
    if (__unlikely(pm->thread_n != ptls->tid))
        jl_errorf("finalizer: the object lives in region %d of another "
                  "thread; cross-thread registration on a region object "
                  "is not supported", r);
    arraylist_t *lst = &ptls->gc_tls.heap.regions[r]->finalizers;
    arraylist_push(lst, v);
    arraylist_push(lst, f);
    return 1;
}

// Move a whole list into a fresh one, so a finalizer that registers a new
// finalizer sees a consistent region list while the old entries run.
static void region_take_list(arraylist_t *dst, arraylist_t *src) JL_NOTSAFEPOINT
{
    memcpy(dst, src, sizeof(arraylist_t));
    if (src->items == src->_space)
        dst->items = dst->_space;
    arraylist_new(src, 0);
}

// Run the pairs of a taken list and free it. The finalizer runner parks the
// window and raises finalizer_depth (gc-common.c), so the finalizers
// allocate in region 0 and no region entry runs until they return.
static void region_run_finalizer_list(jl_task_t *ct, arraylist_t *list)
{
    if (list->len != 0)
        jl_gc_run_finalizer_list(ct, list);
    arraylist_free(list);
}

// Free the malloc'd data of a region's memories at a reset.
static void region_free_malloced(small_arraylist_t *lst) JL_NOTSAFEPOINT
{
    size_t n = 0, l = lst->len;
    void **items = lst->items;
    while (n < l) {
        jl_genericmemory_t *m = (jl_genericmemory_t*)((uintptr_t)items[n] & ~(uintptr_t)1);
        int isaligned = (uintptr_t)items[n] & 1;
        jl_gc_free_memory(m, isaligned);
        l--;
        items[n] = items[l];
    }
    lst->len = l;
}

// --- the brackets around a stock collection ----------------------------------------
// A stock collection coexists with live regions by two brackets around it
// and a clear after every pass. Before: every thread's window is parked and
// region 0 installed, so the sweep prologue's cursor sync sees norm_pools
// everywhere. After each pass: every region page the mark touched
// (has_marked is the card) gets its cells' low header bits cleared - the
// mark walked region objects normally, which keeps liveness exact through
// them, and the clear keeps the bits clean for the next pass; a freelist link
// survives the blind clear because an aligned pointer carries zero low bits.
// Region pages are never swept and region objects never grow old, so they
// never enter a remembered set. The clear runs after every pass, not once
// per collection: a forced full collection runs a second, young pass, and a
// region object still marked from the first pass would not be traversed
// again, so its region-0 children would be swept from under it. After the
// last pass: the parked windows are installed again.

void jl_gc_region_prepare_stock_collection(void) JL_NOTSAFEPOINT
{
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
        heap->saved_region = heap->current_region;
        if (heap->current_region != 0)
            jl_gc_region_install_task(ptls2, 0);
    }
}

void jl_gc_region_clear_stock_marks(void) JL_NOTSAFEPOINT
{
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
        for (int n = 1; n < JL_GC_MAX_REGIONS; n++) {
            if (heap->regions[n] == NULL)
                continue;
            for (jl_gc_pagemeta_t *pg = heap->regions[n]->pages; pg != NULL; pg = pg->region_next) {
                if (!pg->has_marked)
                    continue;
                int osize = pg->osize;
                char *cell = pg->data + GC_PAGE_OFFSET;
                char *end = pg->data + GC_PAGE_SZ;
                for (; cell + osize <= end; cell += osize)
                    ((jl_taggedvalue_t*)cell)->header &= ~(uintptr_t)(GC_MARKED | GC_OLD);
                pg->has_marked = 0;
                pg->has_young = 0;
                pg->nold = 0;
                pg->prev_nold = 0;
            }
        }
    }
}

void jl_gc_region_finish_stock_collection(void) JL_NOTSAFEPOINT
{
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
        if (heap->saved_region != 0)
            jl_gc_region_install_task(ptls2, heap->saved_region);
    }
}

// The region finalizer lists are roots of the stock mark, like the thread
// lists: a finalizer function that only the list references must survive
// until the reset or the census runs it. Called in the finalizer phase of
// the stock mark, before the queue drains.
void jl_gc_region_mark_finalizer_lists(jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT
{
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
        for (int n = 1; n < JL_GC_MAX_REGIONS; n++)
            if (heap->regions[n] != NULL)
                gc_mark_finlist(mq, &heap->regions[n]->finalizers, 0);
    }
}

// --- windows -----------------------------------------------------------------------

// The state of a region on a heap is made on the heap's first use of the
// region: a window, a task switch that installs one, or a borrow. Zeroed
// memory is the empty state of everything but the pool sizes and the two
// lists. calloc_s aborts when memory runs out, as the runtime does for its
// own metadata. The state is never freed: a reset parks the pages for the
// next window on the region, and the chains live here.
static void region_lazy_init(jl_thread_heap_t *heap, int n) JL_NOTSAFEPOINT
{
    if (n == 0 || heap->regions[n] != NULL)
        return;
    jl_gc_region_state_t *rs = (jl_gc_region_state_t*)calloc_s(sizeof(jl_gc_region_state_t));
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++)
        rs->pools[i].osize = heap->norm_pools[i].osize;
    small_arraylist_new(&rs->mallocarrays, 0);
    arraylist_new(&rs->finalizers, 0);
    heap->regions[n] = rs;
}

// Open a window on region n, or close it (n = 0). Every region's cursors
// live in its own array, so the switch is one pointer store; the inlined
// allocation fast path is untouched. Returns the region that was current;
// EINVAL for a bad region number, EBUSY while finalizers run on this thread.
JL_DLLEXPORT int jl_gc_region_set(int n)
{
#ifdef JL_NO_REGION_ALLOC
    // The stock-only build allocates through norm_pools only; a window
    // would allocate into the wrong pools, so the entry refuses.
    (void)n;
    return JL_GC_REGION_EINVAL;
#else
    jl_task_t *ct = jl_current_task;
    jl_thread_heap_t *heap = &ct->ptls->gc_tls.heap;
    int old = heap->current_region;
    if (n < 0 || n >= JL_GC_MAX_REGIONS)
        return JL_GC_REGION_EINVAL;
    if (n == old)
        return old;
    if (n != 0 && heap->finalizer_depth != 0)
        return JL_GC_REGION_EBUSY;
    // A quarantined region frees nothing ever again: its reset and its
    // census refuse, and the stock collector never sweeps a region page. A
    // window on it would fill memory that nothing can reclaim, so the
    // program stops here instead of at its memory limit.
    if (__unlikely(n != 0 && jl_gc_region_quarantined(n)))
        return JL_GC_REGION_EQUARANTINED;
    if (__unlikely(!jl_atomic_load_relaxed(&jl_gc_region_barrier_on)))
        jl_atomic_store_release(&jl_gc_region_barrier_on, 1);
    region_lazy_init(heap, n);
    heap->active_pools = (n == 0) ? heap->norm_pools : heap->regions[n]->pools;
    heap->current_region = (uint8_t)n;
    return old;
#endif
}

// The region of the open window on the calling thread, 0 when none is open.
JL_DLLEXPORT int jl_gc_region_current(void)
{
    return jl_current_task->ptls->gc_tls.heap.current_region;
}

// Install a parked region on this thread: the stock collection parks every
// window before it runs and installs it again after.
void jl_gc_region_install_task(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    region_lazy_init(heap, n);
    heap->active_pools = (n == 0) ? heap->norm_pools : heap->regions[n]->pools;
    heap->current_region = (uint8_t)n;
}

// Install a borrowed region on this thread (jl_gc_region_borrow in
// gc-common.c).
void jl_gc_region_install_borrow(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    region_lazy_init(heap, n);
    heap->active_pools = (n == 0) ? heap->norm_pools : heap->regions[n]->pools;
    heap->current_region = (uint8_t)n;
}

// --- reset ---------------------------------------------------------------------------

// The per-heap reset body. The caller owns the preconditions. Everything in
// the region dies: its finalizers run first, on whole objects; then the
// malloc'd data of its memories is freed; the headers die with the pages.
// The reset walks nothing: every page hangs on one chain with a tail, and a
// fresh page's metadata is allowed to be stale because gc_add_page resets a
// page when it claims it. So the pool cursors are cleared, the chain is
// parked on the fresh list in O(1), and the page count comes from the
// counters the claim path keeps.
// The finalizer phase of a reset, on its own because it runs Julia code: a
// finalizer allocates, stores, and can quarantine the region it belongs to.
// It runs before the free, and never with the world stopped. A finalizer can
// register a finalizer on another object of the region, so the phase takes
// the list again until it stays empty; the bound turns a finalizer that
// registers one every round into a refusal instead of a hang. Returns 0
// with the list empty, EFINALIZERS otherwise.
#define REGION_FINALIZER_ROUNDS 64
static int region_reset_finalizers(jl_task_t *ct, jl_thread_heap_t *heap, int n)
{
    if (heap->regions[n] == NULL)
        return 0;
    for (int round = 0; round < REGION_FINALIZER_ROUNDS; round++) {
        if (heap->regions[n]->finalizers.len == 0)
            return 0;
        arraylist_t run;
        region_take_list(&run, &heap->regions[n]->finalizers);
        region_run_finalizer_list(ct, &run);
    }
    return heap->regions[n]->finalizers.len == 0 ? 0 : JL_GC_REGION_EFINALIZERS;
}

// The free. The caller drained the finalizer list, or refused: no Julia code
// runs here, so the caller may hold the world stopped through it.
static uint64_t region_reset_heap(jl_thread_heap_t *heap, int n)
{
    jl_gc_region_state_t *rs = heap->regions[n];
    if (rs == NULL)
        return 0;
    assert(rs->finalizers.len == 0);
    region_free_malloced(&rs->mallocarrays);
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++) {
        rs->pools[i].freelist = NULL;
        rs->pools[i].newpages = NULL;
    }
    uint64_t pages = (uint64_t)rs->n_pages + rs->n_fresh;
    jl_gc_pagemeta_t *head = rs->pages;
    if (head != NULL) {
        rs->pages_tail->region_next = rs->fresh_pages;
        rs->fresh_pages = head;
        rs->pages = NULL;
        rs->pages_tail = NULL;
        rs->n_fresh += rs->n_pages;
        rs->n_pages = 0;
    }
    return pages;
}

// The body of the reset. The phases are ordered so that each one sees the
// result of the one before:
//
// 1. The preconditions.
// 2. The finalizers of the region, which run Julia code. A finalizer can
//    store one of its own objects into an older region, which quarantines
//    this region, so nothing may be freed before they have all run.
// 3. The quarantine, read again. A reset that freed after step 2 condemned
//    the region would leave the published reference dangling.
// 4. The free.
static uint64_t region_reset_body(int n, int checked)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    if (!region_valid(n))
        return (uint64_t)JL_GC_REGION_EINVAL;
    if (n == heap->current_region || heap->finalizer_depth != 0)
        return (uint64_t)JL_GC_REGION_EBUSY;
    if (heap->regions[n] == NULL)
        return 0;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return (uint64_t)JL_GC_REGION_EQUARANTINED;

    int pending = region_reset_finalizers(ct, heap, n);
    if (__unlikely(pending != 0))
        return (uint64_t)pending;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return (uint64_t)JL_GC_REGION_EQUARANTINED;
    (void)checked;      // the root check and its pause arrive with the debug entries
    return region_reset_heap(heap, n);
}

// Reset region n on the calling thread's heap: run its finalizers, free the
// malloc'd data of its memories, and park its pages for reuse. A region
// another thread filled is reset on that thread.
// Returns the pages the region held (fresh pages included), 0 for a region
// never used, or a refusal code cast to uint64_t: EINVAL for a bad number,
// EBUSY while the region is current or finalizers run on this thread,
// EQUARANTINED after an escape.
JL_DLLEXPORT uint64_t jl_gc_region_reset(int n)
{
    return region_reset_body(n, 1);
}

// The region of an object, read from its page tag in constant time. NULL
// metadata means the object is not a pool object (big, malloc'd, permanent,
// or foreign); those all belong to region 0.
JL_DLLEXPORT int jl_gc_region_of(jl_value_t *v)
{
    jl_gc_pagemeta_t *meta = page_metadata((char*)jl_astaggedvalue(v));
    if (meta == NULL)
        return 0;
    return (int)meta->region_n;
}

// --- initialization ----------------------------------------------------------------------

void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT
{
    heap->current_region = 0;
    heap->saved_region = 0;
    heap->finalizer_depth = 0;
    heap->active_pools = heap->norm_pools;
    memset(heap->regions, 0, sizeof(heap->regions)); // no region has state yet
}

#ifdef __cplusplus
}
#endif
