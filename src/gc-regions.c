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

// --- the brackets around a stock collection ----------------------------------------
// A stock collection runs with region 0 installed on every thread: the
// brackets park the windows and install them again. After each pass the
// marks the pass left on region pages are cleared: the mark walks region
// objects like any other, the clear keeps the bits clean for the census,
// and a second pass of a full collection must traverse them again.

// The hand-over of a quarantined region to the stock collector, on one heap,
// with the world stopped: the pages lose their tag, and the sweep of this
// collection treats them like any other page. The cells past a bump cursor
// and the wholly dead fresh pages get zero headers, so that the sweep reads
// them as free. The finalizers and the malloc'd memories go to the lists of
// the thread. The region state stays allocated and empty, so a window still
// open on the region allocates into new pages.
static void region_handover_heap(jl_ptls_t ptls2, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
    jl_gc_region_state_t *rs = heap->regions[n];
    if (rs == NULL)
        return;
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++) {
        char *bump = (char*)rs->pools[i].newpages;
        if (bump != NULL) {
            jl_gc_pagemeta_t *pg = page_metadata(gc_page_data(bump - 1));
            size_t osize = pg->osize;
            char *end = pg->data + GC_PAGE_OFFSET + ((GC_PAGE_SZ - GC_PAGE_OFFSET) / osize) * osize;
            for (char *c = bump; c < end; c += osize)
                ((jl_taggedvalue_t*)c)->header = 0;
        }
        rs->pools[i].freelist = NULL;
        rs->pools[i].newpages = NULL;
    }
    for (size_t k = 0; k < rs->finalizers.len; k++)
        arraylist_push(&ptls2->finalizers, rs->finalizers.items[k]);
    rs->finalizers.len = 0;
    for (size_t k = 0; k < rs->mallocarrays.len; k++)
        small_arraylist_push(&ptls2->gc_tls_common.heap.mallocarrays, rs->mallocarrays.items[k]);
    rs->mallocarrays.len = 0;
    jl_gc_pagemeta_t *next;
    for (jl_gc_pagemeta_t *pg = rs->pages; pg != NULL; pg = next) {
        next = pg->region_next;
        pg->region_next = NULL;
        pg->region_n = 0;
    }
    for (jl_gc_pagemeta_t *pg = rs->fresh_pages; pg != NULL; pg = next) {
        next = pg->region_next;
        pg->region_next = NULL;
        pg->region_n = 0;
        memset(pg->data + GC_PAGE_OFFSET, 0, GC_PAGE_SZ - GC_PAGE_OFFSET);
    }
    rs->pages = rs->pages_tail = rs->fresh_pages = NULL;
    rs->n_pages = rs->n_fresh = 0;
}

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
    uint64_t q = jl_atomic_load_relaxed(&region_quarantined_mask);
    if (__unlikely(q != 0)) {
        for (int n = 1; n < JL_GC_MAX_REGIONS; n++) {
            if (!((q >> n) & 1))
                continue;
            for (int t_i = 0; t_i < gc_n_threads; t_i++) {
                if (gc_all_tls_states[t_i] != NULL)
                    region_handover_heap(gc_all_tls_states[t_i], n);
            }
        }
        jl_atomic_fetch_and_relaxed(&region_quarantined_mask, ~q);
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
// lists.
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

// Point the allocator at a pool array. The JIT passes the offset of a pool
// of norm_pools in the TLS; pool_base + offset is the same pool of `pools`.
STATIC_INLINE void region_use_pools(jl_thread_heap_t *heap, jl_gc_pool_t *pools) JL_NOTSAFEPOINT
{
    heap->active_pools = pools;
    heap->pool_base = (char*)pools - offsetof(jl_tls_states_t, gc_tls.heap.norm_pools);
}

// The state of a region on a heap, allocated at the first use of the region
// on that heap and never freed; zeroed memory is the empty state of all but
// the pool sizes and the two lists.
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

// A region is live from its first window after a reset; its parent gains a
// live child.
static void region_mark_live(jl_thread_heap_t *heap, int n) JL_NOTSAFEPOINT
{
    if (n == 0 || (heap->region_live_mask & ((uint64_t)1 << n)))
        return;
    heap->region_live_mask |= (uint64_t)1 << n;
    int p = region_parent[n];
    if (heap->region_child_count[p]++ == 0)
        heap->region_haschild_mask |= (uint64_t)1 << p;
}

// A reset ends the life of a region; its parent loses a live child.
static void region_mark_empty(jl_thread_heap_t *heap, int n) JL_NOTSAFEPOINT
{
    if (n == 0 || !(heap->region_live_mask & ((uint64_t)1 << n)))
        return;
    heap->region_live_mask &= ~((uint64_t)1 << n);
    int p = region_parent[n];
    if (--heap->region_child_count[p] == 0)
        heap->region_haschild_mask &= ~((uint64_t)1 << p);
}

// Open a window on region n, or close it (n = 0): the switch is one pointer
// store, and the inlined allocation path is untouched. Returns the region
// that was current; EINVAL for a bad number, EBUSY while finalizers run on
// this thread.
JL_DLLEXPORT int jl_gc_region_set(int n) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_thread_heap_t *heap = &ct->ptls->gc_tls.heap;
    int old = heap->current_region;
    if (n < 0 || n >= JL_GC_MAX_REGIONS)
        return JL_GC_REGION_EINVAL;
    if (n == old)
        return old;
    if (n != 0 && heap->finalizer_depth != 0)
        return JL_GC_REGION_EBUSY;
    // A quarantined region frees nothing again; a window on it would fill
    // memory that nothing reclaims.
    if (__unlikely(n != 0 && jl_gc_region_quarantined(n)))
        return JL_GC_REGION_EQUARANTINED;
#ifdef WITH_GC_REGION_BARRIER
    if (__unlikely(!jl_atomic_load_relaxed(&jl_gc_region_barrier_on)))
        jl_atomic_store_release(&jl_gc_region_barrier_on, 1);
#endif
    region_lazy_init(heap, n);
    region_mark_live(heap, n);
    // An open window pins the task to the thread that holds the region's
    // pages; the close restores the stickiness.
    if (old == 0 && n != 0) {
        jl_atomic_fetch_add_relaxed(&region_windows_open, 1);
        ct->sticky_before_region = ct->sticky;
        ct->sticky = 1;
    }
    else if (n == 0 && old != 0) {
        jl_atomic_fetch_add_relaxed(&region_windows_open, -1);
        ct->sticky = ct->sticky_before_region;
    }
    region_use_pools(heap, (n == 0) ? heap->norm_pools : heap->regions[n]->pools);
    heap->current_region = (uint8_t)n;
    return old;
}

// The region of the open window on the calling thread, 0 when none is open.
JL_DLLEXPORT int jl_gc_region_current(void) JL_NOTSAFEPOINT
{
    return jl_current_task->ptls->gc_tls.heap.current_region;
}

// Install the parked region of a task at a task switch; the window count
// is untouched.
void jl_gc_region_install_task(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    region_lazy_init(heap, n);
    region_use_pools(heap, (n == 0) ? heap->norm_pools : heap->regions[n]->pools);
    heap->current_region = (uint8_t)n;
}

// Install a borrowed region on this thread. A borrow can bring a region to
// a heap that never opened a window on it; the region becomes live there,
// so its parent cannot be reset while the borrowed buffer lives.
void jl_gc_region_install_borrow(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    region_lazy_init(heap, n);
    region_mark_live(heap, n);
    region_use_pools(heap, (n == 0) ? heap->norm_pools : heap->regions[n]->pools);
    heap->current_region = (uint8_t)n;
}

// A borrow brackets one allocation and changes no window state; a task must
// not switch inside one.
JL_DLLEXPORT int jl_gc_region_borrow(int n) JL_NOTSAFEPOINT
{
    if (n < 0 || n >= JL_GC_MAX_REGIONS)
        return JL_GC_REGION_EINVAL;
    jl_ptls_t ptls = jl_current_task->ptls;
    int lent = ptls->gc_tls.heap.current_region;
    if (lent != n)
        jl_gc_region_install_borrow(ptls, n);
    return lent;
}

JL_DLLEXPORT void jl_gc_region_unborrow(int lent) JL_NOTSAFEPOINT
{
    if (lent >= 0 && lent != jl_current_task->ptls->gc_tls.heap.current_region)
        jl_gc_region_install_task(jl_current_task->ptls, lent);
}

// The borrow of region 0, for the runtime's own allocations on behalf of a
// task that holds a window.
JL_DLLEXPORT int jl_gc_region_suspend(void) JL_NOTSAFEPOINT
{
    return jl_gc_region_borrow(0);
}

JL_DLLEXPORT void jl_gc_region_resume(int parked) JL_NOTSAFEPOINT
{
    jl_gc_region_unborrow(parked);
}

// A region-0 zone closes the window and opens it again after, so the task
// may switch inside; a refusal of the reopen leaves the window closed.
JL_DLLEXPORT int jl_gc_region_zone_enter(void) JL_NOTSAFEPOINT
{
    return jl_gc_region_set(0);
}

JL_DLLEXPORT void jl_gc_region_zone_leave(int saved) JL_NOTSAFEPOINT
{
    if (saved > 0)
        jl_gc_region_set(saved);
}

// Close the window of a task that ends (jl_finish_task); only a close lowers
// the process-wide count of open windows.
void jl_gc_region_close_window(jl_task_t *ct) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ct->ptls->gc_tls.heap;
    if (__likely(heap->current_region == 0))
        return;
    jl_atomic_fetch_add_relaxed(&region_windows_open, -1);
    ct->sticky = ct->sticky_before_region;
    ct->region = 0;
    jl_gc_region_install_task(ct->ptls, 0);
}

// --- reset ---------------------------------------------------------------------------

// The root scan shared with the debug entries below; the caller stopped the
// world.
static int64_t region_root_scan(jl_ptls_t ptls, jl_thread_heap_t *heap, int n) JL_CANSAFEPOINT;
static int64_t region_root_scan_global(jl_ptls_t ptls, int n);

// The finalizer phase of a reset runs Julia code, before the free and never
// with the world stopped. A finalizer can register a finalizer on another
// object of the region, so the phase repeats until the list stays empty;
// the bound turns an endless registration into EFINALIZERS.
#define REGION_FINALIZER_ROUNDS 64
static int region_reset_finalizers(jl_task_t *ct, jl_thread_heap_t *heap, int n) JL_CANSAFEPOINT
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

// The free: no Julia code runs here, so the caller may hold the world
// stopped through it. The malloc'd data is freed, the cursors are cleared,
// and the page chain is parked on the fresh list in O(1).
static uint64_t region_reset_heap(jl_thread_heap_t *heap, int n)
{
    jl_gc_region_state_t *rs = heap->regions[n];
    if (rs == NULL)
        return 0;
    assert(rs->finalizers.len == 0);
    region_free_malloced(&rs->mallocarrays, 0);
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
    region_mark_empty(heap, n);         // the parent may now be resettable
    return pages;
}

// The body of both reset entries. The phases are ordered so that each one
// sees the result of the one before:
//
// 1. The preconditions.
// 2. The finalizers of the region, which run Julia code. A finalizer can
//    store one of its own objects into an older region, which quarantines
//    this region, so nothing may be freed before they have all run.
// 3. The quarantine, read again. A reset that freed after step 2 condemned
//    the region would leave the published reference dangling.
// 4. The root check and the free, in one stop-the-world pause. The barrier
//    sees the heap and not the stack, so this is the only thing that stands
//    between a live local and a freed object. `checked` is 0 for the unsafe
//    entry, which frees with no pause and no scan.
static uint64_t region_reset_body(int n, int checked) JL_CANSAFEPOINT
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
    // A live descendant may hold a legal reference into this region.
    if (__unlikely((heap->region_haschild_mask >> n) & 1))
        return (uint64_t)JL_GC_REGION_ECHILD;

    int pending = region_reset_finalizers(ct, heap, n);
    if (__unlikely(pending != 0))
        return (uint64_t)pending;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return (uint64_t)JL_GC_REGION_EQUARANTINED;

    if (!checked)
        return region_reset_heap(heap, n);

    // Several threads reset their own leaves at once, so a lost safepoint is
    // common: wait for the winner and try again, up to a bound.
    uint32_t saved_disable;
    int8_t old_state;
    int attempt = 0;
    for (;;) {
        saved_disable = jl_atomic_exchange(&jl_gc_disable_counter, 0);
        old_state = jl_atomic_load_relaxed(&ptls->gc_state);
        jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
        if (jl_safepoint_start_gc(ct))
            break;
        jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        jl_safepoint_wait_thread_resume(ct);
        if (++attempt >= 1024)
            return (uint64_t)JL_GC_REGION_ERACE;
    }
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);

    int64_t roots = region_root_scan(ptls, heap, n);
    uint64_t result;
    if (roots != 0) {
        jl_safe_printf("REGION-RESET refused: %lld live references into region %d\n",
                       (long long)roots, n);
        result = (uint64_t)JL_GC_REGION_EROOT;
    }
    else {
        // No finalizer is left, so the pause holds through the free.
        result = region_reset_heap(heap, n);
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    jl_safepoint_wait_thread_resume(ct);
    return result;
}

// Reset region n on the calling thread's heap: run its finalizers, check
// that no execution root references into it, free the malloc'd data, park
// the pages. Returns the pages the region held, 0 for a region never used,
// or a refusal code cast to uint64_t (EINVAL, EBUSY, EQUARANTINED, ECHILD,
// ERACE, EROOT).
JL_DLLEXPORT uint64_t jl_gc_region_reset(int n) JL_CANSAFEPOINT
{
    return region_reset_body(n, 1);
}

// The reset without the root check: a reference from a stack slot, a
// register or a parked task then points into freed memory, and the next
// collection reports CORPSE and aborts.
JL_DLLEXPORT uint64_t jl_gc_region_unsafe_reset(int n) JL_CANSAFEPOINT
{
    return region_reset_body(n, 0);
}

// Reset a region several threads filled, a trunk, as one act on every heap
// with the world stopped: trunk objects on different heaps may reference
// each other. Pending finalizers return EFINALIZERS, because nothing can run
// them with the world stopped; the root check runs on every instance.
// Returns the pages reclaimed, or a refusal code cast to uint64_t.
JL_DLLEXPORT uint64_t jl_gc_region_reset_global(int n) JL_CANSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (!region_valid(n))
        return (uint64_t)JL_GC_REGION_EINVAL;
    if (ptls->gc_tls.heap.current_region != 0 ||
        jl_atomic_load_relaxed(&region_windows_open) != 0)
        return (uint64_t)JL_GC_REGION_EBUSY;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return (uint64_t)JL_GC_REGION_EQUARANTINED;

    uint32_t saved_disable = jl_atomic_exchange(&jl_gc_disable_counter, 0);
    int8_t old_state = jl_atomic_load_relaxed(&ptls->gc_state);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    if (!jl_safepoint_start_gc(ct)) {
        jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        jl_safepoint_wait_thread_resume(ct);
        return (uint64_t)JL_GC_REGION_ERACE;
    }
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);

    // The preconditions on every heap first, so that nothing is freed when
    // one heap refuses.
    uint64_t result = 0;
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
        if (heap->finalizer_depth != 0) {
            result = (uint64_t)JL_GC_REGION_EBUSY;
            break;
        }
        if ((heap->region_haschild_mask >> n) & 1) {
            result = (uint64_t)JL_GC_REGION_ECHILD;
            break;
        }
        if (heap->regions[n] != NULL && heap->regions[n]->finalizers.len != 0) {
            result = (uint64_t)JL_GC_REGION_EFINALIZERS;
            break;
        }
    }
    if (result == 0) {
        // The root check over every instance.
        int64_t roots = region_root_scan_global(ptls, n);
        if (roots != 0) {
            jl_safe_printf("REGION-RESET refused: %lld live references into region %d\n",
                           (long long)roots, n);
            result = (uint64_t)JL_GC_REGION_EROOT;
        }
    }
    if (result == 0) {
        for (int t_i = 0; t_i < gc_n_threads; t_i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[t_i];
            if (ptls2 != NULL)
                result += region_reset_heap(&ptls2->gc_tls.heap, n);
        }
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    jl_safepoint_wait_thread_resume(ct);
    return result;
}

#ifdef __cplusplus
}
#endif
