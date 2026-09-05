// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// A thread allocates into region n while a window on n is open, and a reset
// frees the whole region without a trace. The stock collector marks region
// objects like any other and never sweeps a region page; the census below
// collects one region alone. The rules an application must keep, and why
// they make the entries below sound, are in doc/src/devdocs/gc-regions.md.
//
// The state lives in three places: the per-heap region table in
// jl_thread_heap_t (gc-tls-stock.h), the page tag region_n in
// jl_gc_pagemeta_t (gc-stock.h), and the process-wide tree and masks in
// this file. The hooks in the allocator, the mark loop, the sweep and the
// finalizer path are in gc-stock.c and gc-common.c; each one calls into this
// file through gc-regions.h.

#include "gc-common.h"
#include "gc-stock.h"
#include "gc-regions.h"

#ifdef __cplusplus
extern "C" {
#endif

// --- process-wide state ------------------------------------------------------

// How many windows are open across every thread. A parked task keeps its
// window, so the count is the number of tasks in a window. The stop-the-world
// census, the global reset and the debug check refuse while any window is
// open; the stock collection parks every open window instead (see the
// brackets below).
static _Atomic(int) region_windows_open = 0;

// The escape barrier. Armed at the first window; disarmed it costs every
// pointer store one well-predicted load-and-branch. Armed, the lowered write
// barrier calls jl_gc_region_wb, which compares the two page tags: a store
// whose child is younger than its parent breaks the reference rule, and the
// child's region is quarantined - its reset and census refuse from then on,
// so an escape costs memory, never a dangling pointer.
JL_DLLEXPORT _Atomic(uint8_t) jl_gc_region_barrier_on = 0;
static _Atomic(uint64_t) region_quarantined_mask = 0;

// The census filter: the region whose census runs now, 0 otherwise. The mark
// loop reads it once per object array and passes it down, so a stock mark
// pays nothing per slot.
_Atomic(int) jl_gc_region_census_target = 0;

// The tasks the census met outside the region. Their stacks are execution
// roots, so each one is scanned once; the table is the dedup, because a
// task's mark bits are left untouched (a stock collection leaves tasks
// old-marked, and a mark-based claim would never fire).
static htable_t region_census_tasks;
static size_t region_census_task_count = 0;

// The debug mode: a reset refuses while an execution root references into
// the region (jl_gc_region_set_debug).
static int region_debug_checks = 0;

// The phase breakdown of the last census: 0 total ns, 1 stop-the-world ns,
// 2 mark ns, 3 sweep ns, 4 live cells kept, 5 cells freed, 6 pages walked,
// 7 pages freed wholesale.
static _Atomic(uint64_t) region_collect_stats[8];

// Read one field of the breakdown; 0 for an index out of range. The fields
// are those of the last census any thread ran: read them on the thread that
// ran the census, right after it returned.
JL_DLLEXPORT uint64_t jl_gc_region_stat(int i)
{
    return (i >= 0 && i < 8) ? jl_atomic_load_relaxed(&region_collect_stats[i]) : 0;
}

// The page count that triggers a census on the open region from the
// allocator (jl_gc_region_maybe_census in gc-regions.h). 0 = never.
_Atomic(int) jl_gc_region_census_page_threshold = 0;

// Set the threshold, process-wide. It takes effect at the next page a window
// claims on any thread.
JL_DLLEXPORT void jl_gc_region_census_threshold(int pages)
{
    jl_atomic_store_relaxed(&jl_gc_region_census_page_threshold, pages);
}

STATIC_INLINE int region_valid(int n) JL_NOTSAFEPOINT
{
    return n > 0 && n < JL_GC_MAX_REGIONS;
}

// --- the region tree ------------------------------------------------------------
// The regions form a declared tree of lifetimes. region_parent[r] names the
// parent of r (0 = a child of the root region 0); region_uptree[r] is the
// bitset of r itself, its ancestors, and 0 -- exactly the regions a store
// from an object of region r may legally target (its own region or an older
// one on its branch). A store of a child of region cr into a parent of
// region pr is legal iff cr is in region_uptree[pr]: the same region, or an
// ancestor.
//
// The default is the chain 0 <- 1 <- 2 <- ..., a total order:
// region_uptree[r] = {0,1,...,r}, so cr in uptree[pr] is exactly cr <= pr.
// The first declaration replaces the chain by the all-root tree and then
// applies the declared edge.
static uint8_t region_parent[JL_GC_MAX_REGIONS];
static _Atomic(uint64_t) region_uptree[JL_GC_MAX_REGIONS];
static int region_tree_declared = 0;

// Rebuild every uptree from region_parent[]. A parent has a lower number
// than its child, so one pass in index order reads each parent's final
// uptree.
static void region_tree_rebuild(void)
{
    for (int r = 0; r < JL_GC_MAX_REGIONS; r++) {
        uint64_t up = (uint64_t)1 << r;
        if (r != 0)
            up |= jl_atomic_load_relaxed(&region_uptree[region_parent[r]]);
        jl_atomic_store_relaxed(&region_uptree[r], up);
    }
}

// Declare the parent of `child`. parent < child keeps the numbers a
// topological order. The tree is declared before the regions are used: the
// call refuses while any region is live on any heap (the live-child counts
// are kept per edge) or while any window is open.
JL_DLLEXPORT int jl_gc_region_declare_parent(int child, int parent)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (!region_valid(child) || parent < 0 || parent >= child)
        return JL_GC_REGION_EINVAL;
    if (jl_atomic_load_relaxed(&region_windows_open) != 0)
        return JL_GC_REGION_EBUSY;

    // The world stops for the test and the rebuild together. The uptree
    // words change one at a time, so a thread that opened a window
    // between a test and a rebuild done apart could read a half-built tree
    // and judge a store against it. A declaration is a startup act and runs
    // a handful of times, so the pause costs nothing that matters.
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
            return JL_GC_REGION_ERACE;
    }
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);

    int result = 0;
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 != NULL && ptls2->gc_tls.heap.region_live_mask != 0) {
            result = JL_GC_REGION_ECHILD;
            break;
        }
    }
    if (result == 0) {
        if (!region_tree_declared) {
            for (int r = 0; r < JL_GC_MAX_REGIONS; r++)
                region_parent[r] = 0;
            region_tree_declared = 1;
        }
        region_parent[child] = (uint8_t)parent;
        region_tree_rebuild();
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    jl_safepoint_wait_thread_resume(ct);
    return result;
}

// The declared parent of `child`: 0 for a child of the root, and 0 for a
// bad region number.
JL_DLLEXPORT int jl_gc_region_parent_of(int child)
{
    if (!region_valid(child))
        return 0;
    return region_parent[child];
}

// The page count of a region on the calling heap: the observable a census
// bounds.
JL_DLLEXPORT int jl_gc_region_pages(int n)
{
    if (!region_valid(n))
        return 0;
    jl_thread_heap_t *heap = &jl_current_task->ptls->gc_tls.heap;
    jl_gc_region_state_t *rs = heap->regions[n];
    return rs == NULL ? 0 : (int)rs->n_pages;
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
    // Legal iff the child's region is the parent's own or one of its
    // ancestors -- a store toward the root of the branch. In the default
    // chain this is exactly cr <= pr; in a tree it forbids a sibling and a
    // descendant in both directions, which the total order could not.
    return !((jl_atomic_load_relaxed(&region_uptree[*pr]) >> *cr) & 1);
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
                       "of region %d; region %d is quarantined - its reset and "
                       "census now refuse, and its memory is retained\n",
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
// them, and the clear keeps the bits clean for the census; a freelist link
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

// A region becomes live at the first window onto it after a reset (or
// ever); its parent gains a live child. Idempotent through region_live_mask.
static void region_mark_live(jl_thread_heap_t *heap, int n) JL_NOTSAFEPOINT
{
    if (n == 0 || (heap->region_live_mask & ((uint64_t)1 << n)))
        return;
    heap->region_live_mask |= (uint64_t)1 << n;
    int p = region_parent[n];
    if (heap->region_child_count[p]++ == 0)
        heap->region_haschild_mask |= (uint64_t)1 << p;
}

// A region becomes empty at its reset; its parent loses a live child, and
// when the last one goes the parent's haschild bit clears -- the parent is
// resettable again.
static void region_mark_empty(jl_thread_heap_t *heap, int n) JL_NOTSAFEPOINT
{
    if (n == 0 || !(heap->region_live_mask & ((uint64_t)1 << n)))
        return;
    heap->region_live_mask &= ~((uint64_t)1 << n);
    int p = region_parent[n];
    if (--heap->region_child_count[p] == 0)
        heap->region_haschild_mask &= ~((uint64_t)1 << p);
}

// Open a window on region n, or close it (n = 0). Every region's cursors
// live in its own array, so the switch is one pointer store; the inlined
// allocation fast path is untouched. The window belongs to the calling
// task: it follows the task across a task switch, and the task stays on its
// thread while the window is open. Returns the region that was current;
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
    region_mark_live(heap, n);
    // An open window pins the task: a region's pages live in the thread
    // heap, so a task holding a window must not migrate. The stickiness
    // it had is restored when the window closes.
    if (old == 0 && n != 0) {
        jl_atomic_fetch_add_relaxed(&region_windows_open, 1);
        ct->sticky_before_region = ct->sticky;
        ct->sticky = 1;
    }
    else if (n == 0 && old != 0) {
        jl_atomic_fetch_add_relaxed(&region_windows_open, -1);
        ct->sticky = ct->sticky_before_region;
    }
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

// Install a task's parked region on this thread at a task switch. The
// window count is untouched: the window belongs to the task and stays
// open while the task is parked.
void jl_gc_region_install_task(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    region_lazy_init(heap, n);
    heap->active_pools = (n == 0) ? heap->norm_pools : heap->regions[n]->pools;
    heap->current_region = (uint8_t)n;
}

// Install a borrowed region on this thread (jl_gc_region_borrow in
// gc-common.c). A task switch installs a region the task opened a window
// on, so the region is live on this heap already. A borrow can bring a
// region number to a heap that never opened a window on it: the borrow of a
// container that another thread made. The region becomes live on this heap
// here, so its parent refuses a reset while the borrowed buffer lives, and
// a tree declaration refuses while it holds pages.
void jl_gc_region_install_borrow(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    region_lazy_init(heap, n);
    region_mark_live(heap, n);
    heap->active_pools = (n == 0) ? heap->norm_pools : heap->regions[n]->pools;
    heap->current_region = (uint8_t)n;
}

// Close the window of a task that reaches its end, whether it returns or
// throws (jl_finish_task in task.c). The count of open windows is
// process-wide and only a close lowers it, so a task that died holding one
// would refuse every census, every global reset and every declaration for
// the life of the process.
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

// The root scans of the checked reset, the global reset and
// jl_gc_region_check, defined with the debug entries below. The caller has
// stopped the world and set gc_n_threads and gc_all_tls_states.
static int64_t region_root_scan(jl_ptls_t ptls, jl_thread_heap_t *heap, int n);
static int64_t region_root_scan_global(jl_ptls_t ptls, int n);

// The per-heap reset body, shared by the single-heap reset and the global
// reset. The caller owns the preconditions. Everything in the region dies:
// its finalizers run first, on whole objects (the single-heap reset only;
// the global reset refuses a region with pending finalizers, because it
// holds the world stopped); then the malloc'd data of its memories is
// freed; the headers die with the pages. The reset walks nothing: every
// page hangs on one chain with a tail, and a fresh page's metadata is
// allowed to be stale because gc_add_page resets a page when it claims it.
// So the pool cursors are cleared, the chain is parked on the fresh list in
// O(1), and the page count comes from the counters the claim path keeps.
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
    // A region with a live child must not reset: a descendant may hold a
    // legal reference into it (leaf -> trunk), which the reset would dangle.
    if (__unlikely((heap->region_haschild_mask >> n) & 1))
        return (uint64_t)JL_GC_REGION_ECHILD;

    int pending = region_reset_finalizers(ct, heap, n);
    if (__unlikely(pending != 0))
        return (uint64_t)pending;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return (uint64_t)JL_GC_REGION_EQUARANTINED;

    if (!checked)
        return region_reset_heap(heap, n);

    // Several threads reset their own leaves at once in the tree model, so a
    // lost safepoint is the common case, not an error. The loser waits for
    // the winner and tries again: it has work to do that nobody else does.
    // The bound keeps a pathological contention from hanging the caller.
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
        // No finalizer is left to run, so the free needs no Julia code and
        // the pause holds through it.
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
// that no execution root references into it, free the malloc'd data of its
// memories, and park its pages for reuse. A region another thread filled is
// reset on that thread, or by the global reset.
//
// Returns the pages the region held (fresh pages included), 0 for a region
// never used, or a refusal code cast to uint64_t: EINVAL for a bad number,
// EBUSY while the region is current or finalizers run on this thread,
// EQUARANTINED after an escape, ECHILD while a child region is live, ERACE
// when another thread won the safepoint, EROOT when an execution root
// references into the region.
JL_DLLEXPORT uint64_t jl_gc_region_reset(int n)
{
    return region_reset_body(n, 1);
}

// The reset without the root check. It frees whatever the region holds, and
// a reference from a stack slot, a register or a parked task's stack is left
// pointing into freed memory: the next collection reports CORPSE and aborts.
// Use it where a measurement needs the pause of the checked entry gone and
// the program can show that no root survives its window.
JL_DLLEXPORT uint64_t jl_gc_region_unsafe_reset(int n)
{
    return region_reset_body(n, 0);
}

// Reset a region that several threads share: a trunk. Each thread filled
// its own heap instance, and trunk objects on different heaps may
// reference each other (the same region, a legal edge), so one instance
// must not free while another lives - the reset is one act across every
// heap, with the world stopped. The world stays stopped, so no finalizer
// can run: a trunk with pending finalizers is refused; a cooperative
// census on each heap runs them first. The root check of the per-heap
// reset runs here too, once for every instance: a parked task's frame that
// references a trunk object refuses the reset with EROOT. Returns the pages
// reclaimed, or a refusal code cast to uint64_t.
JL_DLLEXPORT uint64_t jl_gc_region_reset_global(int n)
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

    // The world is stopped. First the preconditions on every heap, so the
    // reset frees nothing when one heap refuses.
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
        // The root check, as the per-heap reset runs it: an execution root
        // of any thread that references any heap's instance refuses.
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

// --- the census ------------------------------------------------------------------------
// A census collects one region alone: it marks from the execution roots
// with the census filter (gc_try_claim_and_push in gc-stock.c pushes only
// objects whose page carries the region tag, plus every task it meets, for
// the task's stack), then sweeps only the region's pages. Globals, the
// remembered sets and every other region are never walked: the reference
// rule says they cannot reference into the region. The census is sound only
// under that rule; a violating edge from outside means the object it names
// is freed here, which is what the escape barrier's quarantine prevents.

// The claim of a task outside the region, called by gc_scoped_claim.
int jl_gc_region_census_claim_task(jl_value_t *task) JL_NOTSAFEPOINT
{
    if (ptrhash_has(&region_census_tasks, task))
        return 0;
    ptrhash_put(&region_census_tasks, task, task);
    region_census_task_count++;
    return 1;
}

static void region_census_begin(int n) JL_NOTSAFEPOINT
{
    htable_reset(&region_census_tasks, region_census_task_count);
    region_census_task_count = 0;
    jl_atomic_store_relaxed(&jl_gc_region_census_target, n);
}

static void region_census_end(void) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&jl_gc_region_census_target, 0);
}

// The scoped sweep shared by both census entries. A page the mark never
// touched (has_marked == 0) holds no live cell: it is reset wholesale in
// O(1) and parked on the region's fresh-page list, which gc_add_page reuses
// before claiming new pages. Only pages with survivors get the cell walk.
// The pool freelists are rebuilt from scratch, so wholesale pages cannot
// leave stale entries. Cells at or past a pool's bump cursor stay owned by
// the cursor, and the cursor page is kept, so a census of the open region
// leaves allocation to continue from the rebuilt freelist. Fills stats
// slots 4..7.
static int64_t region_scoped_sweep(jl_thread_heap_t *heap, int n)
{
    int64_t freed = 0;
    uint64_t live = 0, pages_walked = 0, pages_wholesale = 0;
    jl_gc_pool_t *pools = heap->regions[n]->pools;
    char *bump[JL_GC_N_MAX_POOLS];
    jl_taggedvalue_t **fl_tail[JL_GC_N_MAX_POOLS];
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++) {
        bump[i] = (char*)pools[i].newpages;
        pools[i].freelist = NULL;
        fl_tail[i] = &pools[i].freelist;
    }
    // The marks are still set here: free the malloc'd data of the dead
    // memories before the page walk clears the bits.
    region_free_malloced(&heap->regions[n]->mallocarrays, 1);
    jl_gc_pagemeta_t *kept = NULL;
    jl_gc_pagemeta_t *kept_tail = NULL;
    jl_gc_pagemeta_t *pg = heap->regions[n]->pages;
    while (pg != NULL) {
        jl_gc_pagemeta_t *next = pg->region_next;
        int i = pg->pool_n;
        int osize = pg->osize;
        char *cell = pg->data + GC_PAGE_OFFSET;
        size_t ncells = (GC_PAGE_SZ - GC_PAGE_OFFSET) / (size_t)osize;
        char *end = cell + ncells * (size_t)osize;
        int is_cursor = (bump[i] != NULL && gc_page_data(bump[i] - 1) == pg->data);
        if (is_cursor && (char*)bump[i] < end)
            end = (char*)bump[i];
        if (!pg->has_marked && !is_cursor) {
            // Stale metadata is fine on the fresh list; the claim resets it.
            pg->region_next = heap->regions[n]->fresh_pages;
            heap->regions[n]->fresh_pages = pg;
            freed += (int64_t)ncells;
            pages_wholesale++;
            pg = next;
            continue;
        }
        for (; cell < end; cell += osize) {
            jl_taggedvalue_t *tv = (jl_taggedvalue_t*)cell;
            uintptr_t h = tv->header;
            if (h & GC_MARKED) {
                tv->header = h & ~(uintptr_t)(GC_MARKED | GC_OLD);
                live++;
            }
            else {
                tv->next = NULL;
                *fl_tail[i] = tv;
                fl_tail[i] = &tv->next;
                freed++;
            }
        }
        pg->has_marked = 0;
        pg->region_next = kept;
        if (kept == NULL)
            kept_tail = pg;
        kept = pg;
        pages_walked++;
        pg = next;
    }
    heap->regions[n]->pages = kept;
    heap->regions[n]->pages_tail = kept_tail;
    heap->regions[n]->n_pages = (uint32_t)pages_walked;
    heap->regions[n]->n_fresh += (uint32_t)pages_wholesale;
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++)
        *fl_tail[i] = NULL;
    jl_atomic_store_relaxed(&region_collect_stats[4], live);
    jl_atomic_store_relaxed(&region_collect_stats[5], (uint64_t)freed);
    jl_atomic_store_relaxed(&region_collect_stats[6], pages_walked);
    jl_atomic_store_relaxed(&region_collect_stats[7], pages_wholesale);
    return freed;
}

// The mark of a census walks the execution roots of every thread, so it sets
// mark bits on objects of the region that live on other heaps as well. The
// sweep walks the calling heap alone, so those bits would stay set: the next
// census on that heap would read a dead cell as live and keep it, and only a
// stock collection would clear them. A program that runs no stock collection
// is the point of the model, so the census clears what it set elsewhere.
static void region_clear_marks_on_other_heaps(jl_thread_heap_t *mine, int n) JL_NOTSAFEPOINT
{
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        jl_thread_heap_t *heap = &ptls2->gc_tls.heap;
        if (heap == mine || heap->regions[n] == NULL)
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
        }
    }
}

// Whether a child of region n is live on any heap. The caller stopped the
// other threads or knows them parked: the masks are read without a fence.
static int region_child_live_on_any_heap(jl_ptls_t *all, int nthreads, int n) JL_NOTSAFEPOINT
{
    for (int t_i = 0; t_i < nthreads; t_i++) {
        jl_ptls_t ptls2 = all[t_i];
        if (ptls2 != NULL && ((ptls2->gc_tls.heap.region_haschild_mask >> n) & 1))
            return 1;
    }
    return 0;
}

// Split the region's finalizer list: the entries whose object the mark did
// not reach move to `dead`. The finalizer phase of the census then marks
// both lists (the survivors' functions, and the dead pairs for one more
// cycle, the way the stock collector keeps a finalizable object alive until
// its finalizer ran).
static void region_split_dead_finalizers(arraylist_t *lst, arraylist_t *dead) JL_NOTSAFEPOINT
{
    arraylist_new(dead, 0);
    size_t j = 0, len = lst->len;
    void **items = lst->items;
    for (size_t i = 0; i < len; i += 2) {
        jl_value_t *obj = (jl_value_t*)(((uintptr_t)items[i]) & ~(uintptr_t)3);
        if (gc_marked(jl_astaggedvalue(obj)->bits.gc)) {
            items[j] = items[i];
            items[j + 1] = items[i + 1];
            j += 2;
        }
        else {
            arraylist_push(dead, items[i]);
            arraylist_push(dead, items[i + 1]);
        }
    }
    lst->len = j;
}

// The mark of a census, from the execution roots of the threads the
// caller names. The census filter is set by the caller. The scanned-byte
// counters of the marking thread are restored, so a census does not enter
// the stock collector's estimate of the live heap.
//
// The remset of the marking thread is restored as well. The stock task scan
// (the Task branch of gc_mark_outrefs) ends in gc_mark_push_remset, which
// adds an old task to the remset of the marking thread. The census scans
// every task, so every old task lands in the remset, but the census sets no
// mark bit on a task. The next stock collection would then find the task in
// the remset first and scan it as a remset object, which sets no page
// metadata; the later claim from the thread-local roots fails because the
// task is already marked; the page of the task keeps has_marked == 0 and the
// sweep frees the page with the live task in it. The census pushes nothing
// else to the remset: a region cell is never old, and the filter drops an
// out-of-region cell that is not a task. Truncation to the entry length
// removes exactly the pushes of the census.
static void region_census_mark(jl_ptls_t ptls, jl_ptls_t *tls_states, int nthreads,
                               jl_thread_heap_t *heap, int n, arraylist_t *dead)
{
    jl_gc_markqueue_t *mq = &ptls->gc_tls.mark_queue;
    size_t scanned = ptls->gc_tls.gc_cache.scanned_bytes;
    size_t perm_scanned = ptls->gc_tls.gc_cache.perm_scanned_bytes;
    size_t remset_len = ptls->gc_tls.heap.remset.len;
    int remset_nptr = ptls->gc_tls.heap.remset_nptr;
    for (int t_i = 0; t_i < nthreads; t_i++) {
        jl_ptls_t ptls2 = tls_states[t_i];
        if (ptls2 != NULL)
            gc_queue_execution_roots(mq, ptls2);
    }
    gc_mark_loop_serial(ptls);
    if (dead != NULL) {
        region_split_dead_finalizers(&heap->regions[n]->finalizers, dead);
        gc_mark_finlist(mq, &heap->regions[n]->finalizers, 0);
        gc_mark_finlist(mq, dead, 0);
        gc_mark_loop_serial(ptls);
    }
    ptls->gc_tls.gc_cache.scanned_bytes = scanned;
    ptls->gc_tls.gc_cache.perm_scanned_bytes = perm_scanned;
    assert(ptls->gc_tls.heap.remset.len >= remset_len);
    ptls->gc_tls.heap.remset.len = remset_len;
    ptls->gc_tls.heap.remset_nptr = remset_nptr;
}

// The stop-the-world census on region n, from the execution roots of every
// thread. The caller owns the preconditions. Two callers: jl_gc_region_collect
// (the region is not current) and jl_gc_region_census_open (the region is
// current, mid-window). Returns the number of freed cells, or ERACE.
static int64_t region_census_core(jl_task_t *ct, jl_ptls_t ptls, jl_thread_heap_t *heap, int n)
{
    // Stop the world the way jl_gc_collect does. jl_safepoint_start_gc
    // refuses while the disable counter is set (the census is meant to run
    // with the stock collector disabled), so the counter is cleared for the
    // stop and restored after.
    uint64_t t0 = jl_hrtime();
    uint32_t saved_disable = jl_atomic_exchange(&jl_gc_disable_counter, 0);
    int8_t old_state = jl_atomic_load_relaxed(&ptls->gc_state);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    if (!jl_safepoint_start_gc(ct)) {
        jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        jl_safepoint_wait_thread_resume(ct);
        return JL_GC_REGION_ERACE;
    }
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);
    uint64_t t_stw = jl_hrtime();

    // A child of the region live on another heap holds legal references
    // into this heap's instance of the region as well (a trunk). The
    // caller checked its own heap; the other heaps are readable only now.
    int64_t freed = region_child_live_on_any_heap(gc_all_tls_states, gc_n_threads, n)
                    ? (int64_t)JL_GC_REGION_ECHILD : 0;
    uint64_t t_mark = t_stw, t_sweep = t_stw;
    if (freed == 0) {
        region_census_begin(n);
        region_census_mark(ptls, gc_all_tls_states, gc_n_threads, heap, n, NULL);
        t_mark = jl_hrtime();
        freed = region_scoped_sweep(heap, n);
        region_clear_marks_on_other_heaps(heap, n);
        t_sweep = jl_hrtime();
        region_census_end();
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    jl_safepoint_wait_thread_resume(ct);
    jl_atomic_store_relaxed(&region_collect_stats[0], t_sweep - t0);
    jl_atomic_store_relaxed(&region_collect_stats[1], t_stw - t0);
    jl_atomic_store_relaxed(&region_collect_stats[2], t_mark - t_stw);
    jl_atomic_store_relaxed(&region_collect_stats[3], t_sweep - t_mark);
    return freed;
}

// The stop-the-world census of region n on the calling thread's heap: free
// the dead objects, keep the live ones. Pending finalizers refuse it: the
// world stays stopped, so nothing could run them. Returns the cells freed,
// or a refusal code: EINVAL for a bad number or a region never used,
// EQUARANTINED after an escape, EFINALIZERS with pending finalizers, ECHILD
// while a child region is live, EBUSY while a window is open on any thread
// or finalizers run on this one.
//
// A live child refuses the census as it refuses the reset. A child holds
// legal references into its parent, and the census filter drops the child's
// objects at the claim: a parent object that only the child references
// would never be marked, and the sweep would free it under the child.
JL_DLLEXPORT int64_t jl_gc_region_collect(int n)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    if (!region_valid(n) || heap->regions[n] == NULL)
        return JL_GC_REGION_EINVAL;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return JL_GC_REGION_EQUARANTINED;
    if (__unlikely(heap->regions[n]->finalizers.len != 0))
        return JL_GC_REGION_EFINALIZERS;
    if (__unlikely((heap->region_haschild_mask >> n) & 1))
        return JL_GC_REGION_ECHILD;
    if (heap->current_region != 0 || heap->finalizer_depth != 0 ||
        jl_atomic_load_relaxed(&region_windows_open) != 0)
        return JL_GC_REGION_EBUSY;
    return region_census_core(ct, ptls, heap, n);
}

// The cooperative census: jl_gc_region_collect without the stop-the-world.
// The caller is the only thread that references the region, so only the
// caller's execution roots are scanned. Every other thread must sit in a
// GC-safe state (parked in C): a thread that runs managed code refuses the
// cooperative path, and the caller falls back to the stop-the-world entry.
// No safepoint is reached while the filter is set, so no other thread can
// start a collection in between; a thread that wants one waits for the
// census at its safepoint. The dead objects' finalizers run after the sweep,
// with the filter off: the census keeps them for one more cycle, so a
// finalizer that allocates and triggers a stock collection sees a whole heap.
// Returns the cells freed, or a refusal code: EINVAL, EQUARANTINED, ECHILD
// and EBUSY as the stop-the-world census, EUNSAFE while another thread runs
// managed code.
JL_DLLEXPORT int64_t jl_gc_region_collect_coop(int n)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    if (!region_valid(n) || heap->regions[n] == NULL)
        return JL_GC_REGION_EINVAL;
    if (__unlikely(jl_gc_region_quarantined(n)))
        return JL_GC_REGION_EQUARANTINED;
    if (__unlikely((heap->region_haschild_mask >> n) & 1))
        return JL_GC_REGION_ECHILD;
    if (heap->current_region != 0 || heap->finalizer_depth != 0)
        return JL_GC_REGION_EBUSY;

    uint64_t t0 = jl_hrtime();
    // The count excludes the stop-the-world entries for the duration, and
    // it excludes a second cooperative census: the two share one process-wide
    // filter and one task table, so a pair that both passed a read of the
    // count would mark with each other's filter and free live objects. The
    // claim is the test and the increment in one act.
    int zero = 0;
    if (!jl_atomic_cmpswap(&region_windows_open, &zero, 1))
        return JL_GC_REGION_EBUSY;
    int nthreads = jl_atomic_load_acquire(&jl_n_threads);
    jl_ptls_t *all = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (int t_i = 0; t_i < nthreads; t_i++) {
        jl_ptls_t ptls2 = all[t_i];
        if (ptls2 == NULL || ptls2 == ptls)
            continue;
        if (jl_atomic_load_relaxed(&ptls2->gc_state) == JL_GC_STATE_UNSAFE) {
            jl_atomic_fetch_add_relaxed(&region_windows_open, -1);
            return JL_GC_REGION_EUNSAFE;
        }
    }
    // Every other thread is parked, so its live-child mask is stable.
    if (region_child_live_on_any_heap(all, nthreads, n)) {
        jl_atomic_fetch_add_relaxed(&region_windows_open, -1);
        return JL_GC_REGION_ECHILD;
    }
    uint64_t t_stw = jl_hrtime();

    arraylist_t dead;
    region_census_begin(n);
    region_census_mark(ptls, &ptls, 1, heap, n, &dead);
    uint64_t t_mark = jl_hrtime();
    int64_t freed = region_scoped_sweep(heap, n);
    uint64_t t_sweep = jl_hrtime();
    region_census_end();
    jl_atomic_fetch_add_relaxed(&region_windows_open, -1);

    jl_atomic_store_relaxed(&region_collect_stats[0], t_sweep - t0);
    jl_atomic_store_relaxed(&region_collect_stats[1], t_stw - t0);
    jl_atomic_store_relaxed(&region_collect_stats[2], t_mark - t_stw);
    jl_atomic_store_relaxed(&region_collect_stats[3], t_sweep - t_mark);
    region_run_finalizer_list(ct, &dead);
    return freed;
}

// The census of the open region, from the allocator when the region passed
// the page threshold: a computation whose garbage dies inside the window,
// not at its boundary, keeps its live state and gets its dead cells back
// without the region growing without bound. Pending finalizers, a
// quarantine and a live child region fall through to the ordinary path.
// Returns 1 when a census ran.
int jl_gc_region_census_open(jl_ptls_t ptls)
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    int n = heap->current_region;
    if (heap->regions[n]->finalizers.len != 0 || jl_gc_region_quarantined(n) ||
        ((heap->region_haschild_mask >> n) & 1))
        return 0;
    return region_census_core(jl_current_task, ptls, heap, n) >= 0;
}

// --- debug ------------------------------------------------------------------------------

// Turn the extra reporting of the reset's root check on or off,
// process-wide. The check itself always runs in jl_gc_region_reset; this
// names the objects it finds.
JL_DLLEXPORT void jl_gc_region_set_debug(int on)
{
    region_debug_checks = on;
}

// The walk of one heap's pages of region n after a census mark: every
// marked cell is an object an execution root still references, which a free
// would dangle. The marks are cleared as they are counted, so the walk leaves
// clean state. The pages are walked up to the bump cursor of their pool.
static int64_t region_count_marked(jl_thread_heap_t *heap, int n)
{
    int64_t violations = 0;
    jl_gc_pool_t *pools = heap->regions[n]->pools;
    char *bump[JL_GC_N_MAX_POOLS];
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++)
        bump[i] = (char*)pools[i].newpages;
    for (jl_gc_pagemeta_t *pg = heap->regions[n]->pages; pg != NULL; pg = pg->region_next) {
        int i = pg->pool_n;
        int osize = pg->osize;
        char *cell = pg->data + GC_PAGE_OFFSET;
        size_t ncells = (GC_PAGE_SZ - GC_PAGE_OFFSET) / (size_t)osize;
        char *end = cell + ncells * (size_t)osize;
        if (bump[i] != NULL && gc_page_data(bump[i] - 1) == pg->data &&
            (char*)bump[i] < end)
            end = (char*)bump[i];
        for (; cell < end; cell += osize) {
            jl_taggedvalue_t *tv = (jl_taggedvalue_t*)cell;
            uintptr_t h = tv->header;
            if (h & GC_MARKED) {
                tv->header = h & ~(uintptr_t)(GC_MARKED | GC_OLD);
                if (region_debug_checks && violations < 8) {
                    jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(jl_valueof(tv));
                    jl_safe_printf("REGION-RESET-CHECK: live reference into region %d: %p type=%s\n",
                                   n, (void*)jl_valueof(tv),
                                   jl_symbol_name(vt->name->name));
                }
                violations++;
            }
        }
        pg->has_marked = 0;
    }
    return violations;
}

// The root scan the checked reset and jl_gc_region_check share. The caller
// stopped the world and set gc_n_threads and gc_all_tls_states.
//
// Mark from the execution roots of every thread with the region filter, then
// count and clear the marks on this heap's pages of the region. The mark ran
// from the roots of every thread, so it marked the other heaps' objects of
// region n too; those marks are cleared as well. A mark left there would
// make the next checked reset of that heap refuse falsely, and would stop
// the next census of that heap at the marked object, which then frees the
// object's children under a live parent. Returns the count.
static int64_t region_root_scan(jl_ptls_t ptls, jl_thread_heap_t *heap, int n)
{
    region_census_begin(n);
    region_census_mark(ptls, gc_all_tls_states, gc_n_threads, heap, n, NULL);
    int64_t violations = region_count_marked(heap, n);
    region_clear_marks_on_other_heaps(heap, n);
    region_census_end();
    return violations;
}

// The root scan of the global reset: one mark, then the count on every
// heap's instance of the region. The same preconditions.
static int64_t region_root_scan_global(jl_ptls_t ptls, int n)
{
    region_census_begin(n);
    region_census_mark(ptls, gc_all_tls_states, gc_n_threads, &ptls->gc_tls.heap, n, NULL);
    int64_t violations = 0;
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 != NULL && ptls2->gc_tls.heap.regions[n] != NULL)
            violations += region_count_marked(&ptls2->gc_tls.heap, n);
    }
    region_census_end();
    return violations;
}

// The debug check behind the refused reset: a region may reset only when no
// execution root references into it. The check is a census mark that must
// find nothing: stop the world, mark from the execution roots with the
// filter, then walk the region's pages -- every marked cell is a violation.
// The marks are cleared again, so the check is repeatable and leaves clean
// state. Returns the count, or a refusal code.
JL_DLLEXPORT int64_t jl_gc_region_check(int n)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    if (!region_valid(n) || heap->regions[n] == NULL)
        return 0;
    if (heap->current_region != 0 || heap->finalizer_depth != 0 ||
        jl_atomic_load_relaxed(&region_windows_open) != 0)
        return JL_GC_REGION_EBUSY;

    uint32_t saved_disable = jl_atomic_exchange(&jl_gc_disable_counter, 0);
    int8_t old_state = jl_atomic_load_relaxed(&ptls->gc_state);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    if (!jl_safepoint_start_gc(ct)) {
        jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        jl_safepoint_wait_thread_resume(ct);
        return JL_GC_REGION_ERACE;
    }
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);

    int64_t violations = region_root_scan(ptls, heap, n);

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_atomic_store_release(&jl_gc_disable_counter, saved_disable);
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    jl_safepoint_wait_thread_resume(ct);
    return violations;
}

// The consistency walk of a region's page chains: every chained page
// carries tag n and an intact page-map entry; the pool cursors point into
// tagged pages; and the allocated-page stack sees the same tagged pages as
// the chains. Returns the error count, or EINVAL.
JL_DLLEXPORT int jl_gc_region_verify(int n)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    if (!region_valid(n))
        return JL_GC_REGION_EINVAL;
    if (heap->regions[n] == NULL)
        return 0;
    int errors = 0;
    uint64_t chain_len = 0;
    for (jl_gc_pagemeta_t *pg = heap->regions[n]->pages; pg != NULL; pg = pg->region_next) {
        chain_len++;
        if (pg->region_n != n) {
            jl_safe_printf("REGION-VERIFY: chained page %p tag %d, expected %d\n",
                           (void*)pg->data, (int)pg->region_n, n);
            errors++;
        }
        jl_gc_pagemeta_t *meta = page_metadata(pg->data);
        if (meta != pg) {
            jl_safe_printf("REGION-VERIFY: page %p map meta %p != chained %p\n",
                           (void*)pg->data, (void*)meta, (void*)pg);
            errors++;
        }
        if (chain_len > 1000000) {
            jl_safe_printf("REGION-VERIFY: chain does not terminate\n");
            errors++;
            break;
        }
    }
    uint64_t fresh_len = 0;
    for (jl_gc_pagemeta_t *fp = heap->regions[n]->fresh_pages; fp != NULL; fp = fp->region_next) {
        fresh_len++;
        if (fp->region_n != n) {
            jl_safe_printf("REGION-VERIFY: fresh page %p tag %d, expected %d\n",
                           (void*)fp->data, (int)fp->region_n, n);
            errors++;
        }
        if (fresh_len > 1000000) {
            jl_safe_printf("REGION-VERIFY: fresh chain does not terminate\n");
            errors++;
            break;
        }
    }
    const jl_gc_pool_t *pools = heap->regions[n]->pools;
    for (int i = 0; i < JL_GC_N_MAX_POOLS; i++) {
        jl_taggedvalue_t *fl = pools[i].newpages;
        if (fl != NULL) {
            jl_gc_pagemeta_t *meta = page_metadata((char*)fl - 1);
            if (meta == NULL || meta->region_n != n) {
                jl_safe_printf("REGION-VERIFY: pool %d newpages %p on page tag %d\n",
                               i, (void*)fl, meta ? (int)meta->region_n : -1);
                errors++;
            }
        }
        if (pools[i].freelist != NULL) {
            jl_gc_pagemeta_t *meta = page_metadata((char*)pools[i].freelist);
            if (meta == NULL || meta->region_n != n) {
                jl_safe_printf("REGION-VERIFY: pool %d freelist head %p on page tag %d\n",
                               i, (void*)pools[i].freelist,
                               meta ? (int)meta->region_n : -1);
                errors++;
            }
        }
    }
    uint64_t in_allocd = 0;
    for (jl_gc_pagemeta_t *pg = jl_atomic_load_relaxed(&ptls->gc_tls.page_metadata_allocd.bottom);
         pg != NULL; pg = pg->next) {
        if (pg->region_n == n)
            in_allocd++;
    }
    if (in_allocd != chain_len + fresh_len) {
        jl_safe_printf("REGION-VERIFY: chain %llu + fresh %llu pages, allocd sees %llu tagged\n",
                       (unsigned long long)chain_len,
                       (unsigned long long)fresh_len,
                       (unsigned long long)in_allocd);
        errors++;
    }
    return errors;
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

void jl_gc_region_init(void)
{
    uint64_t up = 0;
    for (int r = 0; r < JL_GC_MAX_REGIONS; r++) {
        region_parent[r] = (r == 0) ? 0 : (uint8_t)(r - 1);
        up |= (uint64_t)1 << r;                  // {0,...,r}
        jl_atomic_store_relaxed(&region_uptree[r], up);
    }
    htable_new(&region_census_tasks, 0);
}

void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT
{
    heap->current_region = 0;
    heap->saved_region = 0;
    heap->finalizer_depth = 0;
    heap->active_pools = heap->norm_pools;
    memset(heap->regions, 0, sizeof(heap->regions)); // no region has state yet
    heap->region_live_mask = 0;
    heap->region_haschild_mask = 0;
    memset(heap->region_child_count, 0, sizeof(heap->region_child_count));
}

#ifdef __cplusplus
}
#endif
