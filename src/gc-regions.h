// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions: the runtime interface
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// A thread allocates into region n while a window on n is open
// (jl_gc_region_set), and frees every object of the region at once with a
// reset, without a trace. The design and the rules an application must keep
// are in doc/src/devdocs/gc-regions.md. Every entry point takes region
// numbers; the numbering and its meaning belong to the application.
//
// The stock collector implements the regions (src/gc-regions.c); a build
// with a third-party heap gets the stubs at the end of this file, so the
// callers in task.c, gc-common.c, gf.c, jltypes.c and staticdata.c compile
// unchanged.

#ifndef JL_GC_REGIONS_H
#define JL_GC_REGIONS_H

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

// The refusal codes. An entry that returns a count returns the code cast
// to its unsigned type: (uint64_t)-2 stands for -2.
enum {
    JL_GC_REGION_EINVAL = -1,       // a bad region number, a bad tree edge,
                                    // or a build that cannot allocate in a region
    JL_GC_REGION_EBUSY = -2,        // the region is current, a window is open,
                                    // or this heap runs region finalizers now
    JL_GC_REGION_ERACE = -3,        // lost the race for the safepoint; retry
    JL_GC_REGION_EUNSAFE = -4,      // another thread runs managed code
                                    // (cooperative census only)
    JL_GC_REGION_EQUARANTINED = -5, // the region was escaped from; its memory
                                    // is retained
    JL_GC_REGION_EFINALIZERS = -6,  // finalizers are pending; a cooperative
                                    // census runs them first
    JL_GC_REGION_ECHILD = -7,       // the region has a live child region
    JL_GC_REGION_EROOT = -8,        // the debug check found an execution root
                                    // that references the region
};

// --- the runtime's own allocations ------------------------------------------
// The runtime allocates on behalf of the task that runs it, and what it
// allocates outlives any window that task holds: it belongs to region 0.
// The C sites - inference, compilation, type instantiation, the dispatch
// cache (gf.c, jltypes.c) - close the window with jl_gc_region_set(0) and
// reopen it after; they never park the task, and an exception past the
// bracket leaves the window closed, which is coherent. The lazily
// initialized state of Base (OncePerProcess, OncePerThread in lock.jl) can
// park the task on a lock, and a closed window would let the parked task
// migrate. So Base brackets its initializers with this pair instead:
// `suspend` installs region 0 and returns the parked region, `resume`
// installs the parked region again (0: nothing to do); the window stays
// open in between - the task stays pinned to its thread and the window
// counts as open - and a `finally` in Base runs the resume on the
// exception path. The pair lives in gc-common.c and is exported for the
// ccall from Base; a third-party heap has no window to park.
JL_DLLEXPORT int jl_gc_region_suspend(void);
JL_DLLEXPORT void jl_gc_region_resume(int parked);
// Borrow a region for the next allocations of this thread, and give it
// back. The window is untouched: a borrow is not a window. A replacement
// buffer is allocated in the region of the buffer it replaces this way.
JL_DLLEXPORT int jl_gc_region_borrow(int n);
JL_DLLEXPORT void jl_gc_region_unborrow(int lent);

#ifndef WITH_THIRD_PARTY_HEAP

// --- the exported API ------------------------------------------------------
// Open a window on region n (n = 0 closes it). Returns the region that was
// current, or a refusal code.
JL_DLLEXPORT int jl_gc_region_set(int n);
JL_DLLEXPORT int jl_gc_region_current(void);
// Free every object of region n on the calling thread's heap, after a check
// that no execution root references into it. Returns the number of pages the
// region held, or a refusal code.
JL_DLLEXPORT uint64_t jl_gc_region_reset(int n);
// The same, without the check and without its pause. A reference from a
// stack slot, a register or a parked task's stack is left dangling.
JL_DLLEXPORT uint64_t jl_gc_region_unsafe_reset(int n);
// Close the window of a task that reaches its end (task.c).
void jl_gc_region_close_window(jl_task_t *ct) JL_NOTSAFEPOINT;
// Free region n on every heap at once, with the world stopped.
JL_DLLEXPORT uint64_t jl_gc_region_reset_global(int n);
// The region tree: declare the parent of a region before either is used.
JL_DLLEXPORT int jl_gc_region_declare_parent(int child, int parent);
JL_DLLEXPORT int jl_gc_region_parent_of(int child);
// A census frees the dead objects of one region and keeps the live ones.
// The stop-the-world census; the cooperative census, which needs every
// other thread parked GC-safe; the page threshold that triggers a census
// on the open region from the allocator (0 = never).
JL_DLLEXPORT int64_t jl_gc_region_collect(int n);
JL_DLLEXPORT int64_t jl_gc_region_collect_coop(int n);
JL_DLLEXPORT void jl_gc_region_census_threshold(int pages);
// Queries: the region of an object, the page count of a region on this
// heap, whether an escape quarantined a region, the phase times and counts
// of the last census (see jl_gc_region_stat).
JL_DLLEXPORT int jl_gc_region_of(jl_value_t *v);
JL_DLLEXPORT int jl_gc_region_pages(int n);
JL_DLLEXPORT int jl_gc_region_quarantined(int n);
JL_DLLEXPORT uint64_t jl_gc_region_stat(int i);
// Debug: with checks on, a reset refuses while an execution root references
// into the region; jl_gc_region_check runs that check alone and returns the
// count; jl_gc_region_verify walks the region's page chains for consistency.
JL_DLLEXPORT void jl_gc_region_set_debug(int on);
JL_DLLEXPORT int64_t jl_gc_region_check(int n);
JL_DLLEXPORT int jl_gc_region_verify(int n);
// The escape barrier, called by the write barrier while a region is in use.
JL_DLLEXPORT void jl_gc_region_wb(const void *parent, const void *child) JL_NOTSAFEPOINT;
// Prefault the pool heap so a later allocation never faults (gc-pages.c).
JL_DLLEXPORT uint64_t jl_gc_heap_reserve(uint64_t bytes) JL_NOTSAFEPOINT;

// --- the hooks the rest of the runtime calls --------------------------------
// The census filter: the region whose census runs now, 0 otherwise. The mark
// loops read it once per object array and pass it down as a parameter.
extern _Atomic(int) jl_gc_region_census_target;
// The filter as the mark loops read it. The stock-only build
// (JL_NO_REGION_ALLOC) opens no window, so no region initializes and no
// census runs: the filter is the constant 0, and the compiler folds the
// census branches out of the mark loops.
STATIC_INLINE int jl_gc_region_census_filter(void) JL_NOTSAFEPOINT
{
#ifdef JL_NO_REGION_ALLOC
    return 0;
#else
    return jl_atomic_load_relaxed(&jl_gc_region_census_target);
#endif
}
// The claim of a task the census meets outside the region. Returns 1 the
// first time a task is met in this census, 0 afterwards.
int jl_gc_region_census_claim_task(jl_value_t *task) JL_NOTSAFEPOINT;
// A finalizer on a region object goes to the region's own list. Returns 1
// when it took the registration.
int jl_gc_region_add_finalizer(jl_ptls_t ptls, void *v, void *f);
// A memory with malloc'd data allocated in a region is tracked by the
// region. Returns 1 when it took the memory.
int jl_gc_region_track_malloced(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned) JL_NOTSAFEPOINT;
// Install a task's parked region on a thread at a task switch.
void jl_gc_region_install_task(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT;
// Install a borrowed region on a thread (jl_gc_region_borrow); the region
// becomes live on this heap.
void jl_gc_region_install_borrow(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT;
// The brackets around a stock collection: park every open window before it,
// install the windows again after it. Between them, every pass of the
// collection clears the marks it left on region pages after its sweep.
void jl_gc_region_prepare_stock_collection(void) JL_NOTSAFEPOINT;
void jl_gc_region_clear_stock_marks(void) JL_NOTSAFEPOINT;
void jl_gc_region_finish_stock_collection(void) JL_NOTSAFEPOINT;
// Mark every region finalizer list as a root of the stock collection.
void jl_gc_region_mark_finalizer_lists(jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT;
// The census the allocator triggers on the open region (see the inline below).
int jl_gc_region_census_open(jl_ptls_t ptls);
extern _Atomic(int) jl_gc_region_census_page_threshold;
// Process and per-heap initialization.
void jl_gc_region_init(void);
void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT;

// The window follows the task: park the region of the task that leaves,
// install the region of the task that arrives.
STATIC_INLINE void jl_gc_region_task_switch(jl_ptls_t ptls, jl_task_t *lastt, jl_task_t *t) JL_NOTSAFEPOINT
{
    lastt->region = ptls->gc_tls.heap.current_region;
    if (t->region != lastt->region)
        jl_gc_region_install_task(ptls, t->region);
}

// The brackets around a finalizer list: finalizers run with region 0
// installed, whatever window the thread holds, and while they run no window
// opens and no region entry runs on the thread. `begin` returns the parked
// region for `end`. A finalizer does not task-switch (the contract of
// Base.finalizer), so the depth is per thread.
STATIC_INLINE int jl_gc_region_finalizers_begin(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    int parked = heap->current_region;
    if (parked != 0)
        jl_gc_region_install_task(ptls, 0);
    heap->finalizer_depth++;
    return parked;
}

STATIC_INLINE void jl_gc_region_finalizers_end(jl_ptls_t ptls, int parked) JL_NOTSAFEPOINT
{
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    heap->finalizer_depth--;
    if (parked != 0)
        jl_gc_region_install_task(ptls, parked);
}

// The allocator's check, inline because it runs on the allocation path
// while a window is open: the threshold is off, or the region is small. The
// caller tests that a window is open, so the open region has state here.
STATIC_INLINE int jl_gc_region_maybe_census(jl_ptls_t ptls)
{
    int threshold = jl_atomic_load_relaxed(&jl_gc_region_census_page_threshold);
    if (__likely(threshold <= 0))
        return 0;
    jl_thread_heap_t *heap = &ptls->gc_tls.heap;
    assert(heap->current_region != 0 && heap->regions[heap->current_region] != NULL);
    if ((int)heap->regions[heap->current_region]->n_pages < threshold)
        return 0;
    return jl_gc_region_census_open(ptls);
}

#else // WITH_THIRD_PARTY_HEAP

// A third-party heap has no regions: every window is refused, every hook
// declines, and the task switch carries nothing.
STATIC_INLINE int jl_gc_region_set(int n) { (void)n; return JL_GC_REGION_EINVAL; }
STATIC_INLINE int jl_gc_region_current(void) { return 0; }
STATIC_INLINE int jl_gc_region_add_finalizer(jl_ptls_t ptls, void *v, void *f) { (void)ptls; (void)v; (void)f; return 0; }
STATIC_INLINE int jl_gc_region_track_malloced(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned) { (void)ptls; (void)m; (void)isaligned; return 0; }
STATIC_INLINE void jl_gc_region_task_switch(jl_ptls_t ptls, jl_task_t *lastt, jl_task_t *t) { (void)ptls; (void)lastt; (void)t; }
STATIC_INLINE void jl_gc_region_close_window(jl_task_t *ct) { (void)ct; }
STATIC_INLINE int jl_gc_region_finalizers_begin(jl_ptls_t ptls) { (void)ptls; return 0; }
STATIC_INLINE void jl_gc_region_finalizers_end(jl_ptls_t ptls, int parked) { (void)ptls; (void)parked; }

#endif // WITH_THIRD_PARTY_HEAP

#ifdef __cplusplus
}
#endif

#endif // JL_GC_REGIONS_H
