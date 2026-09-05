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
// callers in task.c, gc-common.c, gf.c and jltypes.c compile unchanged.

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
    JL_GC_REGION_EINVAL = -1,       // a bad region number, or a build that
                                    // cannot allocate in a region
    JL_GC_REGION_EBUSY = -2,        // the region is current, or this heap
                                    // runs region finalizers now
    JL_GC_REGION_EQUARANTINED = -5, // the region was escaped from; its memory
                                    // is retained
    JL_GC_REGION_EFINALIZERS = -6,  // finalizers are pending after the bounded
                                    // rounds of the reset
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
// Free every object of region n on the calling thread's heap. Returns the
// number of pages the region held, or a refusal code.
JL_DLLEXPORT uint64_t jl_gc_region_reset(int n);
// Queries: the region of an object, whether an escape quarantined it.
JL_DLLEXPORT int jl_gc_region_of(jl_value_t *v);
JL_DLLEXPORT int jl_gc_region_quarantined(int n);
// The escape barrier, called by the write barrier while a region is in use.
JL_DLLEXPORT void jl_gc_region_wb(const void *parent, const void *child) JL_NOTSAFEPOINT;

// --- the hooks the rest of the runtime calls --------------------------------
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
// Per-heap initialization.
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

#else // WITH_THIRD_PARTY_HEAP

// A third-party heap has no regions: every window is refused, every hook
// declines, and the task switch carries nothing.
STATIC_INLINE int jl_gc_region_set(int n) { (void)n; return JL_GC_REGION_EINVAL; }
STATIC_INLINE int jl_gc_region_current(void) { return 0; }
STATIC_INLINE int jl_gc_region_add_finalizer(jl_ptls_t ptls, void *v, void *f) { (void)ptls; (void)v; (void)f; return 0; }
STATIC_INLINE int jl_gc_region_track_malloced(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned) { (void)ptls; (void)m; (void)isaligned; return 0; }
STATIC_INLINE void jl_gc_region_task_switch(jl_ptls_t ptls, jl_task_t *lastt, jl_task_t *t) { (void)ptls; (void)lastt; (void)t; }
STATIC_INLINE int jl_gc_region_finalizers_begin(jl_ptls_t ptls) { (void)ptls; return 0; }
STATIC_INLINE void jl_gc_region_finalizers_end(jl_ptls_t ptls, int parked) { (void)ptls; (void)parked; }

#endif // WITH_THIRD_PARTY_HEAP

#ifdef __cplusplus
}
#endif

#endif // JL_GC_REGIONS_H
