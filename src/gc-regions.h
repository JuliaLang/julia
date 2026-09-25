// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions: the runtime interface
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// While a window on region n is open (jl_gc_region_set), the thread
// allocates into region n; a reset frees every object of the region at once,
// without a trace. doc/src/devdocs/gc-regions.md states the design and the
// rules a program must keep. Every entry takes region numbers; their meaning
// belongs to the program.
//
// The stock collector implements the regions (src/gc-regions.c) in a build
// with WITH_GC_REGIONS. The rest of the runtime calls the hooks at the end
// of this file; without the flag each hook expands to no code.

#ifndef JL_GC_REGIONS_H
#define JL_GC_REGIONS_H
#ifdef WITH_GC_REGIONS

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
    JL_GC_REGION_EQUARANTINED = -5, // an escape quarantined the region, until
                                    // the next stock collection
    JL_GC_REGION_EFINALIZERS = -6,  // finalizers are pending; a cooperative
                                    // census runs them first
    JL_GC_REGION_ECHILD = -7,       // the region has a live child region
    JL_GC_REGION_EROOT = -8,        // an execution root references the region
};

// The state of one region on one heap, allocated at the first use of the
// region on that heap and kept for the life of the process; a reset parks
// the pages for the next window. `regions[n]` of the heap points at it
// (gc-tls-stock.h); region 0 has none, its pools are norm_pools.
typedef struct _jl_gc_region_state_t {
    jl_gc_pool_t pools[JL_GC_N_MAX_POOLS];
    struct _jl_gc_pagemeta_t *pages;       // chained through region_next
    struct _jl_gc_pagemeta_t *fresh_pages; // wholly dead pages, reused before new ones
    struct _jl_gc_pagemeta_t *pages_tail;  // the last link of `pages`
    uint32_t n_pages;                      // pages on `pages`
    uint32_t n_fresh;                      // pages on `fresh_pages`
    arraylist_t finalizers;                // (tagged object, function) pairs of the region
    small_arraylist_t mallocarrays;        // memories with malloc'd data of the region
} jl_gc_region_state_t;


// --- the exported API ------------------------------------------------------
// Open a window on region n (n = 0 closes it). Returns the region that was
// current, or a refusal code.
JL_DLLEXPORT int jl_gc_region_set(int n) JL_NOTSAFEPOINT;
JL_DLLEXPORT int jl_gc_region_current(void) JL_NOTSAFEPOINT;

// --- the hooks the rest of the runtime calls --------------------------------
// The census filter: the region of the census that runs now, 0 otherwise.
extern _Atomic(int) jl_gc_region_census_target;
STATIC_INLINE int jl_gc_region_census_filter(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&jl_gc_region_census_target);
}
// Records a task the census reached outside the region; returns 1 the first
// time, 0 afterwards.
int jl_gc_region_census_claim_task(jl_value_t *task) JL_NOTSAFEPOINT;
// Registers a finalizer of a region object on the list of its region;
// returns 1 when v is a region object of this thread. A registration from
// another thread quarantines the region and returns 0, so the finalizer
// goes to the thread's own list.
int jl_gc_region_add_finalizer(jl_ptls_t ptls, void *v, void *f) JL_NOTSAFEPOINT;
// Tracks a memory with malloc'd data on the list of its region; returns 1
// when m is a region object.
int jl_gc_region_track_malloced(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned) JL_NOTSAFEPOINT;
// Install a task's parked region on a thread at a task switch.
void jl_gc_region_install_task(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT;
// The brackets of a stock collection: park every open window before it,
// hand every quarantined region to the stock collector, and install the
// windows again after it; after each pass, clear the marks the pass left on
// region pages.
void jl_gc_region_prepare_stock_collection(void) JL_NOTSAFEPOINT;
void jl_gc_region_clear_stock_marks(void) JL_NOTSAFEPOINT;
void jl_gc_region_finish_stock_collection(void) JL_NOTSAFEPOINT;
// Mark every region finalizer list as a root of the stock collection.
void jl_gc_region_mark_finalizer_lists(jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT;
// Process and per-heap initialization.
void jl_gc_region_init(void) JL_NOTSAFEPOINT;
void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT;


// The brackets of a finalizer list: region 0 is installed while it runs, no
// window opens on the thread, and no region entry runs. `begin` returns the
// parked region for `end`; the depth is per thread, because a finalizer does
// not switch tasks.
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



#ifdef __cplusplus
}
#endif

#else // WITH_GC_REGIONS

// Without the regions each hook expands to no code, and the runtime
// compiles to the stock runtime.
#define jl_gc_region_finalizers_begin(ptls) 0
#define jl_gc_region_finalizers_end(ptls, parked) ((void)(parked))
#define jl_gc_region_add_finalizer(ptls, v, f) 0
#define jl_gc_region_track_malloced(ptls, m, isaligned) 0
#define jl_gc_region_census_filter() 0
#define jl_gc_region_mark_finalizer_lists(mq) ((void)(mq))
#define jl_gc_region_clear_stock_marks() ((void)0)
#define jl_gc_region_prepare_stock_collection() ((void)0)
#define jl_gc_region_finish_stock_collection() ((void)0)
#define jl_gc_region_init() ((void)0)
#define jl_gc_region_init_heap(heap) ((void)(heap))

#endif // WITH_GC_REGIONS
#endif // JL_GC_REGIONS_H
