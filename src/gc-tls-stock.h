// This file is a part of Julia. License is MIT: https://julialang.org/license

// Meant to be included in "julia_threads.h"
#ifndef JL_GC_TLS_H
#define JL_GC_TLS_H

#include "julia_atomics.h"
#include "work-stealing-queue.h"
// GC threading ------------------------------------------------------------------

#include "arraylist.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    struct _jl_taggedvalue_t *freelist; // root of list of free objects
    struct _jl_taggedvalue_t *newpages; // root of list of chunks of free objects
    uint16_t osize; // size of objects in this pool
} jl_gc_pool_t;

typedef struct {
    // variable for tracking young (i.e. not in `GC_OLD_MARKED`/last generation) large objects
    struct _bigval_t *young_generation_of_bigvals;

    // lower bound of the number of pointers inside remembered values
    int remset_nptr;
    // remembered set
    arraylist_t remset;

    // variables for allocating objects from pools
#define JL_GC_N_MAX_POOLS 51 // conservative. must be kept in sync with `src/julia_internal.h`
    jl_gc_pool_t norm_pools[JL_GC_N_MAX_POOLS];

    // --- regions ---------------------------------------------------------------
    // A region is a numbered set of pool pages with its own allocation
    // cursors (see gc-regions.h and doc/src/devdocs/gc-regions.md). Region 0
    // is the default heap: norm_pools and the pages the stock collector sweeps.
#define JL_GC_MAX_REGIONS 64
    uint8_t current_region;
    uint8_t saved_region;   // parked by a stock collection: every thread
                            // runs the collection with region 0 installed
    uint8_t finalizer_depth; // > 0 while finalizers run on this thread: the
                            // window is parked and region 0 installed, no
                            // window opens and no region entry runs until
                            // they return (jl_gc_region_finalizers_begin)
    // The live pool array of the current region: norm_pools for region 0,
    // regions[n]->pools for region n. Allocation paths decode their stable
    // norm_pools-relative offset into an index and address through this
    // pointer, so a region switch is one pointer store -- no copying, and
    // no parked state that can go stale.
    jl_gc_pool_t *active_pools;
    // The state of each region on this heap (jl_gc_region_state_t, below),
    // made on the first use of the region here: a window, a task switch that
    // installs a window, or a borrow (region_lazy_init in gc-regions.c). NULL
    // until then, and always NULL for region 0, whose pools are norm_pools.
    // The state is never freed: a reset parks the pages for the next window.
    struct _jl_gc_region_state_t *regions[JL_GC_MAX_REGIONS];
    // The tree's reset bookkeeping, per heap (a region's pages are per heap).
    // A region is "live" between a region_set onto it and its reset. Its
    // parent counts its live children; region_haschild bit r is set while
    // region r has at least one live child. A leaf reset is then one bit
    // test: refuse to reset a region whose haschild bit is set, because a
    // live descendant may reference into it (a legal leaf -> trunk edge that
    // the reset would dangle). Zero-initialized: no region is live, none has
    // a child.
    uint64_t region_live_mask;             // bit r: region r is live here
    uint64_t region_haschild_mask;         // bit r: region r has a live child
    uint8_t region_child_count[JL_GC_MAX_REGIONS];
    // --------------------------------------------------------------------------
} jl_thread_heap_t;

// The state of one region on one heap: the allocation cursors, the page
// chains, and the two lists a reset and a census work through. A heap holds
// a pointer to it in `regions[n]` once the region is used on that heap.
typedef struct _jl_gc_region_state_t {
    jl_gc_pool_t pools[JL_GC_N_MAX_POOLS];
    struct _jl_gc_pagemeta_t *pages;   // chained through region_next
    struct _jl_gc_pagemeta_t *fresh_pages; // wholly dead pages, ready for
                                       // reuse; their metadata is stale -
                                       // gc_add_page resets a page when it
                                       // claims it, so nobody resets one here
    struct _jl_gc_pagemeta_t *pages_tail; // last link of `pages`, so a reset
                                       // parks the whole chain in O(1)
    uint32_t n_pages;                  // pages on `pages`
    uint32_t n_fresh;                  // pages on `fresh_pages`
    arraylist_t finalizers;            // (tagged object, function) pairs
                                       // registered on this region's
                                       // objects: the reset runs them all
                                       // before it frees; the cooperative
                                       // census runs the dead ones after
                                       // its sweep, with the filter off
    small_arraylist_t mallocarrays;    // memories with malloc'd data
                                       // allocated in this region: their
                                       // data is freed by the reset (all
                                       // of it) and by the census (the
                                       // dead), never by the stock sweep
} jl_gc_region_state_t;

typedef struct {
    ws_queue_t chunk_queue;
    ws_queue_t ptr_queue;
    arraylist_t reclaim_set;
} jl_gc_markqueue_t;

typedef struct {
    // thread local increment of `perm_scanned_bytes`
    size_t perm_scanned_bytes;
    // thread local increment of `scanned_bytes`
    size_t scanned_bytes;
} jl_gc_mark_cache_t;

typedef struct {
    _Atomic(struct _jl_gc_pagemeta_t *) bottom;
} jl_gc_page_stack_t;

typedef struct {
    jl_thread_heap_t heap;
    jl_gc_page_stack_t page_metadata_allocd;
    jl_gc_markqueue_t mark_queue;
    jl_gc_mark_cache_t gc_cache;
    _Atomic(size_t) gc_sweeps_requested;
    _Atomic(size_t) gc_stack_sweep_requested;
    arraylist_t sweep_objs;
} jl_gc_tls_states_t;

#ifdef __cplusplus
}
#endif

#endif // JL_GC_TLS_H
