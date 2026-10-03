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

#ifdef WITH_GC_REGIONS
    // The GC regions of this heap (gc-regions.h). Region 0 is the default
    // heap: norm_pools, and the pages the stock collector sweeps.
#define JL_GC_MAX_REGIONS 64
    uint8_t current_region;      // the region the thread allocates into
    uint8_t saved_region;        // the window parked by a stock collection
    uint8_t finalizer_depth;     // > 0 while finalizers run on this thread
    jl_gc_pool_t *active_pools;  // norm_pools, or the pools of the current region
    char *pool_base;             // active_pools less the offset of norm_pools in the
                                 // TLS: the pool of an allocation offset is pool_base + offset
    struct _jl_gc_region_state_t *regions[JL_GC_MAX_REGIONS]; // NULL until used; NULL for region 0
    uint64_t region_live_mask;     // bit r: region r is live on this heap
    uint64_t region_haschild_mask; // bit r: region r has a live child region
    uint8_t region_child_count[JL_GC_MAX_REGIONS];
#endif
} jl_thread_heap_t;

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
    // dead objects requiring weak processing (currently:
    // dead-but-still-linked cancellation sources) found by this thread
    // during the current sweep; processed (and the list drained) by
    // sweep_weak_processing before the world restarts
    small_arraylist_t weak_processing_objs;
} jl_gc_tls_states_t;

#ifdef __cplusplus
}
#endif

#endif // JL_GC_TLS_H
