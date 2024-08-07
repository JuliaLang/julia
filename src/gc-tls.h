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
    // variable for tracking weak references
    small_arraylist_t weak_refs;
    // live tasks started on this thread
    // that are holding onto a stack from the pool
    small_arraylist_t live_tasks;

    // variables for tracking malloc'd arrays
    struct _mallocmemory_t *mallocarrays;
    struct _mallocmemory_t *mafreelist;

    // variable for tracking young (i.e. not in `GC_OLD_MARKED`/last generation) large objects
    struct _bigval_t *young_generation_of_bigvals;

    // lower bound of the number of pointers inside remembered values
    int remset_nptr;
    // remembered set
    arraylist_t remset;

    // variables for allocating objects from pools
#define JL_GC_N_MAX_POOLS 51 // conservative. must be kept in sync with `src/julia_internal.h`
    jl_gc_pool_t norm_pools[JL_GC_N_MAX_POOLS];

#define JL_N_STACK_POOLS 16
    small_arraylist_t free_stacks[JL_N_STACK_POOLS];
} jl_thread_heap_t;

typedef struct {
    _Atomic(int64_t) allocd;
    _Atomic(int64_t) pool_live_bytes;
    _Atomic(uint64_t) malloc;
    _Atomic(uint64_t) realloc;
    _Atomic(uint64_t) poolalloc;
    _Atomic(uint64_t) bigalloc;
    _Atomic(int64_t) free_acc;
    _Atomic(uint64_t) alloc_acc;
} jl_thread_gc_num_t;

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
    jl_thread_gc_num_t gc_num;
    jl_gc_markqueue_t mark_queue;
    jl_gc_mark_cache_t gc_cache;
    _Atomic(size_t) gc_sweeps_requested;
    arraylist_t sweep_objs;
} jl_gc_tls_states_t;

#ifdef __cplusplus
}
#endif

#endif // JL_GC_TLS_H
