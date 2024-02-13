// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-interface-collector.h"
#include "julia.h"
#include "julia_threads.h"
#include "julia_internal.h"
#include "threading.h"
#ifdef __GLIBC__
#include <malloc.h> // for malloc_trim
#endif

#ifndef JL_GC_COMMON_H
#define JL_GC_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif

// This struct must be kept in sync with the Julia type of the same name in base/timing.jl
typedef struct {
    int64_t     allocd;
    int64_t     deferred_alloc;
    int64_t     freed;
    uint64_t    malloc;
    uint64_t    realloc;
    uint64_t    poolalloc;
    uint64_t    bigalloc;
    uint64_t    freecall;
    uint64_t    total_time;
    uint64_t    total_allocd;
    size_t      interval;
    int         pause;
    int         full_sweep;
    uint64_t    max_pause;
    uint64_t    max_memory;
    uint64_t    time_to_safepoint;
    uint64_t    max_time_to_safepoint;
    uint64_t    total_time_to_safepoint;
    uint64_t    sweep_time;
    uint64_t    mark_time;
    uint64_t    total_sweep_time;
    uint64_t    total_mark_time;
    uint64_t    last_full_sweep;
    uint64_t    last_incremental_sweep;
} jl_gc_num_t;

extern jl_gc_num_t gc_num;

typedef struct {
    _Atomic(size_t) bytes_mapped;
    _Atomic(size_t) bytes_resident;
    _Atomic(size_t) heap_size;
    _Atomic(size_t) heap_target;
} gc_heapstatus_t;

extern gc_heapstatus_t gc_heap_stats;

STATIC_INLINE void jl_batch_accum_heap_size(jl_ptls_t ptls, uint64_t sz) JL_NOTSAFEPOINT
{
    uint64_t alloc_acc = jl_atomic_load_relaxed(&ptls->gc_num.alloc_acc) + sz;
    if (alloc_acc < 16*1024)
        jl_atomic_store_relaxed(&ptls->gc_num.alloc_acc, alloc_acc);
    else {
        jl_atomic_fetch_add_relaxed(&gc_heap_stats.heap_size, alloc_acc);
        jl_atomic_store_relaxed(&ptls->gc_num.alloc_acc, 0);
    }
}

STATIC_INLINE void jl_batch_accum_free_size(jl_ptls_t ptls, uint64_t sz) JL_NOTSAFEPOINT
{
    jl_atomic_store_relaxed(&ptls->gc_num.free_acc, jl_atomic_load_relaxed(&ptls->gc_num.free_acc) + sz);
}

extern int64_t live_bytes;
extern uint64_t freed_in_runtime;
void combine_thread_gc_counts(jl_gc_num_t *dest, int update_heap) JL_NOTSAFEPOINT;
void reset_thread_gc_counts(void) JL_NOTSAFEPOINT;
int64_t inc_live_bytes(int64_t inc) JL_NOTSAFEPOINT;

STATIC_INLINE uintptr_t gc_ptr_tag(void *v, uintptr_t mask) JL_NOTSAFEPOINT
{
    return ((uintptr_t)v) & mask;
}

STATIC_INLINE void *gc_ptr_clear_tag(void *v, uintptr_t mask) JL_NOTSAFEPOINT
{
    return (void*)(((uintptr_t)v) & ~mask);
}

void schedule_finalization(void *o, void *f) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif
