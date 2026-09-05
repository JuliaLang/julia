// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// A thread allocates into region n while a window on n is open. The stock
// collector marks region objects like any other and never sweeps a region
// page. The rules an application must keep, and why they make the entries
// below sound, are in doc/src/devdocs/gc-regions.md.
//
// The state lives in two places: the per-heap region table in
// jl_thread_heap_t (gc-tls-stock.h) and the page tag region_n in
// jl_gc_pagemeta_t (gc-stock.h). The hooks in the allocator and the sweep
// are in gc-stock.c; each one calls into this file through gc-regions.h.

#include "gc-common.h"
#include "gc-stock.h"
#include "gc-regions.h"

#ifdef __cplusplus
extern "C" {
#endif

STATIC_INLINE int region_valid(int n) JL_NOTSAFEPOINT
{
    return n > 0 && n < JL_GC_MAX_REGIONS;
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
    heap->regions[n] = rs;
}

// Open a window on region n, or close it (n = 0). Every region's cursors
// live in its own array, so the switch is one pointer store; the inlined
// allocation fast path is untouched. Returns the region that was current;
// EINVAL for a bad region number.
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


// --- initialization ----------------------------------------------------------------------

void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT
{
    heap->current_region = 0;
    heap->active_pools = heap->norm_pools;
    memset(heap->regions, 0, sizeof(heap->regions)); // no region has state yet
}

#ifdef __cplusplus
}
#endif
