// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// GC regions: the runtime interface
// ========================================================================= //
//
// A region is a numbered set of pool pages with its own allocation cursors.
// A thread allocates into region n while a window on n is open
// (jl_gc_region_set); the stock collector never sweeps a region page. The
// design and the rules an application must keep are in
// doc/src/devdocs/gc-regions.md. Every entry point takes region numbers; the
// numbering and its meaning belong to the application.
//
// The stock collector implements the regions (src/gc-regions.c); a build
// with a third-party heap has none.

#ifndef JL_GC_REGIONS_H
#define JL_GC_REGIONS_H

#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

// The refusal codes.
enum {
    JL_GC_REGION_EINVAL = -1,       // a bad region number, or a build that
                                    // cannot allocate in a region
};

#ifndef WITH_THIRD_PARTY_HEAP

// --- the exported API ------------------------------------------------------
// Open a window on region n (n = 0 closes it). Returns the region that was
// current, or a refusal code.
JL_DLLEXPORT int jl_gc_region_set(int n);
JL_DLLEXPORT int jl_gc_region_current(void);

// --- per-heap initialization -------------------------------------------------
void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT;

#endif // WITH_THIRD_PARTY_HEAP

#ifdef __cplusplus
}
#endif

#endif // JL_GC_REGIONS_H
