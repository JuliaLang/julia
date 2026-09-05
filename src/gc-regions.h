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
// with a third-party heap gets the stubs at the end of this file, so the
// callers in gf.c and jltypes.c compile unchanged.

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

// --- the hooks the rest of the runtime calls --------------------------------
// Install a parked region on a thread: the stock collection parks every
// window before it runs and installs it again after.
void jl_gc_region_install_task(jl_ptls_t ptls, int n) JL_NOTSAFEPOINT;
// The brackets around a stock collection: park every open window before it,
// install the windows again after it. Between them, every pass of the
// collection clears the marks it left on region pages after its sweep.
void jl_gc_region_prepare_stock_collection(void) JL_NOTSAFEPOINT;
void jl_gc_region_clear_stock_marks(void) JL_NOTSAFEPOINT;
void jl_gc_region_finish_stock_collection(void) JL_NOTSAFEPOINT;
// Per-heap initialization.
void jl_gc_region_init_heap(jl_thread_heap_t *heap) JL_NOTSAFEPOINT;

#else // WITH_THIRD_PARTY_HEAP

// A third-party heap has no regions: every window is refused.
STATIC_INLINE int jl_gc_region_set(int n) { (void)n; return JL_GC_REGION_EINVAL; }
STATIC_INLINE int jl_gc_region_current(void) { return 0; }

#endif // WITH_THIRD_PARTY_HEAP

#ifdef __cplusplus
}
#endif

#endif // JL_GC_REGIONS_H
