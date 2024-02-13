// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifdef THIRD_PARY_GC
#error "This file should not be compiled with MMTK GC"
#endif

#ifndef JL_GC_IMPL_RUNTIME_BARRIERS_H
#define JL_GC_IMPL_RUNTIME_BARRIERS_H

#include "gc-interface-runtime-barriers.h"
#include "object-layout.h"

#ifdef __cplusplus
extern "C" {
#endif

STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    // parent and ptr isa jl_value_t*
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 && // parent is old and not in remset
                   (jl_astaggedvalue(ptr)->bits.gc & 1) == 0)) // ptr is young
        jl_gc_queue_root((jl_value_t*)parent);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    // if ptr is old
    if (__unlikely(jl_astaggedvalue(ptr)->bits.gc == 3)) {
        jl_gc_queue_root((jl_value_t*)ptr);
    }
}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    // 3 == GC_OLD_MARKED
    // ptr is an immutable object
    if (__likely(jl_astaggedvalue(parent)->bits.gc != 3))
        return; // parent is young or in remset
    if (__likely(jl_astaggedvalue(ptr)->bits.gc == 3))
        return; // ptr is old and not in remset (thus it does not point to young)
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(ptr);
    const jl_datatype_layout_t *ly = dt->layout;
    if (ly->npointers)
        jl_gc_queue_multiroot((jl_value_t*)parent, ptr, dt);
}

#ifdef __cplusplus
}
#endif

#endif // JL_GC_IMPL_RUNTIME_BARRIERS_H
