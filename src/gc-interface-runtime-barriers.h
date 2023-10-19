// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_INTERFACE_RUNTIME_BARRIERS_H
#define JL_GC_INTERFACE_RUNTIME_BARRIERS_H

#include "object-layout.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT void jl_gc_queue_root(const jl_value_t *ptr) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_queue_multiroot(const jl_value_t *parent, const void *ptr, jl_datatype_t *dt) JL_NOTSAFEPOINT;
STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT;
STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT; // ptr isa jl_value_t*
STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif // JL_GC_INTERFACE_RUNTIME_BARRIERS_H
