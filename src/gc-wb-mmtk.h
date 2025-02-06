// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  write barriers which should be inlined by the compiler
*/

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

// GC write barriers

// TODO: implement these functions for MMTk
STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
}


#ifdef __cplusplus
}
#endif

#endif
