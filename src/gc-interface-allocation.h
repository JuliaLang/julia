// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_INTERFACE_ALLOCATION_H
#define JL_GC_INTERFACE_ALLOCATION_H

#ifdef __cplusplus
extern "C" {
#endif

#define JL_SMALL_BYTE_ALIGNMENT 16
// JL_HEAP_ALIGNMENT is the maximum alignment that the GC can provide
#define JL_HEAP_ALIGNMENT JL_SMALL_BYTE_ALIGNMENT
#define GC_MAX_SZCLASS (2032-sizeof(void*))
struct _jl_value_t *jl_gc_pool_alloc_noinline(jl_ptls_t ptls, int pool_offset, int osize);
struct _jl_value_t *jl_gc_big_alloc_noinline(jl_ptls_t ptls, size_t allocsz);

#ifdef __cplusplus
}
#endif

#endif // JL_GC_INTERFACE_ALLOCATION_H
