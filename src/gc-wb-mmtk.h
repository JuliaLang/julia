// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

extern void mmtk_object_reference_write_post(void* mutator, const void* parent, const void* ptr);
extern void mmtk_object_reference_write_slow(void* mutator, const void* parent, const void* ptr);
extern const void* MMTK_SIDE_LOG_BIT_BASE_ADDRESS;

#define MMTK_OBJECT_BARRIER (1)
// Stickyimmix needs write barrier. Immix does not need write barrier.
#ifdef MMTK_PLAN_IMMIX
#define MMTK_NEEDS_WRITE_BARRIER (0)
#endif
#ifdef MMTK_PLAN_STICKYIMMIX
#define MMTK_NEEDS_WRITE_BARRIER (1)
#endif

// Directly call into MMTk for write barrier (debugging only)
STATIC_INLINE void mmtk_gc_wb_full(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    mmtk_object_reference_write_post(&ptls->gc_tls.mmtk_mutator, parent, ptr);
}

// Inlined fastpath
STATIC_INLINE void mmtk_gc_wb_fast(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    if (MMTK_NEEDS_WRITE_BARRIER == MMTK_OBJECT_BARRIER) {
        intptr_t addr = (intptr_t) (void*) parent;
        uint8_t* meta_addr = (uint8_t*) (MMTK_SIDE_LOG_BIT_BASE_ADDRESS) + (addr >> 6);
        intptr_t shift = (addr >> 3) & 0b111;
        uint8_t byte_val = *meta_addr;
        if (((byte_val >> shift) & 1) == 1) {
            jl_task_t *ct = jl_current_task;
            jl_ptls_t ptls = ct->ptls;
            mmtk_object_reference_write_slow(&ptls->gc_tls.mmtk_mutator, parent, ptr);
        }
    }
}

STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    mmtk_gc_wb_fast(ptr, (void*)0);
}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, (void*)0);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) * dest_p,
                                          jl_genericmemory_t *src, _Atomic(void*) * src_p,
                                          size_t* n) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(dest_owner, (void*)0);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_ptr(const jl_value_t *owner, jl_genericmemory_t *src, char* src_p,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(owner, (void*)0);
}


#ifdef __cplusplus
}
#endif

#endif
