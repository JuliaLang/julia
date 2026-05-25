// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

extern void mmtk_object_reference_write_pre(void* mutator, const void* parent, const void* ptr);
extern void mmtk_object_reference_write_post(void* mutator, const void* parent, const void* ptr);
extern void mmtk_object_reference_write_slow(void* mutator, const void* parent, const void* ptr);
extern int mmtk_object_is_managed_by_mmtk(void* addr);
extern void* MMTK_SIDE_LOG_BIT_BASE_ADDRESS;

#define MMTK_OBJECT_NO_BARRIER (0)
#define MMTK_OBJECT_POST_WRITE_BARRIER (1)
#define MMTK_OBJECT_PRE_WRITE_BARRIER (2)
// Stickyimmix needs write barrier. Immix does not need write barrier.
#ifdef MMTK_PLAN_IMMIX
#define MMTK_NEEDS_WRITE_BARRIER (0)
#endif
#ifdef MMTK_PLAN_STICKYIMMIX
#define MMTK_NEEDS_WRITE_BARRIER (1)
#endif
#ifdef MMTK_PLAN_CONCURRENTIMMIX
#define MMTK_NEEDS_WRITE_BARRIER (2)
#endif
#ifdef MMTK_PLAN_STICKYIMMIX_PRE_WRITE
#define MMTK_NEEDS_WRITE_BARRIER (2)
#endif

// // Directly call into MMTk for write barrier (debugging only)
// STATIC_INLINE void mmtk_gc_wb_pre(const void *parent, const void *ptr) JL_NOTSAFEPOINT
// {
//     // Only log objects in the managed (non-immortal) heap. Immortal and sysimage
//     // objects are never collected, so SATB does not need to track them.
//     if (!mmtk_object_is_managed_by_mmtk((void*)ptr))
//         return;
//     jl_task_t *ct = jl_current_task;
//     jl_ptls_t ptls = ct->ptls;
//     mmtk_object_reference_write_pre(&ptls->gc_tls.mmtk_mutator, parent, ptr);
// }

// // Directly call into MMTk for write barrier (debugging only)
// STATIC_INLINE void mmtk_gc_wb_post(const void *parent, const void *ptr) JL_NOTSAFEPOINT
// {
//     jl_task_t *ct = jl_current_task;
//     jl_ptls_t ptls = ct->ptls;
//     mmtk_object_reference_write_post(&ptls->gc_tls.mmtk_mutator, parent, ptr);
// }

STATIC_INLINE void mmtk_gc_log_bit_wb_fast(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
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

STATIC_INLINE void mmtk_gc_wb_pre_fast(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    if (MMTK_NEEDS_WRITE_BARRIER == MMTK_OBJECT_PRE_WRITE_BARRIER) {
        mmtk_gc_log_bit_wb_fast(parent, ptr);
    }
}

// Inlined fastpath
STATIC_INLINE void mmtk_gc_wb_post_fast(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    if (MMTK_NEEDS_WRITE_BARRIER == MMTK_OBJECT_POST_WRITE_BARRIER) {
        mmtk_gc_log_bit_wb_fast(parent, ptr);
    }
}

STATIC_INLINE void jl_gc_wb_pre(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_pre_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_post(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_post_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_back_post(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    mmtk_gc_wb_post_fast(ptr, (void*)0);
}

STATIC_INLINE void jl_gc_wb_back_pre(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    mmtk_gc_wb_pre_fast(ptr, (void*)0);
}

STATIC_INLINE void jl_gc_wb_fresh_pre(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT
{
    // Fresh allocations do not yet have any references to track; no pre-barrier needed.
    // NOTE: mmtk_gc_wb_pre accesses jl_current_task which is not yet set up
    // during early Julia initialization (before jl_init_root_task), so this
    // cannot safely call mmtk_gc_wb_pre.
}

STATIC_INLINE void jl_gc_wb_fresh_post(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT
{
    // No post-barrier needed for fresh allocations since MMTK_OBJECT_PRE_WRITE_BARRIER
    // plans do not use a post-barrier.
}

STATIC_INLINE void jl_gc_wb_current_task_post(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_post_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_current_task_pre(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_pre_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_knownold(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    __builtin_unreachable();
}

STATIC_INLINE void jl_gc_multi_wb_pre(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_pre_fast(parent, (void*)0);
}

STATIC_INLINE void jl_gc_multi_wb_post(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_post_fast(parent, (void*)0);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_boxed_pre(const jl_value_t *dest_owner, _Atomic(void*) * dest_p,
                                          jl_genericmemory_t *src, _Atomic(void*) * src_p,
                                          size_t* n) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_pre_fast(dest_owner, (void*)0);
}
STATIC_INLINE void jl_gc_wb_genericmemory_copy_boxed_post(const jl_value_t *dest_owner, _Atomic(void*) * dest_p,
                                          jl_genericmemory_t *src, _Atomic(void*) * src_p,
                                          size_t* n) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_post_fast(dest_owner, (void*)0);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_ptr_pre(const jl_value_t *owner, char* srcdata,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_pre_fast(owner, (void*)0);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_ptr_post(const jl_value_t *owner, jl_genericmemory_t *src, char* src_p,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_post_fast(owner, (void*)0);
}


#ifdef __cplusplus
}
#endif

#endif
