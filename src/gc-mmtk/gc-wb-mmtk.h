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
extern void mmtk_object_reference_write_slow(void* mutator, const void* parent, const void* ptr);
extern void* MMTK_SIDE_LOG_BIT_BASE_ADDRESS;

#define MMTK_OBJECT_BARRIER (1)
// Stickyimmix needs write barrier. Immix does not need write barrier.
#ifdef MMTK_PLAN_IMMIX
#define MMTK_NEEDS_WRITE_BARRIER (0)
#endif
#ifdef MMTK_PLAN_STICKYIMMIX
#define MMTK_NEEDS_WRITE_BARRIER (1)
#endif
// ConcurrentImmix uses a SATB barrier. Since every write barrier is now emitted
// before the store, the same inlined log-bit check works: when the parent's log
// bit is set, the slow path can snapshot its still-current fields.
#ifdef MMTK_PLAN_CONCURRENTIMMIX
#define MMTK_NEEDS_WRITE_BARRIER (1)
#endif

// Directly call into MMTk for write barrier (debugging only). The pre entry is
// emitted before the store, which is correct for both StickyImmix and
// ConcurrentImmix.
STATIC_INLINE void mmtk_gc_wb_full(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    mmtk_object_reference_write_pre(&ptls->gc_tls.mmtk_mutator, parent, ptr);
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

STATIC_INLINE void jl_gc_wb(const void *parent, void *slot JL_UNUSED, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    mmtk_gc_wb_fast(ptr, (void*)0);
}

STATIC_INLINE void jl_gc_wb_fresh(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}

STATIC_INLINE void jl_gc_wb_current_task(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
#ifdef MMTK_SNAPSHOT_BARRIER
    mmtk_gc_wb_fast(parent, ptr);
#endif
}

STATIC_INLINE void jl_gc_wb_knownold(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
#ifdef MMTK_SNAPSHOT_BARRIER
    mmtk_gc_wb_fast(parent, ptr);
#endif
}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, (void*)0);
}

STATIC_INLINE void jl_gc_wb_module_usings(const void *mod, const void *from) JL_NOTSAFEPOINT
{
    // logs the module: the only location this collector can name anyway
    mmtk_gc_wb_fast(mod, from);
}

// The fused operations log the whole destination object before its elements change,
// which covers both the insertion side (StickyImmix) and the snapshot of the
// overwritten values (ConcurrentImmix).

STATIC_INLINE void jl_gc_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) *dest_p,
                                          jl_genericmemory_t *src, _Atomic(void*) *src_p,
                                          size_t n) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(dest_owner, (void*)0);
    memmove_refs(dest_p, src_p, n);
}

STATIC_INLINE void jl_gc_genericmemory_copy_ptr(const jl_value_t *owner, char *destdata,
                                          jl_genericmemory_t *src, char *srcdata,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(owner, (void*)0);
    memmove_refs((_Atomic(void*)*)destdata, (_Atomic(void*)*)srcdata, n * dt->layout->size / sizeof(void*));
}

STATIC_INLINE void jl_gc_genericmemory_clear(const jl_value_t *owner JL_UNUSED,
                                          jl_genericmemory_t *m JL_UNUSED, char *data,
                                          size_t nbytes) JL_NOTSAFEPOINT
{
#ifdef MMTK_SNAPSHOT_BARRIER
    // a deletion barrier must snapshot the overwritten references before the clear
    mmtk_gc_wb_fast(owner, (void*)0);
#endif
    memset(data, 0, nbytes);
}


#ifdef __cplusplus
}
#endif

#endif
