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

STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    mmtk_gc_wb_fast(ptr, (void*)0);
}

// The three annotated-store barriers (see gc-interface.h for what each one asserts).
//
// Each assertion is about the value being *stored*, so each is a reason a generational
// plan has nothing to remember. A plan whose barrier must also observe the value being
// *displaced* -- MMTK_SNAPSHOT_BARRIER -- gets no such licence from two of the three, and
// has to take the full barrier instead.

// `parent` is younger than the last safepoint. A generational plan need not remember it.
// A snapshot barrier need not either: marking can only have begun at a safepoint, so a
// live snapshot cannot contain any field of `parent`, and the values being displaced are
// the uninitialised ones the allocator left behind.
STATIC_INLINE void jl_gc_wb_fresh(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}

#ifdef MMTK_SNAPSHOT_BARRIER
// Being in a remset means the parent will be *rescanned*, which recovers references
// inserted into it but not references removed from it: the rescan observes the field
// after the store. A reference moved out of the current task and into an object the
// collector has already blackened is then reachable only through a location the snapshot
// never saw, and is collected while live.
STATIC_INLINE void jl_gc_wb_current_task(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, ptr);
}

// That `ptr` is old says nothing about the reference it displaces, which is the one a
// snapshot barrier has to record.
STATIC_INLINE void jl_gc_wb_knownold(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, ptr);
}
#else
STATIC_INLINE void jl_gc_wb_current_task(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}
STATIC_INLINE void jl_gc_wb_knownold(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}
#endif

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    mmtk_gc_wb_fast(parent, (void*)0);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) ** dest_pp,
                                          jl_genericmemory_t *src, _Atomic(void*) ** src_pp,
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
