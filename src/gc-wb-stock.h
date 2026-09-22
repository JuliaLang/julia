// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

// This collector remembers the object containing the overwritten field, so it has no use
// for the field's address.
STATIC_INLINE void jl_gc_wb(const void *parent, void *slot JL_UNUSED, const void *ptr) JL_NOTSAFEPOINT
{
    // parent isa jl_value_t* and ptr isa jl_value_t* or NULL
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */)) // parent is old and not in remset
        jl_gc_wb_cold(parent, slot, ptr);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    // if ptr is old
    if (__unlikely(jl_astaggedvalue(ptr)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        jl_gc_queue_root((jl_value_t*)ptr);
    }
}

STATIC_INLINE void jl_gc_wb_finalizer_queue(arraylist_t *queue JL_UNUSED) JL_NOTSAFEPOINT
{
    // this is a deletion, so no barrier is required for this GC
}


// These "special case" stores require no barrier under the stock GC, since the stock GC
// has a purely generational barrier.

// parent is newly allocated since last safepoint
STATIC_INLINE void jl_gc_wb_fresh(const void *parent JL_UNUSED, void *slot JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}
// parent is `jl_current_task`
STATIC_INLINE void jl_gc_wb_current_task(const void *parent JL_UNUSED, void *slot JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}
// ptr is a known old object
STATIC_INLINE void jl_gc_wb_knownold(const void *parent JL_UNUSED, void *slot JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, void *dest, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    // ptr is an immutable object
    if (__likely(jl_astaggedvalue(parent)->bits.gc != 3 /* GC_OLD_MARKED */))
        return; // parent is young or in remset
    if (__unlikely(jl_astaggedvalue(parent)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
        // GC_MARKED optimizations are invalid for generations >= 2
        jl_gc_queue_root((jl_value_t*)parent);
        return;
    }
    if (__likely(jl_astaggedvalue(ptr)->bits.gc == 3 /* GC_OLD_MARKED */))
        return; // ptr is old and not in remset (thus it does not point to young)
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(ptr);
    const jl_datatype_layout_t *ly = dt->layout;
    if (ly->npointers)
        jl_gc_queue_multiroot((jl_value_t*)parent, dest, ptr, dt);
}

STATIC_INLINE void jl_gc_wb_module_usings(const void *mod, const void *from) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_astaggedvalue(mod)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        if (jl_astaggedvalue(mod)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */ ||
            !(jl_astaggedvalue(from)->bits.gc & 1 /* GC_MARKED */))
            jl_gc_queue_root((jl_value_t*)mod);
    }
}

// Maximum number of pointer fields to scan before remembering the owner.
#define JL_GC_COPY_SCAN_MAX_POINTERS 4

STATIC_INLINE void jl_gc_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) *dest_p,
                                                  jl_genericmemory_t *src JL_UNUSED, _Atomic(void*) *src_p,
                                                  size_t n) JL_NOTSAFEPOINT
{
    memmove_refs(dest_p, src_p, n);
    if (n == 0 || __likely(jl_astaggedvalue(dest_owner)->bits.gc != 3 /* GC_OLD_MARKED */))
        return; // destination is young or remembered, or the copy is empty
    if (n <= JL_GC_COPY_SCAN_MAX_POINTERS &&
        __likely(jl_astaggedvalue(dest_owner)->bits.in_image != 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
        // For small copies into old objects, scan what we just copied and see if all the
        // elements were old to avoid adding `dest` to the remset, which saves a full scan
        // of the destination memory at the next GC
        for (size_t i = 0; i < n; i++) {
            void *val = jl_atomic_load_relaxed(dest_p + i);
            if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                jl_gc_queue_root(dest_owner);
                return;
            }
        }
        return;
    }
    jl_gc_queue_root(dest_owner);
    return;
}

STATIC_INLINE void jl_gc_genericmemory_copy_ptr(const jl_value_t *owner, char *destdata,
                                          jl_genericmemory_t *src JL_UNUSED, char *srcdata,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *ly = dt->layout;
    size_t stride = ly->size / sizeof(void*);
    _Atomic(void*) *dest_p = (_Atomic(void*)*)destdata;
    memmove_refs(dest_p, (_Atomic(void*)*)srcdata, n * stride);
    if (n == 0 || __likely(jl_astaggedvalue(owner)->bits.gc != 3 /* GC_OLD_MARKED */))
        return; // destination is young or remembered, or the copy is empty
    if (n * ly->npointers <= JL_GC_COPY_SCAN_MAX_POINTERS &&
        __likely(jl_astaggedvalue(owner)->bits.in_image != 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
        // For small copies into old objects, scan what we just copied and see if all the
        // elements were old to avoid adding `dest` to the remset, which saves a full scan
        // of the destination memory at the next GC
        for (size_t i = 0; i < n; i++) {
            for (uint32_t j = 0; j < ly->npointers; j++) {
                size_t offset = jl_ptr_offset(dt, j);
                void *val = jl_atomic_load_relaxed(dest_p + i * stride + offset);
                if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                    jl_gc_queue_root(owner);
                    return;
                }
            }
        }
        return;
    }
    jl_gc_queue_root(owner);
    return;
}

STATIC_INLINE void jl_gc_genericmemory_clear(const jl_value_t *owner JL_UNUSED,
                                          jl_genericmemory_t *m JL_UNUSED, char *data,
                                          size_t nbytes) JL_NOTSAFEPOINT
{
    // a clear inserts no references, and this collector records only insertions
    memset(data, 0, nbytes);
}

#ifdef __cplusplus
}
#endif

#endif
