// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

// Slow paths for `jl_gc_wb` and `jl_gc_multi_wb` respectively.
JL_DLLEXPORT void jl_gc_wb_cold(const void *parent, void *slot, const void *ptr) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_multi_wb_cold(const struct _jl_value_t *parent, void *dest, const void *stored,
                                        struct _jl_datatype_t *dt) JL_NOTSAFEPOINT;

// Whether storing `ptr` into `parent`, an old object not in the remset, must put `parent` in the
// remset. Permanent objects (with the `GC_PERM` bit: image objects and those from `jl_gc_permobj`)
// are never freed and marking does not trace through them, so an edge to one never needs remembering;
// any other edge does if `ptr` is young or `parent` is permanent but not yet in the `image_remset`.
// Codegen's inline barrier (`FinalLowerGC::lowerWriteBarrier`) mirrors this.
// Only `jl_gc_wb_object`, whose callers store no object to check, remembers `parent` regardless.
STATIC_INLINE int jl_gc_wb_must_remember(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    if (ptr == NULL)
        return 0;
    // `ptr` is young 0, old 1 or permanent 9, and `parent` needs 0, or 4 if it is permanent but not
    // in the `image_remset`. Codegen compares the AND of several tags, which takes the minimum.
    uintptr_t level = jl_astaggedvalue(ptr)->header & (8 /* GC_PERM */ | 1 /* GC_MARKED */);
    assert(level != 8 && "permanent object is not marked");
    return level <= (jl_astaggedvalue(parent)->header & 4 /* GC_PERM_NOT_REMSET */);
}

STATIC_INLINE void jl_gc_wb(const void *parent, void *slot, const void *ptr) JL_NOTSAFEPOINT
{
    // parent isa jl_value_t* and ptr isa jl_value_t* or NULL
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */)) // parent is old and not in remset
        jl_gc_wb_cold(parent, slot, ptr);
}

// For a store of something that is not an object (e.g. a stack buffer) into `parent`.
STATIC_INLINE void jl_gc_wb_object(const void *parent) JL_NOTSAFEPOINT // parent isa jl_value_t*
{
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */)) // parent is old and not in remset
        jl_gc_queue_root((jl_value_t*)parent);
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
    if (__likely(jl_astaggedvalue(ptr)->bits.gc == 3 /* GC_OLD_MARKED */ &&
                 !(jl_astaggedvalue(parent)->header & 4 /* GC_PERM_NOT_REMSET */)))
        return; // ptr is old and not in remset (thus it does not point to young)
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(ptr);
    const jl_datatype_layout_t *ly = dt->layout;
    if (ly->npointers)
        jl_gc_multi_wb_cold((jl_value_t*)parent, dest, ptr, dt);
}

STATIC_INLINE void jl_gc_wb_module_usings(const void *mod, const void *from) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_astaggedvalue(mod)->bits.gc == 3 /* GC_OLD_MARKED */) && jl_gc_wb_must_remember(mod, from))
        jl_gc_queue_root((jl_value_t*)mod);
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
    if (n <= JL_GC_COPY_SCAN_MAX_POINTERS) {
        // For small copies into old objects, scan what we just copied and see if any
        // element needs remembering, to avoid adding `dest` to the remset, which saves
        // a full scan of the destination memory at the next GC
        for (size_t i = 0; i < n; i++) {
            if (jl_gc_wb_must_remember(dest_owner, jl_atomic_load_relaxed(dest_p + i))) {
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
    if (n * ly->npointers <= JL_GC_COPY_SCAN_MAX_POINTERS) {
        // For small copies into old objects, scan what we just copied and see if any
        // element needs remembering, to avoid adding `dest` to the remset, which saves
        // a full scan of the destination memory at the next GC
        for (size_t i = 0; i < n; i++) {
            for (uint32_t j = 0; j < ly->npointers; j++) {
                size_t offset = jl_ptr_offset(dt, j);
                if (jl_gc_wb_must_remember(owner, jl_atomic_load_relaxed(dest_p + i * stride + offset))) {
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
