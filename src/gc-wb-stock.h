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

STATIC_INLINE void jl_gc_wb(const void *parent, void *slot, const void *ptr) JL_NOTSAFEPOINT
{
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_check(parent, ptr);
#endif
    // parent isa jl_value_t* and ptr isa jl_value_t* or NULL
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */)) // parent is old and not in remset
        jl_gc_wb_cold(parent, slot, ptr);
}

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
#ifdef WITH_GC_REGIONS
// With the regions the barrier is not purely generational: the region check runs at each of
// them, because none of the three properties says anything about the region of `ptr`.
#endif

// parent is newly allocated since last safepoint
STATIC_INLINE void jl_gc_wb_fresh(const void *parent JL_UNUSED, void *slot JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT
{
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_check(parent, ptr);
#endif
}
// parent is `jl_current_task`
STATIC_INLINE void jl_gc_wb_current_task(const void *parent JL_UNUSED, void *slot JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT
{
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_check(parent, ptr);
#endif
}
// ptr is a known old object
STATIC_INLINE void jl_gc_wb_knownold(const void *parent JL_UNUSED, void *slot JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT
{
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_check(parent, ptr);
#endif
}
#ifdef WITH_GC_REGIONS
STATIC_INLINE void jl_gc_multi_wb_fresh(const void *parent, const void *data, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_inline_check(parent, data, dt);
}
#endif

STATIC_INLINE void jl_gc_multi_wb(const void *parent, void *dest, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
#ifdef WITH_GC_REGIONS
    // The pointer fields of `ptr` are what the copy puts into `parent`: the pair check, then
    // the fields when the pair fails (jl_gc_region_wb_copy_inline_check in gc-interface.h).
    jl_gc_region_wb_copy_inline_check(parent, (const void*)ptr, (const char*)ptr, 1, 0,
                                      (jl_datatype_t*)jl_typeof(ptr));
#endif
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
        jl_gc_multi_wb_cold((jl_value_t*)parent, dest, ptr, dt);
}

STATIC_INLINE void jl_gc_wb_module_usings(const void *mod, const void *from) JL_NOTSAFEPOINT
{
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_check(mod, from);
#endif
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
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_copy_boxed_check(dest_owner, src, src_p, n);
#endif
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
#ifdef WITH_GC_REGIONS
    jl_gc_region_wb_copy_inline_check(owner, src, srcdata, n, ly->size, (jl_datatype_t*)jl_tparam1(dt));
#endif
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
