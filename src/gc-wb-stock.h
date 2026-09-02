// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

// This collector records the modified object, not the field. Thus it does not use `slot`.
STATIC_INLINE void jl_gc_wb(const void *parent, void *slot JL_UNUSED, const void *ptr) JL_NOTSAFEPOINT
{
    // parent isa jl_value_t* and ptr isa jl_value_t* or NULL
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */)) // parent is old and not in remset
        jl_gc_wb_cold(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    // if ptr is old
    if (__unlikely(jl_astaggedvalue(ptr)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        jl_gc_queue_root((jl_value_t*)ptr);
    }
}

// The annotated stores (see gc-interface.h). This collector only records old parents
// that hold young children. Each property is a reason that there is nothing to record:
//  - A fresh parent is still young.
//  - The marker keeps each old task in the remset (see gc-stock.c). Thus the collector
//    scans the fields of a task, and the JL_GC_PUSH_* roots on its stack, in each
//    collection.
//  - An old `ptr` is not a young child.
STATIC_INLINE void jl_gc_wb_fresh(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}
STATIC_INLINE void jl_gc_wb_current_task(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}
STATIC_INLINE void jl_gc_wb_knownold(const void *parent JL_UNUSED, const void *ptr JL_UNUSED) JL_NOTSAFEPOINT {}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
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
        jl_gc_queue_multiroot((jl_value_t*)parent, ptr, dt);
}

STATIC_INLINE void jl_gc_wb_module_usings(const void *mod, const void *from) JL_NOTSAFEPOINT
{
    // records the module: the only location this collector can name anyway
    if (__unlikely(jl_astaggedvalue(mod)->bits.gc == 3 /* GC_OLD_MARKED */))
        jl_gc_wb_cold(mod, from);
}

STATIC_INLINE void jl_gc_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) *dest_p,
                                          jl_genericmemory_t *src, _Atomic(void*) *src_p,
                                          size_t n) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_astaggedvalue(dest_owner)->bits.gc == 3 /* GC_OLD_MARKED */ )) {
        if (__unlikely(jl_astaggedvalue(dest_owner)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
            // GC_MARKED optimizations are invalid for generations >= 2
            jl_gc_queue_root(dest_owner);
        }
        else if (jl_astaggedvalue(jl_genericmemory_owner(src))->bits.gc != 3 /* GC_OLD_MARKED */) {
            // check each value as it is stored, until the first young one queues the
            // owner; the rest of the copy then needs no checks
            size_t done = 0;
            if (dest_p < src_p || dest_p > src_p + n) {
                for (; done < n; done++) { // copy forwards
                    void *val = jl_atomic_load_relaxed(src_p + done);
                    jl_atomic_store_release(dest_p + done, val);
                    // `val` is young or old-unmarked (or dest is image and val is non-image)
                    if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                        jl_gc_queue_root(dest_owner);
                        ++done;
                        break;
                    }
                }
                dest_p += done;
                src_p += done;
            }
            else {
                for (; done < n; done++) { // copy backwards
                    void *val = jl_atomic_load_relaxed(src_p + n - done - 1);
                    jl_atomic_store_release(dest_p + n - done - 1, val);
                    // `val` is young or old-unmarked (or dest is image and val is non-image)
                    if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                        jl_gc_queue_root(dest_owner);
                        ++done;
                        break;
                    }
                }
            }
            n -= done;
        }
    }
    memmove_refs(dest_p, src_p, n);
}

STATIC_INLINE void jl_gc_genericmemory_copy_ptr(const jl_value_t *owner, char *destdata,
                                          jl_genericmemory_t *src, char *srcdata,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    size_t elsz = dt->layout->size;
    memmove_refs((_Atomic(void*)*)destdata, (_Atomic(void*)*)srcdata, n * elsz / sizeof(void*));
    if (__unlikely(jl_astaggedvalue(owner)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        if (__unlikely(jl_astaggedvalue(owner)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
            // GC_MARKED optimizations are invalid for generations >= 2
            jl_gc_queue_root(owner);
            return;
        }
        jl_value_t *src_owner = jl_genericmemory_owner(src);
        if (jl_astaggedvalue(src_owner)->bits.gc != 3 /* GC_OLD_MARKED */) {
            // check the values that were actually stored, so that a store racing with
            // this copy cannot slip an unrecorded young reference into the destination
            jl_datatype_t *ety = (jl_datatype_t*)jl_tparam1(dt);
            for (size_t done = 0; done < n; done++) {
                char *s = destdata + done * elsz;
                if (*((jl_value_t**)s + ety->layout->first_ptr) != NULL)
                    jl_gc_queue_multiroot(owner, s, ety);
            }
        }
    }
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
