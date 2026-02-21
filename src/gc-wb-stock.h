// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    // parent and ptr isa jl_value_t*
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */ && // parent is old and not in remset
                   (jl_astaggedvalue(ptr)->bits.gc & 1 /* GC_MARKED */) == 0)) // ptr is young
        jl_gc_queue_root((jl_value_t*)parent);
}

STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT // ptr isa jl_value_t*
{
    // if ptr is old
    if (__unlikely(jl_astaggedvalue(ptr)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        jl_gc_queue_root((jl_value_t*)ptr);
    }
}

STATIC_INLINE void jl_gc_multi_wb(const void *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
{
    // 3 == GC_OLD_MARKED
    // ptr is an immutable object
    if (__likely(jl_astaggedvalue(parent)->bits.gc != 3))
        return; // parent is young or in remset
    if (__likely(jl_astaggedvalue(ptr)->bits.gc == 3))
        return; // ptr is old and not in remset (thus it does not point to young)
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(ptr);
    const jl_datatype_layout_t *ly = dt->layout;
    if (ly->npointers)
        jl_gc_queue_multiroot((jl_value_t*)parent, ptr, dt);
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) * dest_p,
                                          jl_genericmemory_t *src, _Atomic(void*) * src_p,
                                          size_t* n) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_astaggedvalue(dest_owner)->bits.gc == 3 /* GC_OLD_MARKED */ )) {
        jl_value_t *src_owner = jl_genericmemory_owner(src);
        size_t done = 0;
        if (jl_astaggedvalue(src_owner)->bits.gc != 3 /* GC_OLD_MARKED */) {
            if (dest_p < src_p || dest_p > src_p + (*n)) {
                for (; done < (*n); done++) { // copy forwards
                    void *val = jl_atomic_load_relaxed(src_p + done);
                    jl_atomic_store_release(dest_p + done, val);
                    // `val` is young or old-unmarked
                    if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                        jl_gc_queue_root(dest_owner);
                        break;
                    }
                }
                src_p += done;
                dest_p += done;
            }
            else {
                for (; done < (*n); done++) { // copy backwards
                    void *val = jl_atomic_load_relaxed(src_p + (*n) - done - 1);
                    jl_atomic_store_release(dest_p + (*n) - done - 1, val);
                    // `val` is young or old-unmarked
                    if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                        jl_gc_queue_root(dest_owner);
                        break;
                    }
                }
            }
            (*n) -= done;
        }
    }
}

STATIC_INLINE void jl_gc_wb_genericmemory_copy_ptr(const jl_value_t *owner, jl_genericmemory_t *src, char* src_p,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_astaggedvalue(owner)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        jl_value_t *src_owner = jl_genericmemory_owner(src);
        size_t elsz = dt->layout->size;
        if (jl_astaggedvalue(src_owner)->bits.gc != 3 /* GC_OLD_MARKED */) {
            dt = (jl_datatype_t*)jl_tparam1(dt);
            for (size_t done = 0; done < n; done++) { // copy forwards
                char* s = (char*)src_p+done*elsz;
                if (*((jl_value_t**)s+dt->layout->first_ptr) != NULL)
                    jl_gc_queue_multiroot(owner, s, dt);
            }
        }
    }
}

#ifdef __cplusplus
}
#endif

#endif
