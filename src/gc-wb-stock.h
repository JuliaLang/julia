// This file is a part of Julia. License is MIT: https://julialang.org/license

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

#ifndef JL_GC_WB_H
#define JL_GC_WB_H

#ifdef __cplusplus
extern "C" {
#endif

// The escape barrier of the GC regions (gc-regions.h): armed at the first
// window, it compares the page tags of parent and child. Disarmed it costs
// one load-and-branch per store; a build with JL_NO_REGION_STORE_BARRIER
// defined leaves it out of every barrier, here and in the generated code.
extern JL_DLLIMPORT _Atomic(uint8_t) jl_gc_region_barrier_on;
JL_DLLEXPORT void jl_gc_region_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT;
// The test of the barrier with no quarantine, and the element-by-element
// checks of a bulk copy (gc-regions.c).
JL_DLLEXPORT int jl_gc_region_would_escape(const void *parent, const void *ptr) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_region_wb_boxed(const void *parent, _Atomic(void*) *src, size_t n) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gc_region_wb_inline(const void *parent, const char *src, size_t n,
                                         size_t elsz, jl_datatype_t *et) JL_NOTSAFEPOINT;
#ifndef JL_NO_REGION_STORE_BARRIER
#define jl_gc_region_wb_check(parent, ptr) do {                         \
        if (__unlikely(jl_atomic_load_relaxed(&jl_gc_region_barrier_on))) \
            jl_gc_region_wb((parent), (ptr));                           \
    } while (0)
// A bulk copy needs one region check, not one per element. Every element of
// the source keeps the reference rule against the source, so it is in the
// source's region or in an ancestor of it. If the source is legal under the
// destination, every element is legal under it too, because the ancestors of
// a legal region are legal. So the check of the pair (destination, source)
// covers the whole copy, and the copy keeps its speed. The converse does
// not hold: a young source of old elements -- a filter or a copy made inside
// a window, appended to an old vector after the window closed -- fails the
// pair check and is legal. So a failed pair check decides nothing; the
// elements are checked one by one, and only a real escape quarantines.
#define jl_gc_region_wb_copy_boxed_check(parent, src, src_p, n) do {     \
        if (__unlikely(jl_atomic_load_relaxed(&jl_gc_region_barrier_on)) && \
            __unlikely(jl_gc_region_would_escape((parent), (src))))     \
            jl_gc_region_wb_boxed((parent), (src_p), (n));              \
    } while (0)
#define jl_gc_region_wb_copy_inline_check(parent, src, src_p, n, elsz, et) do { \
        if (__unlikely(jl_atomic_load_relaxed(&jl_gc_region_barrier_on)) && \
            __unlikely(jl_gc_region_would_escape((parent), (src))))     \
            jl_gc_region_wb_inline((parent), (src_p), (n), (elsz), (et)); \
    } while (0)
// The pointer fields of one inline value whose bytes are not a heap object
// -- a field, an element, a stack buffer -- so no pair check stands in for
// them: each one is checked when the flag is armed.
#define jl_gc_region_wb_inline_check(parent, src_p, et) do {             \
        if (__unlikely(jl_atomic_load_relaxed(&jl_gc_region_barrier_on))) \
            jl_gc_region_wb_inline((parent), (const char*)(src_p), 1, 0, (et)); \
    } while (0)
#else
#define jl_gc_region_wb_check(parent, ptr) do { } while (0)
#define jl_gc_region_wb_copy_boxed_check(parent, src, src_p, n) do { } while (0)
#define jl_gc_region_wb_copy_inline_check(parent, src, src_p, n, elsz, et) do { } while (0)
#define jl_gc_region_wb_inline_check(parent, src_p, et) do { } while (0)
#endif

STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    // parent and ptr isa jl_value_t*
    jl_gc_region_wb_check(parent, ptr);
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc == 3 /* GC_OLD_MARKED */ && // parent is old and not in remset
                   (jl_astaggedvalue(parent)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */ || // parent in image and not in remset
                    (jl_astaggedvalue(ptr)->bits.gc & 1 /* GC_MARKED */) == 0))) // ptr is young
        jl_gc_queue_root((jl_value_t*)parent);
}

// Three of the four annotations of gc-interface.h. Each one names a store
// whose generational half is unnecessary, so the body here is the region
// check alone: a fresh parent takes the region of the open window, the
// current task is a region-0 object in every supported program, and an old
// child says nothing about which region holds it.
STATIC_INLINE void jl_gc_wb_fresh(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_check(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_current_task(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_check(parent, ptr);
}

STATIC_INLINE void jl_gc_wb_knownold(const void *parent, const void *ptr) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_check(parent, ptr);
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
    // ptr is an immutable object; its pointer fields are what the store puts
    // into parent, so they decide when the pair check fails (see the bulk
    // copy below).
    jl_gc_region_wb_copy_inline_check(parent, (const void*)ptr, (const char*)ptr, 1, 0,
                                      (jl_datatype_t*)jl_typeof(ptr));
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

// A copy of the bytes of an inline value into a fresh object (jl_new_bits and
// the atomic field reads): the fourth annotation of gc-interface.h, so the
// body is the region check alone. `data` is a field, an element or a stack
// buffer, not an object, so its pointer fields are checked one by one.
STATIC_INLINE void jl_gc_multi_wb_fresh(const void *parent, const void *data, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_inline_check(parent, data, dt);
}

// The region check of a bulk copy is the pair check above, then the
// elements when the pair fails.
STATIC_INLINE void jl_gc_wb_genericmemory_copy_boxed(const jl_value_t *dest_owner, _Atomic(void*) ** dest_pp,
                                          jl_genericmemory_t *src, _Atomic(void*) ** src_pp,
                                          size_t* n) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_copy_boxed_check(dest_owner, src, *src_pp, *n);
    if (__unlikely(jl_astaggedvalue(dest_owner)->bits.gc == 3 /* GC_OLD_MARKED */ )) {
        jl_value_t *src_owner = jl_genericmemory_owner(src);
        size_t done = 0;
        if (__unlikely(jl_astaggedvalue(dest_owner)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
            // GC_MARKED optimizations are invalid for generations >= 2
            jl_gc_queue_root(dest_owner);
            return;
        }
        if (jl_astaggedvalue(src_owner)->bits.gc != 3 /* GC_OLD_MARKED */) {
            _Atomic(void*) *dest_p = *dest_pp;
            _Atomic(void*) *src_p = *src_pp;
            if (dest_p < src_p || dest_p > src_p + (*n)) {
                for (; done < (*n); done++) { // copy forwards
                    void *val = jl_atomic_load_relaxed(src_p + done);
                    jl_atomic_store_release(dest_p + done, val);
                    // `val` is young or old-unmarked (or dest is image and val is non-image)
                    if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                        jl_gc_queue_root(dest_owner);
                        ++done;
                        break;
                    }
                }
                // advance caller's pointers past the elements we just
                // copied so the trailing memmove_refs picks up where we
                // left off
                *src_pp = src_p + done;
                *dest_pp = dest_p + done;
            }
            else {
                for (; done < (*n); done++) { // copy backwards
                    void *val = jl_atomic_load_relaxed(src_p + (*n) - done - 1);
                    jl_atomic_store_release(dest_p + (*n) - done - 1, val);
                    // `val` is young or old-unmarked (or dest is image and val is non-image)
                    if (val && !(jl_astaggedvalue(val)->bits.gc & 1 /* GC_MARKED */)) {
                        jl_gc_queue_root(dest_owner);
                        ++done;
                        break;
                    }
                }
            }
            (*n) -= done;
        }
    }
}

// The same for inline elements with pointer fields; `dt` is the memory type.
STATIC_INLINE void jl_gc_wb_genericmemory_copy_ptr(const jl_value_t *owner, jl_genericmemory_t *src, char* src_p,
                                          size_t n, jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    jl_gc_region_wb_copy_inline_check(owner, src, src_p, n, dt->layout->size,
                                      (jl_datatype_t*)jl_tparam1(dt));
    if (__unlikely(jl_astaggedvalue(owner)->bits.gc == 3 /* GC_OLD_MARKED */)) {
        if (__unlikely(jl_astaggedvalue(owner)->bits.in_image == 1 /* GC_IN_IMAGE_NOT_REMSET */)) {
            // GC_MARKED optimizations are invalid for generations >= 2
            jl_gc_queue_root(owner);
            return;
        }
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
