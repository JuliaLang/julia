// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  buffer constructors and primitives
*/
#include <stdlib.h>
#include <string.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define JL_BUFFER_ALIGN(jl_value, nbytes) LLT_ALIGN(jl_value, nbytes)

static inline void bufferassign_safe(int hasptr, jl_value_t *parent, char *dst, const jl_value_t *src, size_t nb) JL_NOTSAFEPOINT
{
    // buffer can assume more alignment than a field would normally have
    assert(nb >= jl_datatype_size(jl_typeof(src))); // nb might move some undefined bits, but we should be okay with that
    if (hasptr) {
        size_t nptr = nb / sizeof(void*);
        memmove_refs((void**)dst, (void* const*)src, nptr);
        jl_gc_multi_wb(parent, src);
    }
    else {
        switch (nb) {
        case  0: break;
        case  1: *(uint8_t*)dst  = *(uint8_t*)src;  break;
        case  2: *(uint16_t*)dst = *(uint16_t*)src; break;
        case  4: *(uint32_t*)dst = *(uint32_t*)src; break;
        case  8: *(uint64_t*)dst = *(uint64_t*)src; break;
        case 16:
            memcpy(jl_assume_aligned(dst, 16), jl_assume_aligned(src, 16), 16);
            break;
        default: memcpy(dst, src, nb);
        }
    }
}

JL_DLLEXPORT char *jl_buffer_typetagdata(jl_buffer_t *a) JL_NOTSAFEPOINT
{
    assert(jl_buffer_isbitsunion(a));
    return ((char*)jl_buffer_data(a)) + jl_buffer_len(a) * jl_buffer_elsize(a);
}

JL_DLLEXPORT int jl_isinlinealloc(jl_value_t *type) JL_NOTSAFEPOINT
{
    size_t fsz = 0, al = 0;
    return jl_islayout_inline(type, &fsz, &al);
}

STATIC_INLINE jl_value_t *jl_buffer_owner(jl_buffer_t *a JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    if (a->flags.how == 3) {
        //TODO
        assert(0);
        // a = (jl_buffer_t*)jl_buffer_data_owner(a);
        // assert(jl_is_string(a) || a->flags.how != 3);
    }
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_buffer_t *jl_new_buffer(jl_value_t *btype, size_t length)
{
    jl_task_t *ct = jl_current_task;
    size_t tot, nel;
    void *data;
    jl_buffer_t *a;
    jl_value_t *eltype = jl_tparam0(btype);
    size_t elsz = 0, al = 0;
    if (!jl_is_kind(jl_typeof(eltype)))
        jl_type_error_rt("Buffer", "element type", (jl_value_t*)jl_type_type, eltype);
    int isunboxed = jl_islayout_inline(eltype, &elsz, &al);
    int isunion = jl_is_uniontype(eltype);
    int hasptr = isunboxed && (jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->layout->npointers > 0);
    if (!isunboxed) {
        elsz = sizeof(void*);
        al = elsz;
    }
    else {
        elsz = LLT_ALIGN(elsz, al);
    }
    assert(isunboxed || elsz == sizeof(void*));
    assert(btype == NULL || isunion == jl_is_uniontype(jl_tparam0(btype)));
    nel = length;
    tot = elsz * nel;
    if (isunboxed) {
        if (elsz == 1 && !isunion) {
            // extra byte for all julia allocated byte arrays
            tot++;
        }
        if (isunion) {
            // an extra byte for each isbits union array element, stored after a->maxsize
            tot += nel;
        }
    }

    int tsz = sizeof(jl_buffer_t);
    if (tot <= ARRAY_INLINE_NBYTES) {
        if (tot >= ARRAY_CACHE_ALIGN_THRESHOLD)
            tsz = JL_BUFFER_ALIGN(tsz, JL_CACHE_BYTE_ALIGNMENT);
        else if (isunboxed && elsz >= 4)
            tsz = JL_BUFFER_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT);
        size_t doffs = tsz;
        tsz += tot;
        // jl_buffer_t is large enough that objects will always be aligned 16
        a = (jl_buffer_t*)jl_gc_alloc(ct->ptls, tsz, btype);
        assert(((size_t)a & 15) == 0);
        // No allocation or safepoint allowed after this
        a->flags.how = 0;
        data = (char*)a + doffs;
    } else {
        data = jl_gc_managed_malloc(tot);
        // Allocate the Array **after** allocating the data
        // to make sure the array is still young
        a = (jl_buffer_t*)jl_gc_alloc(ct->ptls, tsz, btype);
        // No allocation or safepoint allowed after this
        a->flags.how = 2;
        jl_gc_track_malloced_buffer(ct->ptls, a);
    }
    a->flags.pooled = tsz <= GC_MAX_SZCLASS;
    memset(data, 0, tot);
    a->data = data;
    a->length = length;
    a->flags.ptrarray = !isunboxed;
    a->flags.hasptr = hasptr;
    a->flags.isshared = 0;
    a->flags.isaligned = 1;
    return a;
}

// own_buffer != 0 iff GC should call free() on this pointer eventually
JL_DLLEXPORT jl_buffer_t *jl_ptr_to_buffer(jl_value_t *btype, void *data, size_t nel, int own_buffer)
{
    jl_task_t *ct = jl_current_task;
    jl_buffer_t *b;
    jl_value_t *eltype = jl_tparam0(btype);

    int isunboxed = jl_stored_inline(eltype);
    if (isunboxed && jl_is_uniontype(eltype))
        jl_exceptionf(jl_argumenterror_type,
                      "unsafe_wrap: unspecified layout for union element type");
    size_t elsz;
    unsigned align;
    if (isunboxed) {
        elsz = jl_datatype_size(eltype);
        align = jl_datatype_align(eltype);
    }
    else {
        align = elsz = sizeof(void*);
    }
    if (((uintptr_t)data) & ((align > JL_HEAP_ALIGNMENT ? JL_HEAP_ALIGNMENT : align) - 1))
        jl_exceptionf(jl_argumenterror_type,
                      "unsafe_wrap: pointer %p is not properly aligned to %u bytes", data, align);

    int tsz = LLT_ALIGN(sizeof(jl_buffer_t), JL_CACHE_BYTE_ALIGNMENT);
    b = (jl_buffer_t *)jl_gc_alloc(ct->ptls, tsz, btype);
    // No allocation or safepoint allowed after this
    b->data = data;
    b->length = nel;
    b->flags.pooled = tsz <= GC_MAX_SZCLASS;
    b->flags.ptrarray = !isunboxed;
    b->flags.hasptr = isunboxed && (jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->layout->npointers > 0);
    b->flags.isshared = 1;
    b->flags.isaligned = 0; // TODO: allow passing memalign'd buffers
    if (own_buffer) {
        b->flags.how = 2;
        jl_gc_track_malloced_buffer(ct->ptls, b);
        jl_gc_count_allocd(nel * elsz + (elsz == 1 ? 1 : 0));
    } else {
        b->flags.how = 0;
    }

    return b;
}

JL_DLLEXPORT jl_value_t *jl_ptrbufferref(jl_buffer_t *a JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    assert(i < jl_buffer_len(a));
    assert(a->flags.ptrarray);
    jl_value_t *elt = jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)a->data) + i);
    if (elt == NULL)
        jl_throw(jl_undefref_exception);
    return elt;
}

JL_DLLEXPORT jl_value_t *jl_bufferref(jl_buffer_t *a, size_t i)
{
    if (a->flags.ptrarray)
        return jl_ptrbufferref(a, i);
    assert(i < jl_buffer_len(a));
    jl_value_t *eltype = (jl_value_t*)jl_tparam0(jl_typeof(a));
    if (jl_is_uniontype(eltype)) {
        // isbits union selector bytes are always stored directly after the last array element
        uint8_t sel = jl_buffer_typetagdata(a)[i];
        eltype = jl_nth_union_component(eltype, sel);
        if (jl_is_datatype_singleton((jl_datatype_t*)eltype))
            return ((jl_datatype_t*)eltype)->instance;
    }
    jl_value_t *r = undefref_check((jl_datatype_t*)eltype, jl_new_bits(eltype, &((char*)a->data)[i * jl_buffer_elsize(a)]));
    if (__unlikely(r == NULL))
        jl_throw(jl_undefref_exception);
    return r;
}

JL_DLLEXPORT int jl_buffer_isassigned(jl_buffer_t *a, size_t i)
{
    if (a->flags.ptrarray) {
        return jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)jl_buffer_data(a)) + i) != NULL;
    }
    else if (a->flags.hasptr) {
         jl_datatype_t *eltype = (jl_datatype_t*)jl_tparam0(jl_typeof(a));
         assert(eltype->layout->first_ptr >= 0);
         jl_value_t **elem = (jl_value_t**)((char*)a->data + i * jl_buffer_elsize(a));
         return elem[eltype->layout->first_ptr] != NULL;
    }
    return 1;
}

JL_DLLEXPORT void jl_bufferset(jl_buffer_t *a JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i)
{
    assert(i < jl_buffer_len(a));
    jl_value_t *eltype = jl_tparam0(jl_typeof(a));
    if (eltype != (jl_value_t *)jl_any_type)
    {
        JL_GC_PUSH1(&rhs);
        if (!jl_isa(rhs, eltype))
            jl_type_error("bufferset", eltype, rhs);
        JL_GC_POP();
    }
    if (!a->flags.ptrarray) {
        int hasptr;
        if (jl_is_uniontype(eltype)) {
            uint8_t *psel = &((uint8_t*)jl_buffer_typetagdata(a))[i];
            unsigned nth = 0;
            if (!jl_find_union_component(eltype, jl_typeof(rhs), &nth))
                assert(0 && "invalid bufferset to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(rhs)))
                return;
            hasptr = 0;
        }
        else {
            hasptr = a->flags.hasptr;
        }
        bufferassign_safe(hasptr, jl_buffer_owner(a), &((char*)a->data)[i * jl_buffer_elsize(a)], rhs, jl_buffer_elsize(a));
    }
    else {
        jl_atomic_store_release(((_Atomic(jl_value_t*)*)a->data) + i, rhs);
        jl_gc_wb(jl_buffer_owner(a), rhs);
    }
}

JL_DLLEXPORT void jl_bufferunset(jl_buffer_t *b, size_t i)
{
    if (i >= jl_buffer_len(b))
        jl_bounds_error_int((jl_value_t *)b, i + 1);
    if (b->flags.ptrarray)
        jl_atomic_store_release(((_Atomic(jl_value_t*)*)b->data) + i, NULL);
    else if (b->flags.hasptr) {
        size_t elsize = jl_buffer_elsize(b);
        jl_assume(elsize >= sizeof(void*) && elsize % sizeof(void*) == 0);
        memset((char*)b->data + elsize * i, 0, elsize);
    }
}
