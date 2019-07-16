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

char *jl_buffer_typetagdata(jl_buffer_t *b) JL_NOTSAFEPOINT
{
    // assert(jl_array_isbitsunion(a));
    return ((char*)jl_buffer_data(b)) + jl_buffer_len(b) * jl_buffer_elsize(b);
}

JL_DLLEXPORT int jl_isinlinealloc(jl_value_t *type) JL_NOTSAFEPOINT
{
    size_t fsz = 0, al = 0;
    return jl_islayout_inline(type, &fsz, &al);
}

JL_DLLEXPORT jl_buffer_t *jl_new_buffer(jl_value_t *btype, size_t length)
{
    //TODO: assert btype is a Buffer{T} datatype, length is reasonable
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *eltype = jl_tparam0(btype);
    size_t tot = 0, elsize = 0, al = 0;
    int isinlinealloc = jl_islayout_inline(eltype, &elsize, &al);
    int isunion = jl_is_uniontype(eltype);
    void *data;
    jl_buffer_t *b;

    if (isinlinealloc) {
        size_t tot = (size_t)elsize * (size_t)length;
        if (isunion)
            tot += length;
        else if (elsize == 1)
            tot++;
    } else {
        tot = (size_t)sizeof(void *) * (size_t)length;
    }

    int tsz = LLT_ALIGN(sizeof(jl_buffer_t), JL_CACHE_BYTE_ALIGNMENT);
    if (tot <= ARRAY_INLINE_NBYTES) {
        if (isinlinealloc && elsize >= 4)
            tsz = LLT_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT); // align data area
        size_t doffs = tsz;
        tsz += tot;
        tsz = LLT_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT); // align whole object
        b = (jl_buffer_t *)jl_gc_alloc(ptls, tsz, btype);
        // No allocation or safepoint allowed after this
        b->flags.how = 0;
        data = (char *)b + doffs;
        if ((tot > 0 && !isinlinealloc) || isunion)
            memset(data, 0, tot);
    } else {
        tsz = LLT_ALIGN(tsz, JL_CACHE_BYTE_ALIGNMENT); // align whole object
        data = jl_gc_managed_malloc(tot);
        // Allocate the Array **after** allocating the data
        // to make sure the array is still young
        b = (jl_buffer_t *)jl_gc_alloc(ptls, tsz, btype);
        // No allocation or safepoint allowed after this
        b->flags.how = 2;
        jl_gc_track_malloced_buffer(ptls, b);
        if (!isinlinealloc || isunion)
            // need to zero out isbits union buffer selector bytes to ensure a valid type index
            memset(data, 0, tot);
    }

    b->data = data;
    b->length = length;
    b->flags.pooled = tsz <= GC_MAX_SZCLASS;
    b->flags.isshared = 0;
    b->flags.isaligned = 1;
    b->flags.isinlinealloc = isinlinealloc;
    return b;
}

// own_buffer != 0 iff GC should call free() on this pointer eventually
JL_DLLEXPORT jl_buffer_t *jl_ptr_to_buffer(jl_value_t *btype, void *data, size_t nel, int own_buffer)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_buffer_t *b;
    jl_value_t *eltype = jl_tparam0(btype);
    size_t elsize = 0, al = 0;
    int isinlinealloc = jl_islayout_inline(eltype, &elsize, &al);
    int isunion = jl_is_uniontype(eltype);

    if (isinlinealloc && isunion)
        jl_exceptionf(jl_argumenterror_type,
                      "unsafe_wrap: unspecified layout for union element type");
    if (((uintptr_t)data) & (al - 1))
        jl_exceptionf(jl_argumenterror_type,
                      "unsafe_wrap: pointer %p is not properly aligned to %u bytes", data, al);

    int tsz = LLT_ALIGN(sizeof(jl_buffer_t), JL_CACHE_BYTE_ALIGNMENT);
    b = (jl_buffer_t *)jl_gc_alloc(ptls, tsz, btype);
    // No allocation or safepoint allowed after this
    b->data = data;
    b->length = nel;
    b->flags.pooled = tsz <= GC_MAX_SZCLASS;
    b->flags.isshared = 1;
    b->flags.isaligned = 0; // TODO: allow passing memalign'd buffers
    b->flags.isinlinealloc = isinlinealloc;
    if (own_buffer) {
        b->flags.how = 2;
        jl_gc_track_malloced_buffer(ptls, b);
        jl_gc_count_allocd(nel * elsize + (elsize == 1 ? 1 : 0));
    } else {
        b->flags.how = 0;
    }

    return b;
}

JL_DLLEXPORT jl_value_t *jl_bufferref(jl_buffer_t *b, size_t i)
{
    assert(i < b->length);
    if (b->flags.isinlinealloc) {
        jl_value_t *eltype = (jl_value_t *)jl_tparam0(jl_typeof(b));
        if (jl_is_uniontype(eltype)) {
            // isbits union selector bytes are always stored directly after the last buffer element
            uint8_t sel = jl_buffer_typetagdata(b)[i];
            eltype = jl_nth_union_component(eltype, sel);
            if (jl_is_datatype_singleton((jl_datatype_t *)eltype))
                return ((jl_datatype_t *)eltype)->instance;
        }
        return jl_new_bits(eltype, &((char *)b->data)[i * jl_datatype_size(eltype)]);
    } else {
        jl_value_t *elt = ((jl_value_t **)b->data)[i];
        if (elt == NULL)
            jl_throw(jl_undefref_exception);
        return elt;
    }
}

JL_DLLEXPORT int jl_buffer_isassigned(jl_buffer_t *b, size_t i)
{
    if (b->flags.isinlinealloc)
        return ((jl_value_t **)jl_buffer_data(b))[i] != NULL;
    return 1;
}

JL_DLLEXPORT void jl_bufferset(jl_buffer_t *b JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i)
{
    assert(i < jl_array_len(b));
    jl_value_t *eltype = jl_tparam0(jl_typeof(b));
    if (eltype != (jl_value_t *)jl_any_type)
    {
        JL_GC_PUSH1(&rhs);
        if (!jl_isa(rhs, eltype))
            jl_type_error("bufferset", eltype, rhs);
        JL_GC_POP();
    }
    if (b->flags.isinlinealloc)
    {
        if (jl_is_uniontype(eltype))
        {
            uint8_t *psel = &((uint8_t *)jl_buffer_typetagdata(b))[i];
            unsigned nth = 0;
            if (!jl_find_union_component(eltype, jl_typeof(rhs), &nth))
                assert(0 && "invalid bufferset to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t *)jl_typeof(rhs)))
                return;
        }
        jl_assign_bits(&((char *)b->data)[i * jl_buffer_elsize(b)], rhs);
    }
    else
    {
        ((jl_value_t **)b->data)[i] = rhs;
        //TODO: do we need to store owner pointer for buffer?
        jl_gc_wb(b, rhs);
    }
}

JL_DLLEXPORT void jl_bufferunset(jl_buffer_t *b, size_t i)
{
    if (i >= jl_buffer_len(b))
        jl_bounds_error_int((jl_value_t *)b, i + 1);
    if (!b->flags.isinlinealloc)
        ((jl_value_t **)b->data)[i] = NULL;
}