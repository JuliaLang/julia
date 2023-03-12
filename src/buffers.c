// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#define JL_BUFFER_IMPL_NUL 1
#define MAXINTVAL (((size_t)-1)>>1)

JL_DLLEXPORT jl_buffer_t *jl_buffer_copy(jl_buffer_t *a)
{
    jl_buffer_t *c = jl_new_buffer(jl_typeof(a), jl_buffer_len((jl_value_t*)a));
    memcpy((void**)jl_buffer_data(c), (void**)jl_buffer_data(a), jl_buffer_nbytes((jl_value_t*)a));
    return c;
}

JL_DLLEXPORT jl_buffer_t *jl_new_buffer(jl_value_t *btype, size_t len)
{
    jl_value_t *eltype = jl_tparam0(btype);
    jl_element_type_layout_t flags = jl_element_type_layout(eltype);
    jl_buffer_t *b;
    jl_task_t *ct = jl_current_task;
    size_t tot = len * flags.elsize;
    if (flags.union_max == 0) {
        if (flags.elsize == 1 && flags.union_max > 1) {
            // extra byte for all julia allocated byte arrays
            tot++;
        }
        if (flags.union_max > 1) {
            // an extra byte for each isbits union array element, stored after len * elsize
            tot += len;
        }
    }
    // align data area
    int tsz = sizeof(jl_buffer_t);
    void *data;
    if (tot <= ARRAY_INLINE_NBYTES) {
        // align data area
        if (tot >= ARRAY_CACHE_ALIGN_THRESHOLD)
            tsz = LLT_ALIGN(tsz, JL_CACHE_BYTE_ALIGNMENT);
        else if (flags.union_max != 0 && flags.elsize >= 4)
            tsz = LLT_ALIGN(tsz, JL_SMALL_BYTE_ALIGNMENT);
        size_t doffs = tsz;
        tsz += tot;
        b = (jl_buffer_t*)jl_gc_alloc(ct->ptls, tot, btype);
        b->length = len;
        data = jl_buffer_data(b) + doffs;
    }
    else {
        data = jl_gc_managed_malloc(tot);
        // Allocate the Array **after** allocating the data
        // to make sure the array is still young
        b = (jl_buffer_t*)jl_gc_alloc(ct->ptls, tsz + sizeof(void*), btype);
        b->length = len;
        *(void**)jl_buffer_data(b) = &data;
        // No allocation or safepoint allowed after this
        jl_gc_track_malloced_buffer(ct->ptls, b);
    }
    // zero initialize data that may otherwise error on load
    if (flags.union_max < 2 || // union or pointer type
        ((jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->layout->npointers > 0)) ||
        (jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->zeroinit))
        memset(data, 0, tot);

    if (JL_BUFFER_IMPL_NUL && flags.elsize == 1)
         ((char*)data)[tot - 1] = '\0';

    return b;
}

JL_DLLEXPORT jl_value_t *jl_bufref(jl_buffer_t *b, size_t i)
{
    size_t len = jl_buffer_len(b);
    assert(i < len);
    size_t elsz = 0, al = 0;
    jl_value_t *ety = jl_tparam0(jl_typeof(b));
    int union_max = jl_islayout_inline(ety, &elsz, &al);
    int isboxed = (union_max == 0);
    char *data = (char*)(jl_buffer_data(b));
    if (!isboxed && jl_is_datatype(ety) && jl_datatype_size(ety) == 0) {
        return ((jl_datatype_t*)ety)->instance;
    }
    else if (!isboxed) {
        if (jl_is_uniontype(ety)) {
            // isbits union selector bytes are always stored directly after the last array element
            uint8_t sel = ((uint8_t*)data + (len * elsz))[i];
            ety = jl_nth_union_component(ety, sel);
            if (jl_is_datatype_singleton((jl_datatype_t*)ety))
                return ((jl_datatype_t*)ety)->instance;
        }
        jl_value_t *r = undefref_check((jl_datatype_t*)ety, jl_new_bits(ety, data +(i * elsz)));
        if (__unlikely(r == NULL))
            jl_throw(jl_undefref_exception);
        return r;

    }
    else {
        jl_value_t *elt = jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)(void**)data) + i);
        if (elt == NULL)
            jl_throw(jl_undefref_exception);
        return elt;
    } }

JL_DLLEXPORT void jl_bufset(jl_buffer_t *b JL_ROOTING_ARGUMENT, jl_value_t *rhs JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED, size_t i)
{
    size_t len = jl_buffer_len(b);
    assert(i < len);
    size_t elsz = 0, al = 0;
    jl_value_t *ety = jl_tparam0(jl_typeof(b));
    int union_max = jl_islayout_inline(ety, &elsz, &al);
    int isboxed = (union_max == 0);
    char *data = (char*)(jl_buffer_data(b));
    if (isboxed) {
        jl_atomic_store_release(((_Atomic(jl_value_t*)*)(void**)data) + i, rhs);
        jl_gc_wb(b, rhs);
    }
    else {
        if (jl_is_uniontype(ety)) {
            // set type tag
            uint8_t *psel = &((uint8_t*)data + (len * elsz))[i];
            unsigned nth = 0;
            if (!jl_find_union_component(ety, jl_typeof(rhs), &nth))
                assert(0 && "invalid bufset to isbits union");
            *psel = nth;
            if (jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(rhs)))
                return;
        }
        switch (elsz) {
        case  0: break;
        case  1: *(uint8_t*)(data + i)  = *(uint8_t*)rhs;  break;
        case  2: *(uint16_t*)(data + (i * elsz)) = *(uint16_t*)rhs; break;
        case  4: *(uint32_t*)(data + (i * elsz)) = *(uint32_t*)rhs; break;
        case  8: *(uint64_t*)(data + (i * elsz)) = *(uint64_t*)rhs; break;
        case 16:
            memcpy(jl_assume_aligned((data + (i * elsz)), 16), jl_assume_aligned(rhs, 16), 16);
            break;
        default: memcpy((data + (i * elsz)), rhs, elsz);
        }
    }
}

JL_DLLEXPORT int jl_buffer_isassigned(jl_buffer_t *b, size_t i)
{
    size_t elsz = 0, al = 0;
    jl_value_t *eltype = jl_tparam0(jl_typeof(b));
    int isunboxed = jl_islayout_inline(eltype, &elsz, &al);
    int hasptr = isunboxed && (jl_is_datatype(eltype) && ((jl_datatype_t*)eltype)->layout->npointers > 0);
    if (!isunboxed) {
        return jl_atomic_load_relaxed(((_Atomic(jl_value_t*)*)jl_array_data(b)) + i) != NULL;
    }
    else if (hasptr) {
        jl_datatype_t *elty = (jl_datatype_t*)eltype;
         assert(elty->layout->first_ptr >= 0);
         jl_value_t **elem = (jl_value_t**)(jl_buffer_data(b) + i * elsz);
         return elem[elty->layout->first_ptr] != NULL;
    }
    return 1;
}